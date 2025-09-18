#' Calculate two-stage difference-in-differences following Gardner (2021)
#'
#' @import fixest
#'
#' @param data The dataframe containing all the variables
#' @param yname Outcome variable
#' @param first_stage Fixed effects and other covariates you want to residualize
#'   with in first stage.
#'   Formula following \code{\link[fixest:feols]{fixest::feols}}.
#'   Fixed effects specified after "`|`".
#' @param second_stage Second stage, these should be the treatment indicator(s)
#'   (e.g. treatment variable or event-study leads/lags).
#'   Formula following \code{\link[fixest:feols]{fixest::feols}}.
#'   Use `i()` for factor variables, see \code{\link[fixest:i]{fixest::i}}.
#' @param treatment A variable that = 1 if treated, = 0 otherwise. The first
#'   stage will be estimated for `treatment == 0`. The second stage will be
#'   estimated for the *full sample*.
#' @param cluster_var What variable to cluster standard errors. This can be IDs
#'   or a higher aggregate level (state for example)
#' @param weights Optional. Variable name for regression weights.
#' @param bootstrap Optional. Should standard errors be calculated using bootstrap?
#'   Default is `FALSE`.
#' @param n_bootstraps Optional. How many bootstraps to run.
#'   Default is `250`.
#' @param return_bootstrap Optional. Logical. Will return each bootstrap second-stage
#'   estimate to allow for manual use, e.g. percentile standard errors and empirical
#'   confidence intervals.
#' @param verbose Optional. Logical. Should information about the two-stage
#'   procedure be printed back to the user?
#'   Default is `TRUE`.
#'
#' @return `fixest` object with adjusted standard errors
#'   (either by formula or by bootstrap). All the methods from `fixest` package
#'   will work, including \code{\link[fixest:esttable]{fixest::esttable}} and
#'   \code{\link[fixest:coefplot]{fixest::coefplot}}
#'
#' @section Examples:
#'
#' Load example dataset which has two treatment groups and homogeneous treatment effects
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' # Load Example Dataset
#' data("df_hom")
#' ```
#'
#' ### Static TWFE
#'
#' You can run a static TWFE fixed effect model for a simple treatment indicator
#' ```{r, comment = "#>", collapse = TRUE}
#' static <- did2s(df_hom,
#'     yname = "dep_var", treatment = "treat", cluster_var = "state",
#'     first_stage = ~ 0 | unit + year,
#'     second_stage = ~ i(treat, ref=FALSE))
#'
#' fixest::esttable(static)
#' ```
#'
#' ### Event Study
#'
#' Or you can use relative-treatment indicators to estimate an event study estimate
#' ```{r, comment = "#>", collapse = TRUE}
#' es <- did2s(df_hom,
#'     yname = "dep_var", treatment = "treat", cluster_var = "state",
#'     first_stage = ~ 0 | unit + year,
#'     second_stage = ~ i(rel_year, ref=c(-1, Inf)))
#'
#' fixest::esttable(es)
#' ```
#'
#' ```{r, eval = F}
#' # plot rel_year coefficients and standard errors
#' fixest::coefplot(es, keep = "rel_year::(.*)")
#' ```
#'
#' ### Example from Cheng and Hoekstra (2013)
#'
#' Here's an example using data from Cheng and Hoekstra (2013)
#' ```{r, comment = "#>", collapse = TRUE}
#' # Castle Data
#' castle <- haven::read_dta("https://github.com/scunning1975/mixtape/raw/master/castle.dta")
#'
#' did2s(
#' 	data = castle,
#' 	yname = "l_homicide",
#' 	first_stage = ~ 0 | sid + year,
#' 	second_stage = ~ i(post, ref=0),
#' 	treatment = "post",
#' 	cluster_var = "state", weights = "popwt"
#' )
#' ```
#'
#' @export
did2s <- function(
  data,
  yname,
  first_stage,
  second_stage,
  treatment,
  cluster_var,
  weights = NULL,
  bootstrap = FALSE,
  n_bootstraps = 250,
  return_bootstrap = FALSE,
  verbose = FALSE
) {
  # Check Parameters ---------------------------------------------------------
  dreamerr::check_arg(data, "data.frame")
  dreamerr::check_value(data[[treatment]], "logical (loose) vector")

  if (verbose) {
    did2s_summary_message(
      yname,
      first_stage,
      second_stage,
      treatment,
      cluster_var,
      bootstrap
    )
  }

  # Point Estimates ----------------------------------------------------------
  est <- did2s_estimate(
    data = data,
    yname = yname,
    first_stage = first_stage,
    second_stage = second_stage,
    treatment = treatment,
    weights = weights,
    bootstrap = bootstrap
  )

  # Analytic Standard Errors -------------------------------------------------
  if (!bootstrap) {
    # Subset data to the observations used in the second stage
    # obsRemoved have - in front of rows, so they are deleted
    removed_rows <- est$second_stage$obs_selection$obsRemoved
    if (!is.null(removed_rows)) {
      data <- data[removed_rows, ]
    }

    # Extract weights
    if (is.null(weights)) {
      weights_vector <- rep.int(1L, nrow(data))
    } else {
      weights_vector <- sqrt(data[[weights]])
    }

    # Extract first stage
    first_u <- est$first_u
    if (!is.null(removed_rows)) {
      first_u <- first_u[removed_rows]
    }

    # x1 is matrix used to predict Y(0)
    x1 <- fixest::sparse_model_matrix(
      est$first_stage,
      data = data,
      type = c("rhs", "fixef")
    )

    # Extract second stage
    second_u <- stats::residuals(est$second_stage)
    x2 <- fixest::sparse_model_matrix(
      est$second_stage,
      type = c("rhs", "fixef")
    )

    # multiply by weights
    first_u <- weights_vector * first_u
    x1 <- weights_vector * x1
    second_u <- weights_vector * second_u
    x2 <- weights_vector * x2

    # x10 is matrix used to estimate first stage (zero out rows with D_it = 1)
    x10 <- copy(x1)
    # treated rows. Note dgcMatrix is 0-index !!
    treated_rows <- which(data[[treatment]] == 1L) - 1
    idx <- x10@i %in% treated_rows
    x10@x[idx] <- 0

    # The influence consists of two terms:
    # (X_2' X_2)^{-1} X_2' (y - \hat{y}(0))
    #
    # First is the second stage standard OLS IF:
    # $$(X_2' X_2)^{-1} X_{2i}' \hat{v}_i$$
    #
    # Second is the effect of the first stage estimate of $\hat{y}(0)$:
    # $$(X_2' X_2)^{-1} X_2' X_1 (X_{10}' X_{10})^{-1) X_{10, i} \hat{u}_i$$
    #
    # The IF is the sum of the two terms
    #
    x2tx2_inv <- (est$second_stage$cov.iid / est$second_stage$sigma2)
    IF_ss <- x2tx2_inv %*% Matrix::t(x2 * second_u)

    # Use robust QR-based solve instead of direct Matrix::solve()
    gamma_hat <- robust_solve_XtX(x10, Matrix::crossprod(x1, x2))
    IF_fs <- x2tx2_inv %*% Matrix::t(gamma_hat) %*% Matrix::t((x10 * first_u))

    IF <- IF_fs - IF_ss

    cl <- data[[cluster_var]]
    cov <- Reduce(
      "+",
      lapply(
        split(1:length(cl), cl),
        function(cl_idx) {
          Matrix::tcrossprod(Matrix::rowSums(IF[, cl_idx, drop = FALSE]))
        }
      )
    )
    cov <- as.matrix(cov)
    rownames(cov) <- colnames(cov) <- names(est$second_stage$coefficients)
  }

  # Bootstrap Standard Errors ------------------------------------------------
  if (bootstrap) {
    if (verbose) {
      message(sprintf(
        "Starting %s bootstraps at cluster level: %s\n",
        n_bootstraps,
        cluster_var
      ))
    }

    # Unique values of cluster variable
    cl <- unique(data[[cluster_var]])

    stat <- function(x, i) {
      # select the observations to subset based on the cluster var
      block_obs <- unlist(lapply(
        i,
        function(n) which(x[n] == data[[cluster_var]])
      ))
      # run regression for given replicate, return estimated coefficients
      stats::coefficients(
        did2s_estimate(
          data = data[block_obs, ],
          yname = yname,
          first_stage = first_stage,
          second_stage = second_stage,
          treatment = treatment,
          weights = weights,
          bootstrap = TRUE
        )$second_stage
      )
    }

    boot <- boot::boot(cl, stat, n_bootstraps)

    # Get estimates and fix names
    estimates <- boot$t
    colnames(estimates) <- names(stats::coef(est$second_stage))

    # Bootstrap Var-Cov Matrix
    cov <- stats::cov(estimates)

    if (return_bootstrap) {
      return(estimates)
    }
  }

  # summary creates fixest object with correct standard errors and vcov
  vcov_list = list()
  vcov_list[[sprintf("Corrected Clustered (%s)", cluster_var)]] = cov
  est <- base::suppressWarnings(summary(est$second_stage, vcov = vcov_list))
  return(est)
}


# Point estimate for did2s
#' Robust solve for X'X beta = X'Y using QR decomposition
#'
#' This function computes the least squares solution beta = (X'X)^(-1) X'Y
#' in a numerically stable way using QR decomposition, handling rank-deficient
#' matrices gracefully.
#'
#' @param X Design matrix (sparse or dense)
#' @param Y Response matrix/vector (can be X'Y if already computed)
#' @return The least squares solution beta (may contain 0 for rank-deficient columns)
robust_solve_XtX <- function(X, Y) {
  # Handle both vector and matrix Y
  if (is.vector(Y)) {
    # Y is a vector, need to compute X'Y
    XtY <- Matrix::crossprod(X, Y)
  } else {
    # Y is a matrix, check dimensions
    if (nrow(Y) == nrow(X)) {
      # Y is the raw response matrix, need to compute X'Y
      XtY <- Matrix::crossprod(X, Y)
    } else if (nrow(Y) == ncol(X)) {
      # Y is already X'Y (cross product form)
      XtY <- Y
    } else {
      stop("Incompatible dimensions between X and Y")
    }
  }

  # Now solve the system X'X beta = X'Y using robust methods
  XtX <- Matrix::crossprod(X)

  # Check if the matrix is singular using condition number approach
  beta_hat <- tryCatch(
    {
      # Try direct solve first for speed
      Matrix::solve(XtX, XtY)
    },
    error = function(e) {
      # If direct solve fails, fall back to SVD-based approach
      # Use the fact that the least squares solution is
      # beta_hat = (X'X)+ * X'Y where (X'X)+ is the Moore-Penrose pseudoinverse

      # For now, use a simple approach: set small singular values to zero
      SVD <- svd(as.matrix(XtX))
      tol <- max(dim(XtX)) * .Machine$double.eps * max(SVD$d)
      positive_indices <- SVD$d > tol

      # Matrix is rank deficient
      d_inv <- ifelse(positive_indices, 1 / SVD$d, 0)
      XtX_pinv <- SVD$v %*% diag(d_inv) %*% t(SVD$u)
      XtX_pinv %*% as.matrix(XtY)
    }
  )

  return(beta_hat)
}


# Point estimate for did2s
did2s_estimate <- function(
  data,
  yname,
  first_stage,
  second_stage,
  treatment,
  weights = NULL,
  bootstrap = FALSE,
  cluster_var = NULL # Just so you can switch `did2s` to `did2s_estimate` for quick dev estimation
) {
  ## We'll use fixest's formula expansion macros to swap out first and second
  ## stages (see: ?fixest::xpd)
  fixest::setFixest_fml(
    ..first_stage = first_stage,
    ..second_stage = second_stage
  )

  # First stage among untreated
  untreat <- data[data[[treatment]] == 0, ]
  if (is.null(weights)) {
    weights_vector <- NULL
  } else {
    weights_vector <- untreat[[weights]]
  }

  first_stage <- fixest::feols(
    fixest::xpd(~ 0 + ..first_stage, lhs = yname),
    data = untreat,
    weights = weights_vector,
    # combine.quick = FALSE, # (deprecated argument)
    fixef.keep_names = TRUE, # allows var1^var2 in FEs
    warn = FALSE,
    notes = FALSE,
  )

  # Residualize outcome variable but keep same yname
  first_u <- data[[yname]] - stats::predict(first_stage, newdata = data)
  data[[yname]] <- first_u

  # Zero out residual rows with D_it = 1 (for analytical SEs later on)
  if (!bootstrap) {
    first_u[data[[treatment]] == 1] <- 0
  }

  # Second stage
  if (!is.null(weights)) {
    weights_vector <- data[[weights]]
  }

  second_stage <- fixest::feols(
    fixest::xpd(~ 0 + ..second_stage, lhs = yname),
    data = data,
    weights = weights_vector,
    warn = FALSE,
    notes = FALSE
  )

  ret <- list(
    first_stage = first_stage,
    second_stage = second_stage
  )

  if (!bootstrap) {
    ret <- list(
      first_stage = first_stage,
      second_stage = second_stage,
      first_u = first_u
    )
  } else {
    ret <- list(second_stage = second_stage)
  }

  return(ret)
}

did2s_summary_message <- function(
  yname,
  first_stage,
  second_stage,
  treatment,
  cluster_var,
  bootstrap
) {
  cluster_msg <- if (bootstrap) {
    sprintf(
      "- Standard errors will be block bootstrapped with cluster `%s`\n",
      cluster_var
    )
  } else {
    sprintf(
      "- Standard errors will be clustered by `%s`\n",
      cluster_var
    )
  }

  msg <- paste(
    "Running Two-stage Difference-in-Differences\n",
    sprintf("- first stage formula %s\n", rlang::f_label(first_stage)),
    sprintf("- second stage formula %s\n", rlang::f_label(second_stage)),
    sprintf(
      "- The indicator variable that denotes when treatment is on is `%s`\n",
      treatment
    ),
    cluster_msg,
    collapse = "\n"
  )

  message(msg)
}
