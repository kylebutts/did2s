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
#' @param vcov_method Method used to solve the first-stage correction in the
#'   analytic variance calculation. `"sparse"` forms and solves the sparse
#'   cross-product matrix and is typically fastest for conventional unit by
#'   time panels. `"matrix_free"` applies the cross-product as a linear
#'   operator and solves it with preconditioned conjugate gradients, avoiding
#'   factorization fill-in that can dominate with high-cardinality crossed
#'   fixed effects.
#' @param vcov_tol Relative residual tolerance for `vcov_method =
#'   "matrix_free"`.
#' @param vcov_maxiter Maximum number of conjugate-gradient iterations for each
#'   second-stage coefficient when `vcov_method = "matrix_free"`.
#' @param verbose Optional. Logical. Should information about the two-stage
#'   procedure be printed back to the user?
#'   Default is `TRUE`.
#'
#' @return `fixest` object with adjusted standard errors
#'   (either by formula or by bootstrap). All the methods from `fixest` package
#'   will work, including \code{\link[fixest:esttable]{fixest::esttable}} and
#'   \code{\link[fixest:coefplot]{fixest::coefplot}}
#'
#' @details Matrix-free PCG will perform better when there are a lot of
#'   complicated fixed effects.
#'
#'   First-stage fixed effects nested within the clustering variable are
#'   detected automatically. Their cluster-level scores are zero by the
#'   first-stage normal equations and are omitted from the VCOV calculation. If
#'   every first-stage column is a nested fixed effect, the first-stage
#'   correction solve is skipped entirely.
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
#'     second_stage = ~ i(rel_year, ref=Inf))
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
#' data(castle, package = "did2s")
#'
#' did2s(
#' 	data = castle,
#' 	yname = "l_homicide",
#' 	first_stage = ~ 0 | state + year,
#' 	second_stage = ~ i(post, ref=0),
#' 	treatment = "post",
#' 	cluster_var = "state",
#'  weights = "popwt"
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
  vcov_method = "sparse",
  vcov_tol = 1e-10,
  vcov_maxiter = 2000L,
  verbose = FALSE
) {
  # Check Parameters -----------------------------------------------------------
  dreamerr::check_arg(data, "data.frame")
  dreamerr::check_value(data[[treatment]], "logical (loose) vector")

  if (
    length(vcov_method) != 1L ||
      !is.character(vcov_method) ||
      !vcov_method %in% c("sparse", "matrix_free")
  ) {
    stop(
      "`vcov_method` must be either \"sparse\" or \"matrix_free\".",
      call. = FALSE
    )
  }
  if (
    !is.numeric(vcov_tol) ||
      length(vcov_tol) != 1L ||
      !is.finite(vcov_tol) ||
      vcov_tol <= 0
  ) {
    stop("`vcov_tol` must be a single positive finite number.", call. = FALSE)
  }
  if (
    !is.numeric(vcov_maxiter) ||
      length(vcov_maxiter) != 1L ||
      !is.finite(vcov_maxiter) ||
      vcov_maxiter < 1 ||
      vcov_maxiter != as.integer(vcov_maxiter)
  ) {
    stop("`vcov_maxiter` must be a positive integer.", call. = FALSE)
  }
  vcov_maxiter <- as.integer(vcov_maxiter)

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

  # Point Estimates ------------------------------------------------------------
  est <- did2s_estimate(
    data = data,
    yname = yname,
    first_stage = first_stage,
    second_stage = second_stage,
    treatment = treatment,
    weights = weights,
    bootstrap = bootstrap
  )

  # Analytic Standard Errors ---------------------------------------------------
  # With X10 equal to the untreated rows of X1 (equivalently, with treated
  # rows set to zero), the
  # observation-level influence function is
  #
  #   IF_i = (X2' X2)^(-1) [
  #     X2_i' u2_i
  #     - X2' X1 (X10' X10)^(-1) X10_i' u1_i
  #   ].
  #
  # The first term is the normal second-stage score, while the second
  # term accounts for estimation uncertainty in the fitted values from
  # the first-stage.
  #
  # The cluster-robust VCOV is sum_g IF_g IF_g', where
  # IF_g = sum_{i in g} IF_i. Square-root weights are incorporated into X1,
  # X10, X2, u1, and u2 below, so no separate weight matrices appear.
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

    # x10 is the first-stage design among untreated observations. Subsetting
    # avoids a full copy of x1 with structurally stored zeros in treated rows.
    untreated_rows <- data[[treatment]] == 0L
    x10 <- x1[untreated_rows, , drop = FALSE]

    x2tx2_inv <- (est$second_stage$cov.iid / est$second_stage$sigma2)
    cl <- data[[cluster_var]]

    ## Here, we are checking if any of the fixed effects are nested within the clusters
    ## e.g. student FE and student cluster or student FE and classroom cluster.
    ## If we find any nesting, these don't need to be included when we calculate the
    ## influence function because after summing IF_i by cluster, these columns will be
    ## exactly equal to 0 from the first-stage estimation equations
    ##
    fe_nesting <- check_nesting_of_fe_in_clusters(
      first_stage = est$first_stage,
      data = data,
      cluster_name = cluster_var
    )
    score_columns <- get_score_columns(fe_nesting, x1)

    # The sandwich only uses cluster sums of the observation scores.
    # Aggregate the sparse scores first instead of materializing a dense
    # n_second_stage_coefs by n_observations influence matrix.
    cluster_id <- match(cl, unique(cl))
    cluster_sum_operator <- Matrix::sparseMatrix(
      i = cluster_id,
      j = seq_along(cluster_id),
      x = 1,
      dims = c(max(cluster_id), length(cluster_id))
    )

    second_stage_score <- cluster_sum_operator %*% (x2 * second_u)
    if (length(score_columns) == 0L) {
      # Every first-stage column is a fixed effect nested in the clusters, so
      # every cluster-level first-stage score is zero by the normal equations.
      first_stage_adjustment <- 0
    } else {
      first_stage_score <-
        cluster_sum_operator[, untreated_rows, drop = FALSE] %*%
        (x10[, score_columns, drop = FALSE] * first_u[untreated_rows])

      # The first-stage adjustment is
      #
      #   Scores_1 (X10' X10)^(-1) X1' X2
      #
      rhs <- Matrix::crossprod(x1, x2)
      correction_coef <- did2s_first_stage_solve(
        x10 = x10,
        rhs = rhs,
        method = vcov_method,
        tol = vcov_tol,
        maxiter = vcov_maxiter
      )
      first_stage_adjustment <- first_stage_score %*%
        correction_coef[score_columns, , drop = FALSE]
    }

    cluster_moment <- second_stage_score - first_stage_adjustment
    IF <- cluster_moment %*% Matrix::t(x2tx2_inv)
    cov <- as.matrix(Matrix::crossprod(IF))

    rownames(cov) <- colnames(cov) <- names(est$second_stage$coefficients)
  }

  # Bootstrap Standard Errors --------------------------------------------------
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

  # Prepare return object ------------------------------------------------------
  # summary creates fixest object with correct standard errors and vcov
  vcov_list <- list()
  vcov_list[[sprintf("Corrected Clustered (%s)", cluster_var)]] <- cov
  est <- base::suppressWarnings(summary(est$second_stage, vcov = vcov_list))
  return(est)
}


# Fixed-effect nesting ---------------------------------------------------------

# Check whether each first-stage fixed effect is nested in the clusters.
# A fixed effect is nested when each of its levels belongs to exactly one
# cluster.
check_nesting_of_fe_in_clusters <- function(first_stage, data, cluster_name) {
  fixef_vars <- first_stage$fixef_vars
  if (is.null(fixef_vars) || length(fixef_vars) == 0L) {
    return(stats::setNames(logical(), character()))
  }

  cl <- data[[cluster_name]]
  vapply(
    fixef_vars,
    function(fixef_name) {
      if (identical(fixef_name, cluster_name)) {
        return(TRUE)
      }
      fixef_var <- data[[fixef_name]]
      if (is.null(fixef_var)) {
        return(FALSE)
      }
      data.table::uniqueN(data.table::data.table(fixef_var, cl)) ==
        data.table::uniqueN(fixef_var)
    },
    logical(1L)
  )
}


# Locate sparse-model-matrix columns with potentially nonzero cluster scores.
get_score_columns <- function(fe_nesting, x1) {
  nested_fixef <- names(fe_nesting)[fe_nesting]
  if (length(nested_fixef) == 0L || ncol(x1) == 0L) {
    return(seq_len(ncol(x1)))
  }

  fixef_labels <- sub("::.*$", "", colnames(x1))
  fixef_labels <- sub("\\[\\[.*\\]\\]$", "", fixef_labels)

  which(!fixef_labels %in% nested_fixef)
}


# First-stage correction backends ----------------------------------------------
# Solve (X10' X10) B = rhs with the requested backend.
did2s_first_stage_solve <- function(
  x10,
  rhs,
  method = "sparse",
  tol = 1e-10,
  maxiter = 2000L
) {
  if (identical(method, "sparse")) {
    solution <- robust_solve_XtX(x10, rhs)
    return(solution)
  } else if (identical(method, "matrix_free")) {
    solution <- pcg_crossprod(
      X = x10,
      rhs = rhs,
      tol = tol,
      maxiter = maxiter
    )
    convergence <- attr(solution, "convergence")
    if (any(!convergence$converged)) {
      warning(
        sprintf(
          paste0(
            "Matrix-free VCOV solve did not converge for %d of %d ",
            "right-hand sides (maximum relative residual %.3e)."
          ),
          sum(!convergence$converged),
          nrow(convergence),
          max(convergence$relative_residual)
        ),
        call. = FALSE
      )
    }
    return(solution)
  } else {
    stop(
      "`vcov_method` must be either \"sparse\" or \"matrix_free\".",
      call. = FALSE
    )
  }
}

# Jacobi-preconditioned conjugate gradients for (X' X) B = rhs.
pcg_crossprod <- function(X, rhs, tol = 1e-10, maxiter = 2000L) {
  rhs <- as.matrix(rhs)
  n_coef <- ncol(X)
  n_rhs <- ncol(rhs)

  if (nrow(rhs) != n_coef) {
    stop("`rhs` must have one row per column of `X`.", call. = FALSE)
  }

  diagonal <- as.numeric(Matrix::colSums(X^2))
  diagonal[!is.finite(diagonal) | diagonal <= 0] <- 1

  solution <- matrix(0, nrow = n_coef, ncol = n_rhs)
  converged <- logical(n_rhs)
  iterations <- integer(n_rhs)
  relative_residual <- numeric(n_rhs)

  for (j in seq_len(n_rhs)) {
    target <- rhs[, j]
    target_norm <- sqrt(sum(target * target))

    if (target_norm == 0) {
      converged[j] <- TRUE
      next
    }

    estimate <- numeric(n_coef)
    residual <- target
    preconditioned_residual <- residual / diagonal
    direction <- preconditioned_residual
    residual_inner <- sum(residual * preconditioned_residual)
    relres <- 1

    for (iteration in seq_len(maxiter)) {
      normal_direction <- as.numeric(
        Matrix::crossprod(X, X %*% direction)
      )
      curvature <- sum(direction * normal_direction)

      if (
        !is.finite(curvature) ||
          curvature <= 0 ||
          !is.finite(residual_inner) ||
          residual_inner <= 0
      ) {
        break
      }

      step <- residual_inner / curvature
      estimate <- estimate + step * direction
      residual <- residual - step * normal_direction
      relres <- sqrt(sum(residual * residual)) / target_norm
      iterations[j] <- iteration

      if (!is.finite(relres) || relres > 1e6 || relres <= tol) {
        break
      }

      preconditioned_residual <- residual / diagonal
      next_residual_inner <- sum(residual * preconditioned_residual)
      direction <- preconditioned_residual +
        (next_residual_inner / residual_inner) * direction
      residual_inner <- next_residual_inner
    }

    solution[, j] <- estimate
    converged[j] <- is.finite(relres) && relres <= tol
    relative_residual[j] <- relres
  }

  attr(solution, "convergence") <- data.frame(
    converged = converged,
    iterations = iterations,
    relative_residual = relative_residual
  )
  solution
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


# Point estimate for did2s -----------------------------------------------------
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
    fixef.rm = "none",
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
