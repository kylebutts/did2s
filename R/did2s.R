#' Calculate two-stage difference-in-differences following Gardner (2021)
#'
#' @param data The dataframe containing all the variables
#' @param yname Outcome variable
#' @param first_stage_formula Fixed effects and other covariates you want to residualize with in first stage, use i() for fixed effects., following fixest::feols.
#' @param treat_formula Second stage, these should be the treatment indicator(s) (e.g. treatment variable or es leads/lags), use i() for factor variables, following fixest::feols.
#' @param treat_var A variable that = 1 if treated, = 0 otherwise
#' @param cluster_var What variable to cluster standard errors. This can be IDs or a higher aggregate level (state for example)
#' @param weights Optional variable to run a weighted first- and second-stage regressions
#' @param bootstrap Should standard errors be calculated using bootstrap? Default is FALSE.
#' @param n_bootstraps How many bootstraps to run. Default is 250
#'
#' @return fixest::feols point estimate with adjusted standard errors (either by formula or by bootstrap)
#' @export
#' @examples
#' # Load Example Dataset
#' data("df_hom")
#'
#' # Static
#' static <- did2s(df_hom, yname = "dep_var", first_stage_formula = "i(state) + i(year)", treat_formula = "i(treat)", treat_var = "treat", cluster_var = "state")
#' fixest::esttable(static)
#'
#' # Event-Study
#' es <- did2s(df_hom, yname = "dep_var", first_stage_formula = "i(state) + i(year)", treat_formula = "i(rel_year)", treat_var = "treat", cluster_var = "state")
#' fixest::esttable(es)
#'
#' # Castle Data
#' castle <- haven::read_dta("https://github.com/scunning1975/mixtape/raw/master/castle.dta")
#'
#' did2s(
#' 	data = castle,
#' 	yname = "l_homicide",
#' 	first_stage_formula = ~ i(sid) + i(year),
#' 	treat_formula = ~ i(post, ref=0),
#' 	treat_var = "post",
#' 	cluster_var = "state", weights = "popwt"
#' )
#'
did2s <- function(data, yname, first_stage_formula, treat_formula, treat_var, cluster_var, weights = NULL, bootstrap = FALSE, n_bootstraps = 250) {

	# Print --------------------------------------------------------------------

	# Check Parameters ---------------------------------------------------------

	# Extract vars from formula
	if(inherits(first_stage_formula, "formula")) first_stage_formula <- as.character(first_stage_formula)[[2]]
	if(inherits(treat_formula, "formula")) treat_formula <- as.character(treat_formula)[[2]]

	# Point Estimates ----------------------------------------------------------

	est <- did2s_estimate(
		data = data,
		yname = yname,
		first_stage_formula = first_stage_formula,
		treat_formula = treat_formula,
		treat_var = treat_var,
		weights = weights
	)

	# Analytic Standard Errors -------------------------------------------------

	if(!bootstrap) {

		# Extract first stage
		first_u <- data[[yname]] - stats::predict(est$first_stage, newdata = data)
		# Zero out rows with D_it = 1
		first_u[data[[treat_var]] == 1] <- 0

		x1 <- stats::model.matrix(est$first_stage, data = data)
		# Zero out rows with D_it = 1
		x10 <- x1
		x10[data[[treat_var]] == 1] <- 0

		# Extract second stage
		second_u <- stats::residuals(est$second_stage)
		x2 <- stats::model.matrix(est$second_stage, data = data)

		# Unique values of cluster variable
		cl <- unique(data[[cluster_var]])

		meat <- lapply(cl, function(c) {
				idx <- data[[cluster_var]] == c

				# W_g = e_2g' X_2g - e_1g' X_1g^0 (X_1g'^0 X_1g^0) X_1g' X_2g
				v <- t(second_u[idx]) %*% (x2[idx,]) -
					t(first_u[idx]) %*% x10[idx,] %*% solve(t(x10) %*% x10) %*% t(x1) %*% x2

				# = W_g' W_g
				return(t(v) %*% v)
			})

		meat_sum <- Reduce("+", meat)

		# (X_2'X_2)^-1 (sum W_g W_g') (X_2'X_2)^-1
		cov <- solve(t(x2) %*% x2) %*% meat_sum %*% solve(t(x2) %*% x2)
	}


	# Bootstrap Standard Errors ------------------------------------------------

	if(bootstrap) {

		cli::cli_alert("Starting {n_bootstraps} bootstraps at cluster level: {cluster_var}")

		samples <- rsample::bootstraps(data, times = n_bootstraps, strata = all_of(cluster_var), pool = 0)


		estimates <- purrr::map_dfr(samples$splits, function(x) {
			data <- rsample::as.data.frame(x)

			estimate <- did2s_estimate(
				data = data,
				yname = yname,
				first_stage_formula = first_stage_formula,
				treat_formula = treat_formula,
				treat_var = treat_var,
				weights = weights
			)

			return(coef(estimate$second_stage))
		})

		cov <- cov(estimates)

	}

	# summary creates fixest object with correct standard errors and vcov
	return(base::suppressWarnings(summary(est$second_stage, .vcov = cov)))
}



did2s_estimate <- function(data, yname, first_stage_formula, treat_formula, treat_var, weights = NULL) {
	# First stage among untreated
	formula <- stats::as.formula(glue::glue("{yname} ~ 0 + {first_stage_formula}"))

	untreat <- dplyr::filter(data, !!rlang::sym(treat_var) == 0)
	if(is.null(weights)) {
		weights_vector <- NULL
	} else {
		weights_vector <- untreat[[weights]]
	}


	first_stage <- fixest::feols(formula, data = untreat, weights = weights_vector, warn=FALSE, notes=FALSE)
	first_stage_cov <- stats::vcov(first_stage)

	# Residualize outcome variable but keep same yname
	data[[yname]] <- data[[yname]] - stats::predict(first_stage, newdata = data)

	# Second stage
	formula <- stats::as.formula(glue::glue("{yname} ~ 0 + {treat_formula}"))

	if(!is.null(weights)) weights_vector <- data[[weights]]

	second_stage <- fixest::feols(formula, data = data, weights = weights_vector, warn=FALSE, notes=FALSE)
	second_stage_cov <- stats::vcov(second_stage)

	return(list(
		first_stage = first_stage,
		first_stage_cov = first_stage_cov,
		second_stage = second_stage,
		second_stage_cov = second_stage_cov
	))
}
