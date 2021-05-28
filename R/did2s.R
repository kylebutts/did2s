#' Calculate two-stage difference-in-differences following Gardner (2021)
#'
#' @param data The dataframe containing all the variables
#' @param yname Outcome variable
#' @param first_stage_formula Fixed effects and other covariates you want to residualize with in first stage, use i() for fixed effects., following fixest::feols.
#' @param treat_formula Second stage, these should be the treatment indicator(s) (e.g. treatment variable or es leads/lags), use i() for factor variables, following fixest::feols.
#' @param treat_var A variable that = 1 if treated, = 0 otherwise
#' @param cluster_var What variable to cluster standard errors. This can be IDs or a higher aggregate level (state for example)
#' @param n_bootstraps How many bootstraps to run. Default is 250
#'
#' @return fixest::feols point estimate with bootstrap standard errors
#' @export
#' @examples
#' # Load Example Dataset
#' data("df_hom")
#'
#' # Static
#' static <- did2s(df_hom, yname = "dep_var", first_stage_formula = "i(state) + i(year)", treat_formula = "i(treat)", treat_var = "treat", cluster_vars = "state")
#' fixest::esttable(static)
#'
#' # Event-Study
#' es <- did2s(df_hom, yname = "dep_var", first_stage_formula = "i(state) + i(year)", treat_formula = "i(rel_year)", treat_var = "treat", cluster_var = "state")
#' fixest::esttable(es)
#'
did2s <- function(data, yname, first_stage_formula, treat_formula, treat_var, cluster_var, n_bootstraps = 250) {

	# Check Parameters ---------------------------------------------------------

	# Extract vars from formula
	if(inherits(first_stage_formula, "formula")) first_stage_formula <- as.character(first_stage_formula)[[2]]
	if(inherits(treat_formula, "formula")) treat_formula <- as.character(treat_formula)[[2]]


	# Point Estimates ----------------------------------------------------------

	estimate <- did2s_estimate(
		data = data,
		yname = yname,
		first_stage_formula = first_stage_formula,
		treat_formula = treat_formula,
		treat_var = treat_var
	)

	cli::cli_alert("Starting {n_bootstraps} bootstraps at cluster level: {cluster_var}")

	samples <- rsample::bootstraps(data, times = n_bootstraps, strata = all_of(cluster_var), pool = 0)

	estimates <- purrr::map_dfr(samples$splits, function(x) {
						   	data <- as.data.frame(x)

						   	estimate <- did2s_estimate(
						   		data = data,
						   		yname = yname,
						   		first_stage_formula = first_stage_formula,
						   		treat_formula = treat_formula,
						   		treat_var = treat_var
						   	)

						   	return(coef(estimate$second_stage))
				   })

	cov <- cov(estimates)


	# summary creates fixest object with correct standard errors and vcov
	return(base::suppressWarnings(summary(estimate$second_stage, .vcov = cov)))
}



did2s_estimate <- function(data, yname, first_stage_formula, treat_formula, treat_var) {
	# First stage among untreated
	formula <- stats::as.formula(glue::glue("{yname} ~ 0 + {first_stage_formula}"))

	first_stage <- fixest::feols(formula, se = "standard", dplyr::filter(data, !!rlang::sym(treat_var) == 0), warn=FALSE, notes=FALSE)
	first_stage_cov <- stats::vcov(first_stage)

	# Residualize outcome variable
	data$adj <- data[[yname]] - stats::predict(first_stage, newdata = data)

	# Second stage
	formula <- stats::as.formula(glue::glue("adj ~ {treat_formula}"))

	second_stage <- fixest::feols(formula, se = "standard", data = data, warn=FALSE, notes=FALSE)
	second_stage_cov <- stats::vcov(second_stage)

	return(list(
		first_stage = first_stage,
		first_stage_cov = first_stage_cov,
		second_stage = second_stage,
		second_stage_cov = second_stage_cov
	))
}
