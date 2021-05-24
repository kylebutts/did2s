#' Calculate two-stage difference-in-differences following Gardner (2021)
#'
#' @param data the dataframe containing all the variables
#' @param yname Outcome variable
#' @param first_stage_formula fixed effects and other covariates you want to residualize with in first stage, use i() for fixed effects., following fixest::feols.
#' @param treat_formula second stage, these should be the treatment indicator(s) (e.g. treatment variable or es leads/lags), use i() for factor variables, following fixest::feols.
#' @param treat_var a variable that = 1 if treated, = 0 otherwise
#' @param cluster_vars what variable to cluster standard errors
#'
#' @return list containing fixest estimate and corrected variance-covariance matrix
#' @export
#' @examples
#' # Load Example Dataset
#' data("df_hom")
#'
#' # Static
#' static <- did2s(df_hom, yname = "dep_var", first_stage_formula = "i(state) + i(year)", treat_formula = "i(treat)", treat_var = "treat", cluster_vars = "state")
#' summary(static$estimate, .vcov = static$adj_cov)
#'
#' # Event-Study
#' es <- did2s(df_hom, yname = "dep_var", first_stage_formula = "i(state) + i(year)", treat_formula = "i(rel_year)", treat_var = "treat", cluster_vars = "state")
#' summary(es$estimate, .vcov = es$adj_cov)
#'
did2s <- function(data, yname, first_stage_formula, treat_formula, treat_var, cluster_vars = NULL) {

	# Extract vars from formula
	if(inherits(first_stage_formula, "formula")) first_stage_formula <- as.character(first_stage_formula)[[2]]
	if(inherits(treat_formula, "formula")) treat_formula <- as.character(treat_formula)[[2]]

	# First stage among untreated
	formula <- stats::as.formula(glue::glue("{yname} ~ 0 + {first_stage_formula}"))

	if(is.null(cluster_vars)) {
		first_stage <- fixest::feols(formula, se = "standard",        dplyr::filter(data, !!rlang::sym(treat_var) == 0), warn = FALSE)
	} else {
		first_stage <- fixest::feols(formula, cluster = cluster_vars, dplyr::filter(data, !!rlang::sym(treat_var) == 0), warn = FALSE)
	}
	first_stage_cov <- stats::vcov(first_stage)

	# Residualize outcome variable
	data$adj <- data[[yname]] - stats::predict(first_stage, newdata = data)

	# Second stage
	formula <- stats::as.formula(glue::glue("adj ~ {treat_formula}"))

	if(is.null(cluster_vars)) {
		second_stage <- fixest::feols(formula, se = "standard",        data = data, warn = FALSE)
	} else {
		second_stage <- fixest::feols(formula, cluster = cluster_vars, data = data, warn = FALSE)
	}
	second_stage <- fixest::feols(formula, cluster = cluster_vars, data = data, warn = FALSE)
	second_stage_cov <- stats::vcov(second_stage)

	# get variable names
	c <- lapply(names(stats::coef(first_stage)), function(x) {
		# intercept
		if(x == "(Intercept)") {
			return(c(1,0))
		}
		# factor variables
		else if(stringr::str_detect(x, "::")) {
			var <- stringr::str_extract(x, ".*(?=::)")
			val <- stringr::str_extract(x, "(?<=::).*")
			formula <- stats::as.formula(glue::glue("({val} == {var}) ~ {treat_formula}"))
			return(fixest::feols(formula, data = data, warn = FALSE)$coefficients)
		}
		# covariates
		else {
			formula <- stats::as.formula(glue::glue("{x} ~ {treat_formula}"))
			return(fixest::feols(formula, data = data, warn = FALSE)$coefficients)
		}



	})
	c <- matrix(unlist(c), ncol = length(names(coef(second_stage))), byrow = T)

	cov <- second_stage_cov + t(c) %*% first_stage_cov %*% c

	# summary creates fixest object with correct standard errors and vcov
	return(summary(second_stage, .vcov = cov))
}
