#' Calculate two-stage difference-in-differences, following Gardner (2021)
#'
#' @param data the dataframe containing all the variables
#' @param yname Outcome variable
#' @param first_stage_formula fixed effects and other covariates you want to residualize with in first stage, use i() for fixed effects., following fixest::feols.
#' @param treat_formula second stage, these should be the treatment indicator(s) (e.g. treatment variable or es leads/lags), use i() for factor variables, following fixest::feols.
#' @param treat_var a variable that = 1 if treated, = 0 otherwise
#' @param cluster_vars what variable to cluster standard errors
#'
did2s <- function(data, yname, first_stage_formula, treat_formula, treat_var, cluster_vars) {

	# Extract vars from formula
	if(inherits(first_stage_formula, "formula")) first_stage_formula <- as.character(first_stage_formula)[[2]]
	if(inherits(treat_formula, "formula")) treat_formula <- as.character(treat_formula)[[2]]

	# First stage among untreated
	formula <- glue("{yname} ~ 0 + {first_stage_formula}") %>% as.formula()

	first_stage <- fixest::feols(formula, cluster = cluster_vars, data %>% filter(!!sym(treat_var) == 0))
	first_stage_cov <- vcov(first_stage)

	# Residualize outcome variable
	data$adj <- data[[yname]] - predict(first_stage, newdata = data)

	# Second stage
	formula <- glue("adj ~ {treat_formula}") %>% as.formula()

	second_stage <- fixest::feols(formula, cluster = cluster_vars, data = data, warn = FALSE)
	second_stage_cov <- vcov(second_stage)

	# get variable names
	c <- lapply(names(coef(first_stage)), function(x) {
		# intercept
		if(x == "(Intercept)") {
			data$intercept <- 1
			formula <- glue("intercept ~ 0 + {treat_formula}") %>% as.formula()
		}
		# factor variables
		else if(str_detect(x, "::")) {
			var <- str_extract(x, ".*(?=::)")
			val <- str_extract(x, "(?<=::).*")
			formula <- glue("({val} == {var}) ~ {treat_formula}") %>% as.formula()
		}
		# covariates
		else {
			formula <- glue("{x} ~ {treat_formula}") %>% as.formula()
		}

		return(fixest::feols(formula, data = data, warn = FALSE)$coefficients)

	}) %>% unlist() %>% matrix(ncol = length(names(coef(second_stage))), byrow = T)

	cov <- second_stage_cov + t(c) %*% first_stage_cov %*% c

	summary(second_stage, .vcov = cov)
	return(list(estimate = second_stage, adj_cov = cov))
}
