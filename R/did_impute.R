did_impute <- function(data, yname, first_stage_formula, treat_var, weights){

	# First stage among untreated
	formula <- stats::as.formula(glue::glue("{yname} ~ 0 + {first_stage_formula}"))
	first_stage <- fixest::feols(formula, se = "standard", dplyr::filter(data, !!rlang::sym(treat_var) == 0), warn=FALSE, notes=FALSE)

	# Residualize outcome variable
	data$zz000adj <- data[[yname]] - stats::predict(first_stage, newdata = data)

	# Point estimate for weights
	est <- c()
	for(weight in weights) {
		# \sum w_{it} * \tau_{it}
		est = c(est, sum(data[[weight]] * data[["zz000adj"]]))
	}

	# Equation (6) of Borusyak et. al. 2021
	# Calculate v_it^*


	# Equation (10) of Borusyak et. al. 2021
	# Calculate \bar{\tau}_{et}

	# Equation (8)
	# Calculate variance of estimate

}








data("df_hom")

# weight = 1/n
data = df_hom %>% group_by(treat) %>% mutate(weight = 1/n() * treat)
yname = "dep_var"
first_stage_formula = "i(state) + i(year)"
treat_var = "treat"
weights = "weight"

