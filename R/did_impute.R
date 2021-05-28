did_impute <- function(data, yname, group_name, first_stage_formula, treat_var, weights){
	# Treat
	treat <- data[data[[treat_var]] == 1,]

	# First stage among untreated
	formula <- stats::as.formula(glue::glue("{yname} ~ 0 + {first_stage_formula}"))
	first_stage <- fixest::feols(formula, se = "standard", dplyr::filter(data, !!rlang::sym(treat_var) == 0), warn=FALSE, notes=FALSE)

	# Residualize outcome variable
	data$zz000adj <- data[[yname]] - stats::predict(first_stage, newdata = data)

	# Point estimate for weights
	est <- c()
	for(weight in weights) {
		# \sum w_{it} * \tau_{it}
		est = c(est, sum(data[data[[treat_var]] == 1,][[weight]] * data[data[[treat_var]] == 1,][["zz000adj"]]))
	}



	# Equation (6) of Borusyak et. al. 2021
	# Calculate v_it^*

	# Create Zs
	Z_0 <- create_Z(0)
	Z_1 <- create_Z(1)
	Z <- create_Z(c(0,1))
	w <- data[data[[treat_var]] == 1,][[weight]]

	# Not Working
	# v <- - t(Z) %*% solve(t(Z_0) %*% Z_0) %*% t(Z_1) %*% w
	v <- w

	# Equation (10) of Borusyak et. al. 2021
	# Calculate \bar{\tau}_{et}

	# Equation (8)
	# Calculate variance of estimate

}



create_Z <- function(treat_val) {
	data <- dplyr::filter(data, !!rlang::sym(treat_var) == treat_val)

	Z <- lapply(names(stats::coef(first_stage)), function(x) {
		# intercept
		if(x == "(Intercept)") {
			return(rep(1, times = nrow(data)))
		}
		# factor variables
		else if(stringr::str_detect(x, "::")) {
			var <- stringr::str_extract(x, ".*(?=::)")
			val <- stringr::str_extract(x, "(?<=::).*")

			if(inherits(data[[var]], "numeric")){
				return(data[[var]] == val)
			} else {
				return(data[[var]] == 'val')
			}
		}
		# covariates
		else {
			return(data[[x]])
		}
	})

	Z <- matrix(unlist(Z), nrow = nrow(data))
	return(Z)
}




data("df_hom")

# weight = 1/n
data = df_hom %>% group_by(treat) %>% mutate(weight = 1/n() * treat)
yname = "dep_var"
group_name = "group"
first_stage_formula = "i(state) + i(year)"
treat_var = "treat"
weights = "weight"

