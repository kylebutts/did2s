#' Treatment effect estimation and pre-trend testing in staggered adoption diff-in-diff designs with an imputation approach of Borusyak, Jaravel, and Spiess (2021)
did_impute <- function(data, yname, gname, tname, idname, first_stage_formula, treat_var, weights){
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


	# Create Zs
	Z_0 <- create_Z(0)
	Z_1 <- create_Z(1)
	Z <- create_Z(c(0,1))
	v_proj <- - Z %*% solve(t(Z_0) %*% Z_0) %*% t(Z_1)

	se <- c()
	for(weight in weights) {

		# Equation (6) of Borusyak et. al. 2021
		# Calculate v_it^*
		data$zz000v <- v_proj %*% w

		# Equation (10) of Borusyak et. al. 2021
		# Calculate tau_it - \bar{\tau}_{et}
		data <- data %>%
			# Create event time
			dplyr::mutate(
				zz000event_time = !!rlang::sym(tname) - !!rlang::sym(gname)
			)
			# group_by Event Group and Event Time
			dplyr::group_by(!!rlang::sym(gname), zz000event_time)  %>%
			dplyr::mutate(
				zz000tau_et = if_else(!!rlang::sym(treat_var) == 1,
									  sum(zz000v^2 * zz000adj)/sum(zz000v^2) * treat,
									  0),
				zz000tau_centered = zz000adj - zz000tau_et
			) %>%
			dplyr::ungroup()

		# Equation (8)
		# Calculate variance of estimate
		variance <- data %>%
			dplyr::group_by(!!rlang::sym(id_name)) %>%
			dplyr::summarize(zz000temp = sum(zz000v * zz000tau_centered)^2) %>%
			dplyr::pull(zz000temp) %>%
			mean()

		se <- c(se, sqrt(variance))
	}

	return(list(estimate = est, se = sqrt(variance)))
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

			return(data[[var]] == val)
		}
		# covariates
		else {
			return(data[[x]])
		}
	})

	Z <- matrix(unlist(Z), nrow = nrow(data))
	return(Z)
}




## Testing ---------------------------------------------------------------------
# data("df_hom")
#
# # weight = 1/n
# data = df_hom %>% group_by(treat) %>% mutate(weight = 1/n() * treat)
# yname = "dep_var"
# group_name = "group"
# rel_year_name = "rel_year"
# id_name = "unit"
# first_stage_formula = "i(state) + i(year)"
# treat_var = "treat"
# weights = "weight"

