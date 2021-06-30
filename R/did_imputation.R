#' **Work in Progress** Treatment effect estimation and pre-trend testing in staggered adoption diff-in-diff designs with an imputation approach of Borusyak, Jaravel, and Spiess (2021)
#'
#' @param data A data frame
#' @param yname outcome variable
#' @param idname variable for unique unit id
#' @param gname variable for unit-specific date of treatment (never-treated should be zero or NA)
#' @param tname variable for calendar period
#' @param first_stage formula for Y(0)
#' @param weights estimation weights for observations. This is used in estimating Y(0) and also augments treatment effect weights
#' @param wtr character vector of treatment weight names (see horizon/allhorizon for standard event study weights)
#' @param horizon vector of event_time. This only applies if wtr is left as NULL. If both wtr and horizon are null, then all values of event_time will be used.
#'
#' @export
did_imputation = function(data, yname, gname, tname, idname, first_stage, weights = NULL, wtr = NULL, horizon = NULL){

# Set-up Parameters ------------------------------------------------------------

	# Extract vars from formula
	if(inherits(first_stage, "formula")) first_stage = as.character(first_stage)[[2]]

	# Treat
	data$zz000treat = 1 * (data[[tname]] >= data[[gname]]) * (data[[gname]] > 0)
	data[is.na(data$zz000treat), "zz000treat"] = 0

	# Create event time
	data = data %>% dplyr::mutate(
			zz000event_time = dplyr::if_else(
				is.na(!!rlang::sym(gname)) | !!rlang::sym(gname) == 0,
				-Inf,
				as.numeric(!!rlang::sym(tname) - !!rlang::sym(gname))
			)
		)

	# horizon/allhorizon options
	if(is.null(wtr)) {
		use_horizon = TRUE

		# create event time weights
		event_time = unique(data$zz000event_time)
		event_time = event_time[is.finite(event_time)]
		wtr = c()

		# allhorizon
		if(is.null(horizon)) {
			horizon = event_time
		}

		for(e in event_time) {
			if(e %in% horizon) {
				if(e >= 0) {
					var = paste0("zz000wtr", e)

					wtr = c(wtr, var)

					data = data %>% dplyr::mutate(
							!!rlang::sym(var) := 1*(zz000event_time == e & !is.na(zz000event_time)),
							!!rlang::sym(var) := !!rlang::sym(var)/sum(!!rlang::sym(var))
						)
				}
			}
		}
	}

	# Weights specified or not
	if(is.null(weights)) {
		weights_vector = NULL
	} else {
		weights_vector = data[[weights]]

		# Estimation weights * wtr weights
		for(w in wtr) {
			data[[w]] = data[[w]] * weights_vector
			data[[w]] = data[[w]]/sum(data[[w]])
		}

		# only treated
		weights_vector = weights_vector[data$zz000treat == 0]
	}

# First Stage estimate ---------------------------------------------------------

	# First stage among untreated
	formula = stats::as.formula(glue::glue("{yname} ~ {first_stage}"))

	# Estimate Y(0) using untreated observations
	first_stage_est = fixest::feols(formula, se = "standard", dplyr::filter(data, zz000treat == 0), weights = weights_vector, warn=FALSE, notes=FALSE)


	# Residualize outcome variable
	data$zz000adj = data[[yname]] - stats::predict(first_stage_est, newdata = data)

	# Point estimate for wtr
	est = c()
	for(w in wtr) {
		# \sum w_{it} * \tau_{it}
		est = c(est, sum(data[data$zz000treat == 1,][[w]] * data[data$zz000treat == 1,][["zz000adj"]]))
	}


# Standard Errors --------------------------------------------------------------

	# Create Zs
	Z = sparse_model_matrix(data, first_stage_est)

	# - Z (Z_0' Z_0)^{-1} Z_1' wtr_1
	v_star = make_V_star(
		Z,
		Z[data$zz000treat == 0, ],
		Z[data$zz000treat == 1, ],
		Matrix::Matrix(as.matrix(data[data$zz000treat == 1, wtr]), sparse = TRUE)
	)

	se = c()
	for(i in 1:length(wtr)) {

		# Equation (6) of Borusyak et. al. 2021
		# Calculate v_it^* = - Z (Z_0' Z_0)^{-1} Z_1' * w_1
		data$zz000v = v_star[, i]

		# fix v_it^* = w for treated observations
		data[data$zz000treat == 1, "zz000v"] = data[data$zz000treat == 1,][[wtr[i]]]

		# Equation (10) of Borusyak et. al. 2021
		# Calculate tau_it - \bar{\tau}_{et}
		data = data %>%
			# group_by Event Group and Event Time
			dplyr::group_by(!!rlang::sym(gname), zz000event_time)  %>%
			dplyr::mutate(
				zz000tau_et = if_else(zz000treat == 1,
									  sum(zz000v^2 * zz000adj)/sum(zz000v^2) * zz000treat,
									  0),
				zz000tau_et = if_else(is.nan(zz000tau_et), 0, zz000tau_et),
				zz000tau_centered = zz000adj - zz000tau_et
			) %>%
			dplyr::ungroup()


		# Equation (8)
		# Calculate variance of estimate
		variance = data %>%
			dplyr::group_by(!!rlang::sym(idname)) %>%
			dplyr::summarize(zz000temp = sum(zz000v * zz000tau_centered)^2) %>%
			dplyr::pull(zz000temp) %>%
			sum()

		se = c(se, sqrt(variance))
	}


# Pre-event Estimates ----------------------------------------------------------

	if(use_horizon) {
		pre_formula <- stats::as.formula(glue::glue(glue::glue("{yname} ~ i(zz000event_time) | {idname} + {tname}")))
		pre_est <- fixest::feols(pre_formula, data %>% dplyr::filter(zz000treat == 0), weights = weights_vector, warn=FALSE, notes=FALSE)
	}


# Create dataframe of results in tidy format -----------------------------------

	# Fix term for horizon option
	wtr = stringr::str_replace(wtr, "zz000wtr", "")

	out <- dplyr::tibble(
		term      = as.numeric(wtr),
		estimate  = est,
		std.error = se,
		conf.low  = est - 1.96 * se,
		conf.high = est + 1.96 * se
	)

	if(use_horizon) {
		pre_out <- broom::tidy(pre_est) %>%
			dplyr::mutate(
				term = stringr::str_remove(term, "zz000event_time::"),
				term = as.numeric(term),
				conf.low = estimate - 1.96 * std.error,
				conf.high = estimate + 1.96 * std.error
			) %>%
			dplyr::filter(term %in% horizon) %>%
			dplyr::select(term, estimate, std.error, conf.low, conf.high)

		out = dplyr::bind_rows(pre_out, out)
	}

	return(out)
}



# Make a sparse_model_matrix for fixest ----------------------------------------

# Make a sparse_model_matrix for fixest
sparse_model_matrix = function(data, fixest) {
	Z = NULL

	# Coefficients
	if("coefficients" %in% names(fixest)) Z = as(model.matrix(fixest, data = data), "sparseMatrix")

	# Fixed Effects
	if("fixef_id" %in% names(fixest)) {
		fixef_list = fixest::fixef(fixest)

		mats = lapply(seq_along(fixef_list), function(i) {
			var = names(fixef_list)[i]
			vals = names(fixef_list[[i]])

			ind = lapply(vals, function(val){
				Matrix::Matrix(as.numeric(data[[var]] == val), ncol = 1, sparse = TRUE)
			})

			do.call("cbind", ind)
		})

		Z = cbind(Z, do.call("cbind", mats))
	}

	return(Z)
}
