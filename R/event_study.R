#' Estimate event-study coefficients using TWFE and 5 proposed improvements.
#'
#' @description Uses the estimation procedures recommended from Borusyak, Jaravel, Spiess (2021); Callaway and Sant'Anna (2020); Gardner (2021); Roth and Sant'Anna (2021); Sun and Abraham (2020)
#'
#' @param data The dataframe containing all the variables
#' @param yname Variable name for outcome variable
#' @param idname Variable name for unique unit id
#' @param tname Variable name for calendar period
#' @param gname Variable name for unit-specific date of initial treatment (never-treated should be zero or NA)
#' @param xformla A formula for the covariates to include in the model. It should be of the form `~ X1 + X2`. Default is NULL.
#' @param horizon Integer of length two. The first integer is the earliest pre-effect to include and the second is the latest post-effect to include. Default is all horizons.
#' @param weights Variable name for estimation weights. This is used in estimating Y(0) and also augments treatment effect weights
#'
#' @return tibble of point estimates for each estimator
#' @export
event_study = function(data, yname, idname, gname, tname, xformla = NULL, horizon = NULL, weights = NULL){

# Setup ------------------------------------------------------------------------

	# Treat
	data$zz000treat = 1 * (data[[tname]] >= data[[gname]]) * (data[[gname]] > 0)
	data[is.na(data$zz000treat), "zz000treat"] = 0

	# Set g to zero if NA
	data[is.na(data[[gname]]), gname] = 0

	# Create event time
	data$zz000event_time = ifelse(
		is.na(data[[gname]]) | data[[gname]] == 0 | data[[gname]] == Inf,
		-Inf,
		as.numeric(data[[tname]] - data[[gname]])
	)

	event_time = unique(data$zz000event_time)
	event_time = event_time[!is.na(event_time) & is.finite(event_time)]
	# All horizons
	if(is.null(horizon)) horizon = event_time

	# Format xformla for inclusion
	if(!is.null(xformla)) {
		xformla_null = paste0("0 + ", as.character(xformla)[[2]])
	} else {
		xformla_null = "0"
	}

# TWFE -------------------------------------------------------------------------

	cli::cli_text("Estimating TWFE Model")

	tidy_twfe = NULL

	try({
		twfe_formula = stats::as.formula(glue::glue("{yname} ~ 1 + {xformla_null} + i(zz000event_time, ref = -1) | {idname} + {tname}"))
		est_twfe = fixest::feols(twfe_formula, data = data, warn = F, notes = F)

		# Extract coefficients and standard errors
		tidy_twfe = broom::tidy(est_twfe)

		# Extract zz000event_time
		tidy_twfe = tidy_twfe[stringr::str_detect(tidy_twfe$term, "zz000event_time::"), ]

		# Make event time into a numeric
		tidy_twfe$term = stringr::str_replace(tidy_twfe$term, "zz000event_time::", "")
		tidy_twfe$term = as.numeric(tidy_twfe$term)

		# Subset column
		tidy_twfe = tidy_twfe[, c("term", "estimate", "std.error")]

		# Add estimator column
		tidy_twfe$estimator = "TWFE"
	})

	if(is.null(tidy_twfe)) cli::cli_warn("TWFE Failed")

# did2s ------------------------------------------------------------------------

	cli::cli_text("Estimating using Gardner (2021)")

	tidy_did2s = NULL

	try({
		did2s_first_stage = stats::as.formula(glue::glue("~ 0 + {xformla_null} | {idname} + {tname}"))

		est_did2s = did2s::did2s(data, yname = yname, first_stage = did2s_first_stage, second_stage = ~i(zz000event_time, ref=-Inf), treatment = "zz000treat", cluster_var = idname, verbose = FALSE)

		# Extract coefficients and standard errors
		tidy_did2s = broom::tidy(est_did2s)

		# Extract zz000event_time
		tidy_did2s = tidy_did2s[stringr::str_detect(tidy_did2s$term, "zz000event_time::"), ]

		# Make event time into a numeric
		tidy_did2s$term = stringr::str_replace(tidy_did2s$term, "zz000event_time::", "")
		tidy_did2s$term = as.numeric(tidy_did2s$term)

		# Subset columns
		tidy_did2s = tidy_did2s[, c("term", "estimate", "std.error")]

		# Add estimator column
		tidy_did2s$estimator = "Gardner (2021)"
	})

	if(is.null(tidy_did2s)) cli::cli_warn("Gardner (2021) Failed")


# did --------------------------------------------------------------------------

	cli::cli_text("Estimating using Callaway and Sant'Anna (2020)")

	tidy_did = NULL

	try({
		est_did = did::att_gt(yname = yname, tname = tname, idname = idname, gname = gname, xformla = xformla, data = data) %>%
			did::aggte(type = "dynamic", na.rm = TRUE)

		# Extract es coefficients
		tidy_did = broom::tidy(est_did)

		# Subset columns
		tidy_did$term = tidy_did$event.time
		tidy_did = tidy_did[, c("term", "estimate", "std.error")]

		tidy_did$estimator = "Callaway and Sant'Anna (2020)"
	})

	if(is.null(tidy_did)) cli::cli_warn("Callaway and Sant'Anna (2020) Failed")

# sunab ------------------------------------------------------------------------

	cli::cli_text("Estimating using Sun and Abraham (2020)")

	tidy_sunab = NULL

	try({
		# Format xformla for inclusion
		if(is.null(xformla)) {
			sunab_xformla = "1"
		} else {
			sunab_xformla = paste0("1 + ", as.character(xformla)[[2]])
		}

		sunab_formla = stats::as.formula(glue::glue("{yname} ~ {sunab_xformla} + sunab({gname}, {tname})"))

		est_sunab = fixest::feols(sunab_formla, data = data)

		tidy_sunab = broom::tidy(est_sunab)

		# Extract zz000event_time
		tidy_sunab = tidy_sunab[stringr::str_detect(tidy_sunab$term, glue::glue("{tname}::")), ]

		# Make event time into a numeric
		tidy_sunab$term = stringr::str_replace(tidy_sunab$term, glue::glue("{tname}::"), "")
		tidy_sunab$term = as.numeric(tidy_sunab$term)

		# Subset columns
		tidy_sunab = tidy_sunab[, c("term", "estimate", "std.error")]

		# Add estimator column
		tidy_sunab$estimator = "Sun and Abraham (2020)"
	})

	if(is.null(tidy_sunab)) cli::cli_warn("Sun and Abraham (2020) Failed")

# did_imputation ---------------------------------------------------------------

	cli::cli_text("Estimating using Borusyak, Jaravel, Spiess (2021)")

	tidy_impute = NULL

	try({
		impute_first_stage = stats::as.formula(glue::glue("~ {xformla_null} | {idname} + {tname}"))

		tidy_impute = didimputation::did_imputation(data,
									 yname = yname, gname = gname, tname = tname, idname = idname,
									 first_stage = impute_first_stage, horizon = TRUE, pretrends = TRUE)

		# Subset columns
		tidy_impute = tidy_impute[, c("term", "estimate", "std.error")]

		# Make event time into a numeric
		tidy_impute$term = as.numeric(tidy_impute$term)

		# Add estimator column
		tidy_impute$estimator = "Borusyak, Jaravel, Spiess (2021)"
	})

	if(is.null(tidy_impute)) cli::cli_warn("Borusyak, Jaravel, Spiess (2021) Failed")

# staggered --------------------------------------------------------------------

	cli::cli_text("Estimatng using Roth and Sant'Anna (2021)")

	tidy_staggered = NULL

	try({
		# Make untreated g = Inf
		data_staggered = data

		data_staggered[,gname] = ifelse(
			data_staggered[[gname]] == 0,
			Inf,
			data_staggered[[gname]]
		)

		tidy_staggered = staggered::staggered(
			data_staggered,
			i = idname, t = tname, g = gname, y = yname, estimand = "eventstudy",
			eventTime = event_time[is.finite(event_time) & event_time != -1]
		)

		# Subset columns
		tidy_staggered$term = tidy_staggered$eventTime
		tidy_staggered$std.error = tidy_staggered$se

		tidy_staggered = tidy_staggered[, c("term", "estimate", "std.error")]

		# Add estimator column
		tidy_staggered$estimator = "Roth and Sant'Anna (2021)"
	})

	if(is.null(tidy_staggered)) cli::cli_warn("Roth and Sant'Anna (2021) Failed")




# Bind results together --------------------------------------------------------

	out = dplyr::bind_rows(tidy_twfe, tidy_did2s, tidy_did, tidy_sunab, tidy_impute, tidy_staggered)

	return(out)

}


#' Plot results of [event_study()]
#' @param out Tibble from [event_study()]
#' @param seperate Logical. Should the estimators be on seperate plots? Default is TRUE.
#' @param horizon Numeric. Vector of length 2. First element is min and second element is max of event_time to plot
#'
#' @return ggplot object that can be fully customized
#' @export
plot_event_study = function(out, seperate = TRUE, horizon = NULL) {

	# Get list of estimators
	estimators = unique(out$estimator)

	# Subset factor levels
	levels = c("TWFE", "Borusyak, Jaravel, Spiess (2021)", "Callaway and Sant'Anna (2020)", "Gardner (2021)", "Roth and Sant'Anna (2021)",  "Sun and Abraham (2020)")
	levels = levels[levels %in% estimators]

	# Make estimator into factor
	out$estimator = factor(out$estimator, levels = levels)


	# Subset color scales
	color_scale = c("TWFE" = "#374E55", "Gardner (2021)" = "#DF8F44", "Callaway and Sant'Anna (2020)" = "#00A1D5", "Sun and Abraham (2020)" = "#B24745", "Roth and Sant'Anna (2021)" = "#79AF97", "Borusyak, Jaravel, Spiess (2021)" = "#6A6599")
	color_scale = color_scale[names(color_scale) %in% estimators]


	# create confidence intervals
	out$ci_lower = out$estimate - 1.96 * out$std.error
	out$ci_upper = out$estimate + 1.96 * out$std.error



	# position depending on sepreate
	if(seperate) position = "identity" else position = ggplot2::position_dodge(width = 0.5)

	# Subset plot if horizon is specified
	if(!is.null(horizon)) {
		out = out[out$term >= horizon[1] & out$term <= horizon[2], ]
	}

	# max and min of limits
	y_lims = c(min(out$ci_lower), max(out$ci_upper)) * 1.05
	x_lims = c(min(out$term) - 1, max(out$term) + 1)

	ggplot2::ggplot(data = out, ggplot2::aes(x = term, y = estimate, color = estimator, ymin = ci_lower, ymax = ci_upper)) +
		{ if(seperate) ggplot2::facet_wrap(~ estimator, scales="free") } +
		ggplot2::geom_point(position = position) +
		ggplot2::geom_errorbar(position = position) +
		ggplot2::geom_vline(xintercept = -0.5, linetype = "dashed") +
		ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
		ggplot2::labs(y = "Point Estimate and 95% Confidence Interval", x = "Event Time", color = "Estimator") +
		{ if(seperate) ggplot2::scale_y_continuous(limits = y_lims) } +
		{ if(seperate) ggplot2::scale_x_continuous(limits = x_lims) } +
		ggplot2::theme_minimal(base_size = 16) +
		ggplot2::scale_color_manual(values = color_scale) +
		ggplot2::guides(
			color = ggplot2::guide_legend(title.position = "top", nrow = 2)
		) +
		ggplot2::theme(legend.position = "bottom")

}

