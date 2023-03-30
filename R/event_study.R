#' Estimate event-study coefficients using TWFE and 5 proposed improvements.
#'
#' @description Uses the estimation procedures recommended from Borusyak, Jaravel, Spiess (2021); Callaway and Sant'Anna (2020); Gardner (2021); Roth and Sant'Anna (2021); Sun and Abraham (2020)
#'
#' @param data The dataframe containing all the variables
#' @param yname Variable name for outcome variable
#' @param idname Variable name for unique unit id
#' @param tname Variable name for calendar period
#' @param gname Variable name for unit-specific date of initial treatment
#'   (never-treated should be zero or NA)
#' @param xformla A formula for the covariates to include in the model.
#'   It should be of the form `~ X1 + X2`. Default is NULL.
#' @param weights Variable name for estimation weights. This is used in
#'   estimating Y(0) and also augments treatment effect weights
#' @param estimator Estimator you would like to use. Use "all" to estimate all.
#'   Otherwise see table to know advantages and requirements for each of these.
#'
#' @return `event_study` returns a data.frame of point estimates for each estimator
#'
#' @examples
#' \donttest{
#' out = event_study(
#'   data = did2s::df_het, yname = "dep_var", idname = "unit",
#'   tname = "year", gname = "g", estimator = "all"
#' )
#' plot_event_study(out)
#' }
#' @export
event_study = function(data, yname, idname, gname, tname,
					   xformla = NULL, weights = NULL,
					   estimator = c("all", "TWFE", "did2s", "did", "impute", "sunab",
					   			  "staggered")
			   ){

# Check Parameters -------------------------------------------------------------

	# Select estimator
	estimator <- match.arg(estimator)

	# Display message about estimator's different assumptions
	if(estimator == "all") {
		message("Note these estimators rely on different underlying assumptions. See Table 2 of `https://arxiv.org/abs/2109.05913` for an overview.")
	}

	# Test that gname is in tname or 0/NA for untreated
	if(!all(
			unique(data[[gname]]) %in% c(0, NA, unique(data[[tname]]))
		)) {
		stop(sprintf(
			"'%s' must denote which year treatment starts for each group. Untreated observations should have g = 0 or NA.",
			gname
		))
	}

	# Test that there exists never-treated units
	if(!any(
		unique(data[[gname]]) %in% c(0, NA)
		)) {
		stop(
			"event_study only works when there is a never-treated groups. This will be updated in the future, though with fewer estimators."
		)
	}

	# If `xformla` is included, note
	if(!is.null(xformla)) {
		if(estimator %in% c("all", "staggered")) {
			message(paste0("Warning: `", xformla, "` is ignored for the `staggered` estimator"))
		}
	}

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

	# Format xformla for inclusion
	if(!is.null(xformla)) {
		xformla_null = paste0("0 + ", as.character(xformla)[[2]])
	} else {
		xformla_null = "0"
	}



# initialize empty arguments
tidy_twfe = NULL
tidy_did2s = NULL
tidy_did = NULL
tidy_sunab = NULL
tidy_impute = NULL
tidy_staggered = NULL

# TWFE -------------------------------------------------------------------------
if(estimator %in% c("TWFE", "all")) {
	message("Estimating TWFE Model")

	try({
		twfe_formula = stats::as.formula(
      paste0(
        yname, " ~ 1 + ", xformla_null, " + i(zz000event_time, ref = c(-1, -Inf)) | ", idname, " + ", tname
      )
    )
		est_twfe = fixest::feols(twfe_formula, data = data, warn = F, notes = F)

		# Extract coefficients and standard errors
		tidy_twfe = broom::tidy(est_twfe)

		# Extract zz000event_time
		tidy_twfe = tidy_twfe[grep("zz000event_time::", tidy_twfe$term), ]

		# Make event time into a numeric
		tidy_twfe$term = as.numeric(gsub("zz000event_time::", "", tidy_twfe$term))

		# Subset column
		tidy_twfe = tidy_twfe[, c("term", "estimate", "std.error")]
	})

	if(is.null(tidy_twfe)) warning("TWFE Failed")
}

# did2s ------------------------------------------------------------------------
if(estimator %in% c("did2s", "all")) {
	message("Estimating using Gardner (2021)")

	try({
		did2s_first_stage = stats::as.formula(
      paste0(
        "~ 0 + ", xformla_null, " | ", idname, " + ", tname
      )
    )

		est_did2s = did2s::did2s(data, yname = yname, first_stage = did2s_first_stage, second_stage = ~i(zz000event_time, ref=-Inf), treatment = "zz000treat", cluster_var = idname, verbose = FALSE)

		# Extract coefficients and standard errors
		tidy_did2s = broom::tidy(est_did2s)

		# Extract zz000event_time
		tidy_did2s = tidy_did2s[grep("zz000event_time::", tidy_did2s$term), ]

		# Make event time into a numeric
		tidy_did2s$term = as.numeric(gsub("zz000event_time::", "", tidy_did2s$term))

		# Subset columns
		tidy_did2s = tidy_did2s[, c("term", "estimate", "std.error")]
	})

	if(is.null(tidy_did2s)) warning("Gardner (2021) Failed")
}

# did --------------------------------------------------------------------------
if(estimator %in% c("did", "all")) {
	message("Estimating using Callaway and Sant'Anna (2020)")

	try({
		est_did = did::att_gt(yname = yname, tname = tname, idname = idname, gname = gname, xformla = xformla, data = data)

		est_did = did::aggte(est_did, type = "dynamic", na.rm = TRUE)

		# Extract es coefficients
		tidy_did = broom::tidy(est_did)

		# Subset columns
		tidy_did$term = tidy_did$event.time
		tidy_did = tidy_did[, c("term", "estimate", "std.error")]
	})

	if(is.null(tidy_did)) warning("Callaway and Sant'Anna (2020) Failed")
}

# sunab ------------------------------------------------------------------------
if(estimator %in% c("sunab", "all")) {
	message("Estimating using Sun and Abraham (2020)")

	try({
		# Format xformla for inclusion
		if(is.null(xformla)) {
			sunab_xformla = "1"
		} else {
			sunab_xformla = paste0("1 + ", as.character(xformla)[[2]])
		}

		sunab_formla = stats::as.formula(
      paste0(
        yname, " ~ ", sunab_xformla, " + sunab(", gname, ", zz000event_time, ref.c =0, ref.p = -1) | ", idname, " + ", tname
      )
    )

		est_sunab = fixest::feols(sunab_formla, data = data)

		tidy_sunab = broom::tidy(est_sunab)

    # Extract zz000event_time
		tidy_sunab = tidy_sunab[grep("zz000event_time::", tidy_sunab$term), ]

		# Make event time into a numeric
		tidy_sunab$term = as.numeric(gsub("zz000event_time::", "", tidy_sunab$term))

		# Subset columns
		tidy_sunab = tidy_sunab[, c("term", "estimate", "std.error")]
	})

	if(is.null(tidy_sunab)) warning("Sun and Abraham (2020) Failed")
}

# did_imputation ---------------------------------------------------------------
if(estimator %in% c("impute", "all")) {
	message("Estimating using Borusyak, Jaravel, Spiess (2021)")

	try({
		impute_first_stage = stats::as.formula(
      paste0(
        "~ 1 + ", xformla_null, "+ i(", tname, ") | ", idname
      )
    )

		tidy_impute = didimputation::did_imputation(data,
									 yname = yname, gname = gname, tname = tname, idname = idname,
									 first_stage = impute_first_stage, horizon = TRUE, pretrends = TRUE)

		# Subset columns
		tidy_impute = tidy_impute[, c("term", "estimate", "std.error")]

		tidy_impute = tidy_impute[grep("^(-)?[0-9]+$", tidy_impute$term), ]

		# Make event time into a numeric
		tidy_impute$term = as.numeric(tidy_impute$term)
	})

	if(is.null(tidy_impute)) warning("Borusyak, Jaravel, Spiess (2021) Failed")
}

# staggered --------------------------------------------------------------------
if(estimator %in% c("staggered", "all")) {
	# Waiting for staggered on CRAN
	message("Estimating using Roth and Sant'Anna (2021)")

	try({
		# Make untreated g = Inf
		data_staggered = data

		data_staggered[,gname] = ifelse(
			data_staggered[[gname]] == 0,
			Inf,
			data_staggered[[gname]]
		)

		event_time_staggered = event_time[is.finite(event_time) & event_time != -1]
		event_time_staggered = event_time_staggered[event_time_staggered != min(event_time_staggered) ]

		tidy_staggered = staggered::staggered(
			data_staggered,
			i = idname, t = tname, g = gname, y = yname, estimand = "eventstudy",
			eventTime = event_time_staggered
		)

		# Subset columns
		tidy_staggered$term = tidy_staggered$eventTime
		tidy_staggered$std.error = tidy_staggered$se

		tidy_staggered = tidy_staggered[, c("term", "estimate", "std.error")]
	})

	if(is.null(tidy_staggered)) warning("Roth and Sant'Anna (2021) Failed")
}

# Bind results together --------------------------------------------------------

	out = data.table::rbindlist(list(
		"TWFE" = tidy_twfe,
		"Gardner (2021)" = tidy_did2s,
		"Callaway and Sant'Anna (2020)" = tidy_did,
		"Sun and Abraham (2020)" = tidy_sunab,
		"Roth and Sant'Anna (2021)" = tidy_staggered,
		"Borusyak, Jaravel, Spiess (2021)" = tidy_impute
	), idcol = "estimator")

	return(out)

}


#' Plot results of [event_study()]
#' @param out Output from [event_study()]
#' @param separate Logical. Should the estimators be on separate plots? Default is TRUE.
#' @param horizon Numeric. Vector of length 2. First element is min and
#'   second element is max of event_time to plot
#'
#' @return `plot_event_study` returns a ggplot object that can be fully customized
#'
#' @rdname event_study
#'
#' @importFrom rlang .data
#' @export
plot_event_study = function(out, separate = TRUE, horizon = NULL) {

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


	# position depending on separate
	if(separate) position = "identity" else position = ggplot2::position_dodge(width = 0.5)

	# Subset plot if horizon is specified
	if(!is.null(horizon)) {
		out = out[out$term >= horizon[1] & out$term <= horizon[2], ]
	}

	# max and min of limits
	y_lims = c(min(out$ci_lower), max(out$ci_upper)) * 1.05
	x_lims = c(min(out$term) - 1, max(out$term) + 1)

	ggplot2::ggplot(
      data = out,
      mapping = ggplot2::aes(
        x = .data$term, y = .data$estimate,
        color = .data$estimator,
        ymin = .data$ci_lower, ymax = .data$ci_upper
      )
		) +
		{ if(separate) ggplot2::facet_wrap(~ estimator, scales="free") } +
		ggplot2::geom_point(position = position) +
		ggplot2::geom_errorbar(position = position) +
		ggplot2::geom_vline(xintercept = -0.5, linetype = "dashed") +
		ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
		ggplot2::labs(y = "Point Estimate and 95% Confidence Interval", x = "Event Time", color = "Estimator") +
		{ if(separate) ggplot2::scale_y_continuous(limits = y_lims) } +
		{ if(separate) ggplot2::scale_x_continuous(limits = x_lims) } +
		ggplot2::theme_minimal(base_size = 16) +
		ggplot2::scale_color_manual(values = color_scale) +
		ggplot2::guides(
			color = ggplot2::guide_legend(title.position = "top", nrow = 2)
		) +
		ggplot2::theme(legend.position = "bottom")

}

