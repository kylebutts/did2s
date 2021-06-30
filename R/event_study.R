#' Estimate event-study coefficients using TWFE and 5 proposed improvements.
#'
#' @description Uses the estimation procedures recommended from Borusyak, Jaravell, Spiess (2021); Callaway and Sant'Anna (2020); Gardner (2021); Roth and Sant'Anna (2021); Sun and Abraham (2020)
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
#' @section Examples:
event_study = function(data, yname, idname, gname, tname, xformla = NULL, horizon = NULL, weights = NULL){

# Setup ------------------------------------------------------------------------

	# Treat
	data$zz000treat = 1 * (data[[tname]] >= data[[gname]]) * (data[[gname]] > 0)
	data[is.na(data$zz000treat), "zz000treat"] = 0

	# Set g to zero if NA
	data[is.na(data[[gname]]), gname] = 0

	# Create event time
	data = data %>% dplyr::mutate(
		zz000event_time = dplyr::if_else(
			is.na(!!rlang::sym(gname)) | !!rlang::sym(gname) == 0,
			NA_real_,
			as.numeric(!!rlang::sym(tname) - !!rlang::sym(gname))
		),
		zz000event_time = dplyr::if_else(
			is.na(zz000event_time), -Inf, zz000event_time
		)
	)


	event_time = unique(data$zz000event_time)
	event_time = event_time[!is.na(event_time)]
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

	twfe_formula = stats::as.formula(glue::glue("{yname} ~ {xformla_null} + i(zz000event_time) | {idname} + {tname}"))
	est_twfe = fixest::feols(twfe_formula, data = data)

	tidy_twfe = broom::tidy(est_twfe) %>%
		dplyr::filter(stringr::str_detect(term, "zz000event_time::")) %>%
		dplyr::mutate(
			term = stringr::str_replace(term, "zz000event_time::", ""),
			term = as.numeric(term)
		) %>%
		dplyr::select(term, estimate, std.error) %>%
		dplyr::mutate(estimator = "TWFE")



# did2s ------------------------------------------------------------------------

	cli::cli_text("Estimating using Gardner (2021)")

	did2s_first_stage = stats::as.formula(glue::glue("~ 0 + {xformla_null} | {idname} + {tname}"))

	est_did2s = did2s::did2s(data, yname = yname, first_stage = did2s_first_stage, second_stage = ~i(zz000event_time, ref=-Inf), treatment = "zz000treat", cluster_var = idname, verbose = FALSE)

	tidy_did2s = broom::tidy(est_did2s) %>%
		dplyr::filter(stringr::str_detect(term, "zz000event_time::")) %>%
		dplyr::mutate(
			term = stringr::str_replace(term, "zz000event_time::", ""),
			term = as.numeric(term)
		) %>%
		dplyr::select(term, estimate, std.error) %>%
		dplyr::mutate(estimator = "Gardner (2021)")

# did --------------------------------------------------------------------------

	cli::cli_text("Estimating using Callaway and Sant'Anna (2020)")

	est_did = did::att_gt(yname = yname, tname = tname, idname = idname, gname = gname, xformla = xformla, data = data) %>%
		did::aggte(type = "dynamic", na.rm = TRUE)

	tidy_did = broom::tidy(est_did) %>%
		dplyr::select(term = event.time, estimate, std.error) %>%
		dplyr::mutate(estimator = "Callaway and Sant'Anna (2020)")

# sunab ------------------------------------------------------------------------

	cli::cli_text("Estimating using Sun and Abraham (2020)")

	# Format xformla for inclusion
	if(is.null(xformla)) {
		sunab_xformla = "1"
	} else {
		sunab_xformla = paste0("1 + ", as.character(xformla)[[2]])
	}

	sunab_formla = stats::as.formula(glue::glue("{yname} ~ {sunab_xformla} + sunab({gname}, {tname})"))

	est_sunab = fixest::feols(sunab_formla, data = data)

	tidy_sunab = broom::tidy(est_sunab) %>%
		filter(stringr::str_detect(term, glue::glue("{tname}::"))) %>%
		dplyr::mutate(
			term = stringr::str_replace(term, glue::glue("{tname}::"), ""),
			term = as.numeric(term)
		) %>%
		dplyr::select(term, estimate, std.error) %>%
		dplyr::mutate(estimator = "Sun and Abraham (2020)")


# did_imputation ---------------------------------------------------------------

	cli::cli_text("Estimating using Borusyak, Jaravell, Spiess (2021)")

	impute_first_stage = stats::as.formula(glue::glue("~ {xformla_null} | {idname} + {tname}"))

	tidy_impute = did_imputation(data, yname = yname, gname = gname, tname = tname, idname = idname, first_stage = impute_first_stage) %>%
		dplyr::select(term, estimate, std.error) %>%
		dplyr::mutate(estimator = "Borusyak, Jaravell, Spiess (2021)")

# staggered --------------------------------------------------------------------

	cli::cli_text("Estimatng using Roth and Sant'Anna (2021)")

	tidy_staggered <- staggered::staggered(
			data %>% dplyr::mutate(!!rlang::sym(gname) := if_else(!!rlang::sym(gname) == 0, Inf, !!rlang::sym(gname))),
			i = idname, t = tname, g = gname, y = yname, estimand = "eventstudy",
			eventTime = event_time[is.finite(event_time) & event_time != -1]
		) %>%
		dplyr::select(term = eventTime, estimate, std.error = se) %>%
		dplyr::mutate(estimator = "Roth and Sant'Anna (2021)")


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

	source("https://raw.githubusercontent.com/kylebutts/templates/master/ggplot_theme/theme_kyle.R")

	out = out %>%
		dplyr::mutate(
			ci_lower = estimate - 1.96 * std.error,
			ci_upper = estimate + 1.96 * std.error,
			estimator = factor(estimator, levels = c("TWFE", "Borusyak, Jaravell, Spiess (2021)", "Callaway and Sant'Anna (2020)", "Gardner (2021)", "Roth and Sant'Anna (2021)",  "Sun and Abraham (2020)"))
		)

	if(seperate) position = "identity" else position = "dodge"

	if(!is.null(horizon)) {
		out = out %>%
			dplyr::filter(term >= horizon[1] & term <= horizon[2])
	}

	y_lims = c(floor(min(out$estimate)), ceiling(max(out$estimate)))
	x_lims = c(min(out$term) - 1, max(out$term) + 1)

	ggplot2::ggplot(out, aes(x = term, y = estimate, color = estimator, ymin = ci_lower, ymax = ci_upper)) +
		{ if(seperate) ggplot2::facet_wrap(~ estimator, scales="free") } +
		ggplot2::geom_point(position = position) +
		ggplot2::geom_errorbar(position = position) +
		ggplot2::geom_vline(xintercept = -0.5, linetype = "dashed") +
		ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
		ggplot2::labs(y = "Point Estimate and 95% Confidence Interval", x = "Event Time", color = "Estimator") +
		#scale_y_continuous(limits = y_lims) +
		#scale_x_continuous(limits = x_lims) +
		ggplot2::theme_kyle(base_size = 16) +
		ggplot2::scale_color_manual(values = c("TWFE" = "#374E55", "Gardner (2021)" = "#DF8F44", "Callaway and Sant'Anna (2020)" = "#00A1D5", "Sun and Abraham (2020)" = "#B24745", "Roth and Sant'Anna (2021)" = "#79AF97", "Borusyak, Jaravell, Spiess (2021)" = "#6A6599")) +
		ggplot2::guides(
			color = ggplot2::guide_legend(title.position = "top", nrow = 2)
		) +
		ggplot2::theme(legend.position = "bottom")

}

