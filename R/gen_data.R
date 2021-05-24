
#' Generate TWFE data
#'
#' @param g1/g2/g3 treatment date for group 1, group 2, group 3. For no treatment, set g = 0.
#' @param panel numeric vector of size 2, start and end years for panel
#' @param te1/te2/te3 treatment effect per group. Will ignore for that group if g = 0.
#' @param te_m1/te_m2/te_m3 treatment effect slope per year
#'
#' @return Dataframe of generated data
#' @export
#' @examples
#' # Homogeneous treatment effect
#' df_hom <- gen_data(panel = c(1990, 2020), g1 = 2000, g2 = 2010, g3 = 0, te1 = 2, te2 = 2, te3 = 0, te_m1 = 0, te_m2 = 0, te_m3 = 0)
#' # Heterogeneous treatment effect
#' df_het <- gen_data(panel = c(1990, 2020), g1 = 2000, g2 = 2010, g3 = 0, te1 = 2, te2 = 1, te3 = 0, te_m1 = 0.05, te_m2 = 0.15, te_m3 = 0)
#'
gen_data <- function(g1 = 2000, g2 = 2010, g3 = 0, panel = c(1990, 2020), te1 = 2, te2 = 2, te3 = 2, te_m1 = 0, te_m2 = 0, te_m3 = 0) {
	tibble::tibble(unit = 1:1000) %>%
		dplyr::mutate(
			state = sample(1:40, dplyr::n(), replace = TRUE),
			unit_fe = stats::rnorm(dplyr::n(), state/5, 1),
			group = stats::runif(dplyr::n()),
			group = dplyr::case_when(
				group < 0.33 ~ "Group 1",
				group < 0.66 ~ "Group 2",
				TRUE ~ "Group 3"
			),
			g = dplyr::case_when(
				group == "Group 1" ~ g1,
				group == "Group 2" ~ g2,
				group == "Group 3" ~ g3,
			)
		) %>%
		tidyr::expand_grid(year = panel[1]:panel[2]) %>%
		# Year FE
		dplyr::group_by(year) %>% dplyr::mutate(year_fe = stats::rnorm(length(year), 0, 1)) %>% dplyr::ungroup() %>%
		dplyr::mutate(
			treat = (year >= g) & (g %in% panel[1]:panel[2]),
			rel_year = dplyr::if_else(g == 0L, Inf, as.numeric(year - g)),
			rel_year_binned = dplyr::case_when(
				rel_year == Inf ~ Inf,
				rel_year <= -6 ~ -6,
				rel_year >= 6 ~ 6,
				TRUE ~ rel_year
			),
			error = stats::rnorm(dplyr::n(), 0, 1),
			# Level Effect
			te =
				(group == "Group 1") * te1 * (year >= g1) +
				(group == "Group 2") * te2 * (year >= g2) +
				(group == "Group 3") * te3 * (year >= g3),
			# dynamic Effect
			te_dynamic =
				(group == "Group 1") * (year >= g1) * te_m1 * (year - g1) +
				(group == "Group 2") * (year >= g2) * te_m2 * (year - g2) +
				(group == "Group 3") * (year >= g3) * te_m3 * (year - g3),
			dep_var = unit_fe + year_fe + te + te_dynamic + error
		)
}
