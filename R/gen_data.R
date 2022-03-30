
#' Generate TWFE data
#'
#' @param panel numeric vector of size 2, start and end years for panel
#' @param g1 treatment date for group 1. For no treatment, set g = 0.
#' @param g2 treatment date for group 2. For no treatment, set g = 0.
#' @param g3 treatment date for group 3. For no treatment, set g = 0.
#' @param te1 treatment effect for group 1. Will ignore for that group if g = 0.
#' @param te2 treatment effect for group 1. Will ignore for that group if g = 0.
#' @param te3 treatment effect for group 1. Will ignore for that group if g = 0.
#' @param te_m1 treatment effect slope per year
#' @param te_m2 treatment effect slope per year
#' @param te_m3 treatment effect slope per year
#' @param n number of individuals in sample
#'
#' @return Dataframe of generated data
#' @export
#' @examples
#' # Homogeneous treatment effect
#' df_hom <- gen_data(panel = c(1990, 2020),
#'     g1 = 2000, g2 = 2010, g3 = 0,
#'     te1 = 2, te2 = 2, te3 = 0,
#'     te_m1 = 0, te_m2 = 0, te_m3 = 0)
#' # Heterogeneous treatment effect
#' df_het <- gen_data(panel = c(1990, 2020),
#'     g1 = 2000, g2 = 2010, g3 = 0,
#'     te1 = 2, te2 = 1, te3 = 0,
#'     te_m1 = 0.05, te_m2 = 0.15, te_m3 = 0)
#'
gen_data <- function(g1 = 2000, g2 = 2010, g3 = 0, panel = c(1990, 2020), te1 = 2, te2 = 2, te3 = 2, te_m1 = 0, te_m2 = 0, te_m3 = 0, n = 1000) {

	# CRAN problem
	unit_fe <- state <- group <- g <- unit <- year_fe <- treat <- rel_year <- rel_year_binned <- error <- te <- te_dynamic <- dep_var <- NULL

	df = data.table::data.table(
		unit = 1:n,
		state = sample(1:40, n, replace = TRUE),
		group = stats::runif(n)
	)

	df[,unit_fe := stats::rnorm(n, state/5, 1),
		][, group := data.table::fcase(
			group < 0.33, "Group 1",
			group < 0.66, "Group 2",
			default = "Group 3"
		)][, g := data.table::fcase(
			group == "Group 1", g1,
			group == "Group 2", g2,
			default = g3
		)]

	# Make into panel
	nyear <- panel[2] - panel[1] + 1
	df = df[rep(1:nrow(df), times = nyear), ]

	df[, year := rep(panel[1]:panel[2], each = n)]

	data.table::setorder(df, unit, year)


	# Add year FE
	df[, year_fe := stats::rnorm(1), by = year]

	# Add treatment variables
	df[,
	    treat := (year >= g) & (g %in% panel[1]:panel[2])
	  ][,
		rel_year := year - g
      ][
      	df$g == 0L, rel_year := Inf
  	  ][,
  	  	rel_year_binned := data.table::fcase(
  	  		rel_year <= -6, -6,
  	  		rel_year >= 6, 6,
  	  		rel_year > -6 & rel_year < 6, rel_year
  	  	)
  	  ][,
  	  	error := stats::rnorm(.N)
  	  ]

	# Level Effect
	df[, te :=
		(df$group == "Group 1") * te1 * (df$year >= g1) +
		(df$group == "Group 2") * te2 * (df$year >= g2) +
		(df$group == "Group 3") * te3 * (df$year >= g3)
	][, te_dynamic :=
		(df$group == "Group 1") * (df$year >= g1) * te_m1 * (df$year - g1) +
		(df$group == "Group 2") * (df$year >= g2) * te_m2 * (df$year - g2) +
		(df$group == "Group 3") * (df$year >= g3) * te_m3 * (df$year - g3)
	][, dep_var :=
	  	df$unit_fe + df$year_fe + df$te + df$te_dynamic + df$error
	]

	return(df)
}
