
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
gen_data <- function(g1 = 2000, g2 = 2010, g3 = 0, panel = c(1990, 2020), te1 = 2, te2 = 2, te3 = 2, te_m1 = 0, te_m2 = 0, te_m3 = 0) {

n = 1000

df = tibble::tibble(unit = 1:n)

df$state = sample(1:40, n, replace = TRUE)
df$unit_fe = stats::rnorm(n, df$state/5, 1)

df$group = stats::runif(n)
df$group = dplyr::case_when(
				df$group < 0.33 ~ "Group 1",
				df$group < 0.66 ~ "Group 2",
				TRUE ~ "Group 3"
			)
df$g = dplyr::case_when(
				df$group == "Group 1" ~ g1,
				df$group == "Group 2" ~ g2,
				df$group == "Group 3" ~ g3,
			)

# Make into panel
nyear <- panel[2] - panel[1] + 1
df = df[rep(1:nrow(df), times = nyear), ]
df$year <- rep(panel[1]:panel[2], each = n)

# Add year FE
year_fe = tibble::tibble(year = panel[1]:panel[2])
year_fe$year_fe = stats::rnorm(length(year_fe$year), 0, 1)

# Join with year_fe tibble
df = dplyr::left_join(df, year_fe, by = "year")

df$treat = (df$year >= df$g) & (df$g %in% panel[1]:panel[2])

df$rel_year = dplyr::if_else(df$g == 0L, Inf, as.numeric(df$year - df$g))

df$rel_year_binned = dplyr::case_when(
	df$rel_year == Inf ~ Inf,
	df$rel_year <= -6 ~ -6,
	df$rel_year >= 6 ~ 6,
	TRUE ~ df$rel_year
)

df$error = stats::rnorm(nrow(df), 0, 1)

# Level Effect
df$te =
	(df$group == "Group 1") * te1 * (df$year >= g1) +
	(df$group == "Group 2") * te2 * (df$year >= g2) +
	(df$group == "Group 3") * te3 * (df$year >= g3)
# dynamic Effect
df$te_dynamic =
	(df$group == "Group 1") * (df$year >= g1) * te_m1 * (df$year - g1) +
	(df$group == "Group 2") * (df$year >= g2) * te_m2 * (df$year - g2) +
	(df$group == "Group 3") * (df$year >= g3) * te_m3 * (df$year - g3)

df$dep_var = df$unit_fe + df$year_fe + df$te + df$te_dynamic + df$error

return(df)
}
