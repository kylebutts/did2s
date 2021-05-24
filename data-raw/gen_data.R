library(tidyverse)

# Gen data ---------------------------------------------------------------------

gen_data <- function(input) {

	df <- tibble(unit = 1:1000) %>%
		mutate(
			state = sample(1:40, n(), replace = TRUE),
			unit_fe = rnorm(n(), state/5, 1),
			group = runif(n()),
			group = case_when(
				group < 0.33 ~ "Group 1",
				group < 0.66 ~ "Group 2",
				TRUE ~ "Group 3"
			),
			g = case_when(
				group == "Group 1" ~ input$g1,
				group == "Group 2" ~ input$g2,
				group == "Group 3" ~ input$g3,
			)
		) %>%
		expand_grid(year = input$panel[1]:input$panel[2]) %>%
		# Year FE
		group_by(year) %>% mutate(year_fe = rnorm(length(year), 0, 1)) %>% ungroup() %>%
		mutate(
			treat = (year >= g) & (g %in% input$panel[1]:input$panel[2]),
			rel_year = if_else(g == 0L, Inf, as.numeric(year - g)),
			rel_year_binned = case_when(
				rel_year == Inf ~ Inf,
				rel_year <= -6 ~ -6,
				rel_year >= 6 ~ 6,
				TRUE ~ rel_year
			),
			error = rnorm(n(), 0, 1),
			# Level Effect
			te =
				(group == "Group 1") * input$te1 * (year >= input$g1) +
				(group == "Group 2") * input$te2 * (year >= input$g2) +
				(group == "Group 3") * input$te3 * (year >= input$g3),
			# dynamic Effect
			te_dynamic =
				(group == "Group 1") * (year >= input$g1) * input$te_m1 * (year - input$g1) +
				(group == "Group 2") * (year >= input$g2) * input$te_m2 * (year - input$g2) +
				(group == "Group 3") * (year >= input$g3) * input$te_m3 * (year - input$g3),
			dep_var = unit_fe + year_fe + te + te_dynamic + error
		)
}


# Homogeneous Effects ----------------------------------------------------------

input <- list(
	panel = c(1990, 2020),
	g1 = 2005,
	g2 = 2010,
	g3 = 0,
	te1 = 2,
	te2 = 2,
	te3 = 0,
	te_m1 = 0,
	te_m2 = 0,
	te_m3 = 0
)

df_hom <- gen_data(input)



# Heterogeneous Effects --------------------------------------------------------

input <- list(
	panel = c(1990, 2020),
	g1 = 2005,
	g2 = 2010,
	g3 = 0,
	te1 = 2,
	te2 = 2,
	te3 = 0,
	te_m1 = 0.05,
	te_m2 = 0.15,
	te_m3 = 0

)

df_het <- gen_data(input)


# Autor JOLE -------------------------------------------------------------------

autor <- haven::read_dta("data-raw/autor-jole-2003.dta")

# Export Data ------------------------------------------------------------------
usethis::use_data(df_hom, overwrite = TRUE)
usethis::use_data(df_het, overwrite = TRUE)
usethis::use_data(autor, overwrite = TRUE)
