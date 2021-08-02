# Homogeneous Effects ----------------------------------------------------------

set.seed(1)
df_hom <- gen_data(panel = c(1990, 2020), g1 = 2000, g2 = 2010, g3 = 0, te1 = 2, te2 = 2, te3 = 0, te_m1 = 0, te_m2 = 0, te_m3 = 0)

# Fake weights
df_hom <- df_hom %>% dplyr::group_by(unit) %>% dplyr::mutate(weight = runif(1))

# Heterogeneous Effects --------------------------------------------------------

df_het <- gen_data(panel = c(1990, 2020), g1 = 2000, g2 = 2010, g3 = 0, te1 = 2, te2 = 1, te3 = 0, te_m1 = 0.05, te_m2 = 0.15, te_m3 = 0)


# Castle data ------------------------------------------------------------------

castle <- haven::read_dta("data-raw/castle.dta")
castle$time_til <- ifelse(is.na(castle$time_til), -Inf, castle$time_til)

castle = castle[, c("year", "sid", "l_homicide", "post", "effyear", "time_til")]


# Export Data ------------------------------------------------------------------
usethis::use_data(df_hom, overwrite = TRUE)
usethis::use_data(df_het, overwrite = TRUE)
usethis::use_data(castle, overwrite = TRUE)
