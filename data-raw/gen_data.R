# Homogeneous Effects ----------------------------------------------------------

df_hom <- gen_data(panel = c(1990, 2020), g1 = 2000, g2 = 2010, g3 = 0, te1 = 2, te2 = 2, te3 = 0, te_m1 = 0, te_m2 = 0, te_m3 = 0)


# Heterogeneous Effects --------------------------------------------------------

df_het <- gen_data(panel = c(1990, 2020), g1 = 2000, g2 = 2010, g3 = 0, te1 = 2, te2 = 1, te3 = 0, te_m1 = 0.05, te_m2 = 0.15, te_m3 = 0)


# Autor JOLE -------------------------------------------------------------------

autor <- haven::read_dta("data-raw/autor-jole-2003.dta")

# Export Data ------------------------------------------------------------------
usethis::use_data(df_hom, overwrite = TRUE)
usethis::use_data(df_het, overwrite = TRUE)
usethis::use_data(autor, overwrite = TRUE)
