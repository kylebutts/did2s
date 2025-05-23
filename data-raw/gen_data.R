# Homogeneous Effects ----------------------------------------------------------

set.seed(1)
df_hom <- gen_data(
  panel = c(1990, 2020),
  g1 = 2000,
  g2 = 2010,
  g3 = 0,
  te1 = 2,
  te2 = 2,
  te3 = 0,
  te_m1 = 0,
  te_m2 = 0,
  te_m3 = 0
)

# Fake weights
data.table::setDT(df_hom)
df_hom[, j = weight := runif(1), by = unit]

# Heterogeneous Effects --------------------------------------------------------

df_het <- gen_data(
  panel = c(1990, 2020),
  g1 = 2000,
  g2 = 2010,
  g3 = 0,
  te1 = 2,
  te2 = 1,
  te3 = 0,
  te_m1 = 0.05,
  te_m2 = 0.15,
  te_m3 = 0
)
data.table::setDT(df_het)

# Castle data ------------------------------------------------------------------

castle <- haven::read_dta("data-raw/castle.dta")
data.table::setDT(castle)
castle[, time_til := data.table::fifelse(is.na(time_til), -Inf, time_til)]

castle = castle[, .(year, sid, l_homicide, post, effyear, time_til)]

df_hom = as.data.frame(df_hom)
df_het = as.data.frame(df_het)
castle = as.data.frame(castle)

# Export Data ------------------------------------------------------------------
usethis::use_data(df_hom, overwrite = TRUE)
usethis::use_data(df_het, overwrite = TRUE)
usethis::use_data(castle, overwrite = TRUE)
