## test-did2s.R ----------------------------------------------------------------
## Kyle Butts, CU Boulder Economics
##
## Test did2s function

data(df_hom)
library(haven)
castle = haven::read_dta("https://github.com/scunning1975/mixtape/raw/master/castle.dta")

# Add random 0/1 variable

df_hom$temp = as.numeric(runif(nrow(df_hom)) > 0.5)

test_that("estimation runs", {
	# Static
	expect_error(did2s(
		data = df_hom, yname = "dep_var", first_stage = ~ 0 | unit + year,
		second_stage = ~ i(treat, ref=FALSE),
		treatment = "treat", cluster_var = "state"),
	NA)
	# Event Study
	expect_error(did2s(
		data = df_hom, yname = "dep_var", first_stage = ~ 0 | unit + year,
		second_stage = ~ i(rel_year, ref=Inf),
		treatment = "treat", cluster_var = "state"),
		NA)
	# Weighted
	expect_error(did2s(
		data = df_hom, yname = "dep_var", first_stage = ~ 0 | unit + year,
		second_stage = ~ i(treat, ref=FALSE), weight = "weight",
		treatment = "treat", cluster_var = "state"),
		NA)
	# Castle data
	expect_error(did2s(
		data = castle,
		yname = "l_homicide",
		first_stage = ~ 0 | sid + year,
		second_stage = ~ i(post, ref=0),
		treatment = "post", cluster_var = "state", weights = "popwt"),
		NA)
	# Interacted FEs
	expect_error(did2s(
		data = df_hom, yname = "dep_var", first_stage = ~ 0 | unit + temp^year,
		second_stage = ~ i(treat, ref=FALSE), weight = "weight",
		treatment = "treat", cluster_var = "state"),
NA)
})

test_that("estimates match previous runs", {
  static = did2s(
    data = df_hom, yname = "dep_var", first_stage = ~ 0 | unit + year,
    second_stage = ~ i(treat, ref=FALSE),
    treatment = "treat", cluster_var = "state"
  )

  expect_true(abs(coef(static) - 2.004653) < 0.0001, )
  expect_true(abs(se(static) - 0.02023667) < 0.0001, )
})


test_that("parameter checking works", {
	# Treatment not 0/1 or T/F
	expect_error(did2s(
		data = df_hom, yname = "dep_var", first_stage = ~ 0 | unit + year,
		second_stage = ~ i(treat, ref=FALSE), weight = "weight",
		treatment = "state", cluster_var = "state"),
		"'state' must be a 0/1 or T/F variable indicating which observations are untreated/not-yet-treated.")
})
