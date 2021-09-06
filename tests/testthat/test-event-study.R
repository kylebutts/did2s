plot_event_study_check <- function(){
	out <- event_study(
		data = df_hom, yname = "dep_var", idname = "unit",
		tname = "year", gname = "g")
	plot_event_study(out)
}

test_that("estimation runs", {
	# Event Study
	expect_error(event_study(
		data = df_hom, yname = "dep_var", idname = "unit",
		tname = "year", gname = "g"),
		NA)
	# Weighted
	expect_error(event_study(
		data = df_hom, yname = "dep_var", idname = "unit",
		tname = "year", gname = "g", weight = "weight"),
		NA)
	# Plot
	expect_error(plot_event_study_check(),
		NA)
})

test_that("parameter checking works", {
	# g mispecified
	expect_error(event_study(
		data = df_hom, yname = "dep_var", idname = "unit",
		tname = "year", gname = "unit"),
		"'unit' must denote which year treatment starts for each group. Untreated observations should have g = 0 or NA.")

})
