data <- df_hom[df_hom$unit <= 60, ]


plot_event_study_check <- function() {
  out <- event_study(
    data = data,
    yname = "dep_var",
    idname = "unit",
    tname = "year",
    gname = "g",
    verbose = FALSE
  )
  plot_event_study(out)
}

test_that("estimation runs", {
  # Event Study
  expect_error(
    event_study(
      data = data,
      yname = "dep_var",
      idname = "unit",
      tname = "year",
      gname = "g",
      verbose = FALSE
    ),
    NA
  )
  # Weighted
  expect_error(
    event_study(
      data = data,
      yname = "dep_var",
      idname = "unit",
      tname = "year",
      gname = "g",
      weight = "weight",
      verbose = FALSE
    ),
    NA
  )
  # Plot
  expect_error(plot_event_study_check(), NA)
})

test_that("parameter checking works", {
  # g mispecified
  expect_error(
    event_study(
      data = data,
      yname = "dep_var",
      idname = "unit",
      tname = "year",
      gname = "unit"
    ),
    "'unit' must denote which year treatment starts for each group. Untreated observations should have g = 0 or NA."
  )
})
