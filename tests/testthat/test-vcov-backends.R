test_that("matrix-free cross-product solve matches a direct solve", {
  set.seed(20260917)
  X <- Matrix::rsparsematrix(250, 35, density = 0.15)
  X <- rbind(X, Matrix::Diagonal(35))
  rhs <- matrix(rnorm(35 * 4), nrow = 35)

  direct <- Matrix::solve(Matrix::crossprod(X), rhs)
  iterative <- pcg_crossprod(X, rhs, tol = 1e-12, maxiter = 5000L)
  iterative_values <- iterative
  attr(iterative_values, "convergence") <- NULL

  expect_equal(as.matrix(iterative_values), as.matrix(direct), tolerance = 1e-8)
  expect_true(all(attr(iterative, "convergence")$converged))
})


test_that("fixed-effect nesting in clusters is detected", {
  panel <- expand.grid(unit = seq_len(6L), year = seq_len(4L), replicate = 1:2)
  panel$state <- c("a", "a", "b", "b", "c", "c")[panel$unit]
  panel$changing_cluster <- paste(panel$state, panel$year > 2L, sep = "_")
  panel$x <- stats::rnorm(nrow(panel))
  panel$y <- stats::rnorm(nrow(panel))

  first_stage <- fixest::feols(
    y ~ 0 | unit + year,
    data = panel,
    fixef.keep_names = TRUE,
    fixef.rm = "none"
  )
  expect_identical(
    check_nesting_of_fe_in_clusters(first_stage, panel, "state"),
    c(unit = TRUE, year = FALSE)
  )
  expect_identical(
    check_nesting_of_fe_in_clusters(first_stage, panel, "unit"),
    c(unit = TRUE, year = FALSE)
  )
  expect_identical(
    check_nesting_of_fe_in_clusters(
      first_stage,
      panel,
      "changing_cluster"
    ),
    c(unit = FALSE, year = FALSE)
  )

  combined_stage <- fixest::feols(
    y ~ 0 | unit^year,
    data = panel,
    fixef.keep_names = TRUE,
    fixef.rm = "none"
  )
  expect_identical(
    check_nesting_of_fe_in_clusters(combined_stage, panel, "state"),
    c(`unit^year` = FALSE)
  )
})


test_that("score columns omit nested varying-slope fixed effects", {
  panel <- expand.grid(unit = seq_len(6L), year = seq_len(4L))
  panel$state <- c("a", "a", "b", "b", "c", "c")[panel$unit]
  panel$x <- stats::rnorm(nrow(panel))
  panel$y <- stats::rnorm(nrow(panel))

  first_stage <- fixest::feols(
    y ~ 0 | unit[x] + year,
    data = panel,
    fixef.keep_names = TRUE,
    fixef.rm = "none"
  )
  x1 <- fixest::sparse_model_matrix(
    first_stage,
    data = panel,
    type = c("rhs", "fixef")
  )
  nesting <- check_nesting_of_fe_in_clusters(first_stage, panel, "state")
  score_columns <- get_score_columns(nesting, x1)
  score_names <- colnames(x1)[score_columns]

  expect_false(any(startsWith(score_names, "unit")))
  expect_true(any(startsWith(score_names, "year")))
})


test_that("the first-stage correction is skipped when every FE is nested", {
  set.seed(20260920)
  panel <- expand.grid(unit = seq_len(100L), year = seq_len(8L))
  panel$state <- ceiling(panel$unit / 10L)
  panel$treat <- panel$unit <= 50L & panel$year >= 5L
  panel$y <-
    stats::rnorm(100L)[panel$unit] +
    2 * panel$treat +
    stats::rnorm(nrow(panel))

  result <- did2s(
    data = panel,
    yname = "y",
    first_stage = ~ 0 | unit,
    second_stage = ~treat,
    treatment = "treat",
    cluster_var = "state"
  )
  stages <- did2s_estimate(
    data = panel,
    yname = "y",
    first_stage = ~ 0 | unit,
    second_stage = ~treat,
    treatment = "treat"
  )
  x2 <- fixest::sparse_model_matrix(
    stages$second_stage,
    type = c("rhs", "fixef")
  )
  bread <- stages$second_stage$cov.iid / stages$second_stage$sigma2
  cluster_score <- rowsum(
    as.matrix(x2 * stats::residuals(stages$second_stage)),
    panel$state,
    reorder = FALSE
  )
  cluster_influence <- cluster_score %*% Matrix::t(bread)
  reference_vcov <- Matrix::crossprod(cluster_influence)

  expect_equal(
    as.numeric(vcov(result)),
    as.numeric(reference_vcov),
    tolerance = 1e-12
  )
})


test_that("analytic VCOV backends agree", {
  data(df_hom, package = "did2s")

  sparse <- did2s(
    data = df_hom,
    yname = "dep_var",
    first_stage = ~ 0 | unit + year,
    second_stage = ~ i(rel_year, ref = Inf),
    treatment = "treat",
    cluster_var = "state",
    vcov_method = "sparse"
  )

  matrix_free <- did2s(
    data = df_hom,
    yname = "dep_var",
    first_stage = ~ 0 | unit + year,
    second_stage = ~ i(rel_year, ref = Inf),
    treatment = "treat",
    cluster_var = "state",
    vcov_method = "matrix_free",
    vcov_tol = 1e-12,
    vcov_maxiter = 5000L
  )

  expect_equal(coef(matrix_free), coef(sparse), tolerance = 1e-12)
  expect_equal(vcov(matrix_free), vcov(sparse), tolerance = 1e-7)
})


test_that("cluster-first VCOV matches observation-level influence functions", {
  data(df_hom, package = "did2s")

  result <- did2s(
    data = df_hom,
    yname = "dep_var",
    first_stage = ~ 0 | unit + year,
    second_stage = ~ i(rel_year, ref = Inf),
    treatment = "treat",
    cluster_var = "state",
    vcov_method = "sparse"
  )
  stages <- did2s_estimate(
    data = df_hom,
    yname = "dep_var",
    first_stage = ~ 0 | unit + year,
    second_stage = ~ i(rel_year, ref = Inf),
    treatment = "treat"
  )

  x1 <- fixest::sparse_model_matrix(
    stages$first_stage,
    data = df_hom,
    type = c("rhs", "fixef")
  )
  x2 <- fixest::sparse_model_matrix(
    stages$second_stage,
    type = c("rhs", "fixef")
  )
  rhs <- Matrix::crossprod(x1, x2)
  first_stage_map <- Matrix::t(did2s_first_stage_solve(
    x10 = x1[!df_hom$treat, , drop = FALSE],
    rhs = rhs,
    method = "sparse"
  ))
  bread <- stages$second_stage$cov.iid / stages$second_stage$sigma2
  observation_influence <- bread %*%
    (Matrix::t(x2 * stats::residuals(stages$second_stage)) -
      first_stage_map %*% Matrix::t(x1 * stages$first_u))
  cluster_influence <- rowsum(
    as.matrix(Matrix::t(observation_influence)),
    df_hom$state,
    reorder = FALSE
  )
  reference_vcov <- as.matrix(Matrix::crossprod(cluster_influence))
  dimnames(reference_vcov) <- dimnames(vcov(result))

  expect_equal(
    as.numeric(vcov(result)),
    as.numeric(reference_vcov),
    tolerance = 1e-10
  )
})
