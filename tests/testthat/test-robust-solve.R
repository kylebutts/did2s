test_that("robust_solve_XtX works with well-conditioned matrices", {
  # Create a well-conditioned design matrix
  set.seed(123)
  n <- 100
  k <- 5
  X <- Matrix::Matrix(rnorm(n * k), n, k)
  Y <- rnorm(n)

  # Test against direct solve (should be identical for well-conditioned case)
  # Convert to base R matrices to avoid Matrix package solve issues in tests
  X_base <- as.matrix(X)
  XtX <- crossprod(X_base)
  XtY <- crossprod(X_base, Y)
  beta_direct <- as.numeric(solve(XtX, XtY))
  beta_robust <- as.numeric(did2s:::robust_solve_XtX(X, Y))

  expect_equal(beta_robust, beta_direct, tolerance = 1e-10)

  # Test fitted values match
  fitted_direct <- as.numeric(X %*% beta_direct)
  fitted_robust <- as.numeric(X %*% beta_robust)

  expect_equal(fitted_robust, fitted_direct, tolerance = 1e-10)
})

test_that("robust_solve_XtX handles rank-deficient matrices", {
  # Create a rank-deficient matrix (column 3 = column 1 + column 2)
  set.seed(123)
  n <- 50
  X1 <- rnorm(n)
  X2 <- rnorm(n)
  X3 <- X1 + X2 # Perfect collinearity
  X <- Matrix::Matrix(cbind(X1, X2, X3))
  Y <- 1 * X1 + 2 * X2 + rnorm(n)

  # Direct solve should fail or give unreliable results
  expect_error(
    solve(Matrix::crossprod(X), Matrix::crossprod(X, Y)),
    class = "simpleError"
  )

  # Robust solve should work (may have 0s for rank-deficient columns)
  beta_robust <- did2s:::robust_solve_XtX(X, Y)
  expect_true(all(is.finite(beta_robust))) # Should all be finite after NA replacement

  # Fitted values should be reasonable
  fitted_robust <- X %*% beta_robust
  expect_true(all(is.finite(fitted_robust)))
  expect_equal(length(fitted_robust), n)

  # The fitted values should be the same whether we use the rank-deficient or full-rank matrix
  # because the redundant column should have coefficient 0
  X_reduced <- X[, 1:2] # Remove rank-deficient column
  beta_reduced <- did2s:::robust_solve_XtX(X_reduced, Y)
  fitted_reduced <- X_reduced %*% beta_reduced

  expect_equal(
    as.numeric(fitted_robust),
    as.numeric(fitted_reduced),
    tolerance = 1e-10
  )
})

test_that("robust_solve_XtX matches fixest::feols fitted values", {
  # Create test data similar to did2s usage
  set.seed(123)
  n <- 200
  data <- data.frame(
    unit = rep(1:50, each = 4),
    time = rep(1:4, 50),
    x1 = rnorm(n),
    x2 = rnorm(n),
    y = rnorm(n)
  )

  # Add perfect collinearity to test rank deficiency
  data$x3 <- data$x1 + 2 * data$x2

  # Create design matrix using fixest::sparse_model_matrix
  # This mimics how the matrix is created in did2s
  feols_fit <- fixest::feols(
    y ~ x1 + x2 + x3 | unit + time,
    data = data,
    warn = FALSE
  )
  X <- fixest::sparse_model_matrix(feols_fit, type = c("rhs", "fixef"))
  Y <- data$y

  # Test our robust solver
  beta_robust <- did2s:::robust_solve_XtX(X, Y)
  fitted_robust <- X %*% beta_robust

  # Compare against fixest fitted values
  fitted_feols <- fitted(feols_fit)

  # Fitted values should be very close (allowing for numerical differences)
  expect_equal(
    as.numeric(fitted_robust),
    as.numeric(fitted_feols),
    tolerance = 1e-8
  )
})

test_that("robust_solve_XtX works with sparse matrices", {
  # Create a sparse design matrix
  set.seed(123)
  n <- 100
  k <- 20

  # Create sparse matrix with many zeros
  X_dense <- matrix(rnorm(n * k), n, k)
  X_dense[abs(X_dense) < 1.5] <- 0 # Make many elements zero
  X <- Matrix::Matrix(X_dense, sparse = TRUE)
  Y <- rnorm(n)

  # Should work with sparse matrices
  beta_robust <- did2s:::robust_solve_XtX(X, Y)
  expect_true(all(is.finite(beta_robust)))

  # Test fitted values
  fitted_robust <- X %*% beta_robust
  expect_equal(length(fitted_robust), n)
  expect_true(all(is.finite(fitted_robust)))
})

test_that("robust_solve_XtX handles edge cases", {
  # Test with single column
  set.seed(123)
  n <- 50
  X <- Matrix::Matrix(rnorm(n), n, 1)
  Y <- rnorm(n)

  beta_robust <- did2s:::robust_solve_XtX(X, Y)
  expect_equal(length(beta_robust), 1)
  expect_true(all(is.finite(beta_robust)))

  # Test with wide matrix (more columns than rows)
  n <- 20
  k <- 25
  X <- Matrix::Matrix(rnorm(n * k), n, k)
  Y <- rnorm(n)

  beta_robust <- did2s:::robust_solve_XtX(X, Y)
  expect_equal(length(beta_robust), k)
  expect_true(all(is.finite(beta_robust)))

  # Fitted values should still work
  fitted <- X %*% beta_robust
  expect_equal(length(fitted), n)
})

test_that("robust_solve_XtX preserves mathematical properties", {
  # Test that the solution satisfies normal equations when possible
  set.seed(123)
  n <- 100
  k <- 5
  X <- Matrix::Matrix(rnorm(n * k), n, k)
  Y <- rnorm(n)

  beta_robust <- did2s:::robust_solve_XtX(X, Y)

  # Check normal equations: X'X beta = X'Y
  # For well-conditioned case, this should hold precisely
  lhs <- Matrix::crossprod(X) %*% beta_robust
  rhs <- Matrix::crossprod(X, Y)

  expect_equal(as.numeric(lhs), as.numeric(rhs), tolerance = 1e-10)
})
