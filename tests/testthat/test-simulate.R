# Helper to create valid covariates object for testing
create_test_covariates <- function() {
  covs <- list(
    make_continuous("x1", icc = 0.3),
    make_continuous("x2", icc = 0.4),
    make_binary("x3", prob = 0.5, icc = 1),
    make_ordinal("x4", probs = c(0.3, 0.4, 0.3), icc = 0.1)
  )
  make_covariates(covariates = covs)
}

# --- Tests for basic functionality ---

test_that("simulate returns data frame with correct structure for single dataset", {
  covs <- create_test_covariates()
  result <- simulate(covs, n_clusters = 5, cluster_size = 10)

  expect_s3_class(result, "simulation")
  expect_s3_class(result, "data.frame")
  expect_true("cluster" %in% colnames(result))
  expect_true("x1" %in% colnames(result))
  expect_true("x2" %in% colnames(result))
  expect_true("x3" %in% colnames(result))
  expect_true("x4" %in% colnames(result))
  expect_equal(nrow(result), 50) # 5 clusters * 10 obs per cluster
})

test_that("simulate returns list of data frames for multiple datasets", {
  covs <- create_test_covariates()
  result <- simulate(covs, n_clusters = 3, cluster_size = 5, n_datasets = 3)

  expect_type(result, "list")
  expect_length(result, 3)

  for (i in seq_along(result)) {
    expect_s3_class(result[[i]], "simulation")
    expect_s3_class(result[[i]], "data.frame")
    expect_true("cluster" %in% colnames(result[[i]]))
    expect_equal(nrow(result[[i]]), 15) # 3 clusters * 5 obs per cluster
  }
})

test_that("simulate uses custom cluster name", {
  covs <- create_test_covariates()
  result <- simulate(covs, n_clusters = 2, cluster_size = 3, cluster_name = "group")

  expect_true("group" %in% colnames(result))
  expect_false("cluster" %in% colnames(result))
})

# --- Tests for cluster_size parameter ---

test_that("simulate works with scalar cluster_size", {
  covs <- create_test_covariates()
  result <- simulate(covs, n_clusters = 4, cluster_size = 8)

  expect_equal(nrow(result), 32) # 4 clusters * 8 obs per cluster
  expect_equal(result$cluster, rep(1:4, each = 8))
})

test_that("simulate works with vector cluster_size", {
  covs <- create_test_covariates()
  result <- simulate(covs, n_clusters = 3, cluster_size = c(5, 10, 15))

  expect_equal(nrow(result), 30) # 5 + 10 + 15
  expect_equal(result$cluster, rep(1:3, times = c(5, 10, 15)))
})

test_that("simulate fails when cluster_size vector length != n_clusters", {
  covs <- create_test_covariates()

  expect_error(
    simulate(covs, n_clusters = 3, cluster_size = c(5, 10)),
    "cluster_size"
  )
})

# --- Tests for seed reproducibility ---

test_that("simulate produces reproducible results with same seed", {
  covs <- create_test_covariates()

  result1 <- simulate(covs, n_clusters = 3, cluster_size = 5, seed = 123)
  result2 <- simulate(covs, n_clusters = 3, cluster_size = 5, seed = 123)

  expect_equal(result1, result2)
})

test_that("simulate produces different results with different seeds", {
  covs <- create_test_covariates()

  result1 <- simulate(covs, n_clusters = 3, cluster_size = 5, seed = 123)
  result2 <- simulate(covs, n_clusters = 3, cluster_size = 5, seed = 456)

  expect_false(isTRUE(all.equal(result1, result2)))
})

test_that("simulate produces same results without seed", {
  covs <- create_test_covariates()

  result1 <- simulate(covs, n_clusters = 3, cluster_size = 5)
  result2 <- simulate(covs, n_clusters = 3, cluster_size = 5)

  # Results should be the same due to seed reset
  expect_true(isTRUE(all.equal(result1, result2)))
})

test_that("simulate with multiple datasets uses incremented seeds", {
  covs <- create_test_covariates()

  result1 <- simulate(covs, n_clusters = 2, cluster_size = 3, n_datasets = 2, seed = 100)
  result2 <- simulate(covs, n_clusters = 2, cluster_size = 3, n_datasets = 2, seed = 100)

  # Same seed should produce identical list of datasets
  expect_equal(result1, result2)

  # Each dataset in the list should be different
  expect_false(isTRUE(all.equal(result1[[1]], result1[[2]])))
})

# --- Tests for DGP attribute ---

test_that("simulate includes DGP attribute with correct components", {
  covs <- create_test_covariates()
  result <- simulate(covs, n_clusters = 3, cluster_size = 5)

  expect_true("DGP" %in% names(attributes(result)))
  dgp <- attr(result, "DGP")
  expect_true("mu" %in% names(dgp))
  expect_true("Sigma_w" %in% names(dgp))
  expect_true("Sigma_b" %in% names(dgp))
})

test_that("simulate with multiple datasets includes DGP attribute in each", {
  covs <- create_test_covariates()
  result <- simulate(covs, n_clusters = 2, cluster_size = 3, n_datasets = 2)

  for (i in seq_along(result)) {
    expect_true("DGP" %in% names(attributes(result[[i]])))
    dgp <- attr(result[[i]], "DGP")
    expect_true("mu" %in% names(dgp))
    expect_true("Sigma_w" %in% names(dgp))
    expect_true("Sigma_b" %in% names(dgp))
  }
})

# --- Tests for input validation ---

test_that("simulate fails with invalid n_clusters", {
  covs <- create_test_covariates()

  expect_error(
    simulate(covs, n_clusters = 0, cluster_size = 5),
    "n_clusters"
  )

  expect_error(
    simulate(covs, n_clusters = -1, cluster_size = 5),
    "n_clusters"
  )

  expect_error(
    simulate(covs, n_clusters = 1.5, cluster_size = 5),
    "n_clusters"
  )
})

test_that("simulate fails with invalid cluster_size", {
  covs <- create_test_covariates()

  expect_error(
    simulate(covs, n_clusters = 3, cluster_size = 0),
    "cluster_size"
  )

  expect_error(
    simulate(covs, n_clusters = 3, cluster_size = -5),
    "cluster_size"
  )

  expect_error(
    simulate(covs, n_clusters = 3, cluster_size = c(5, 0, 10)),
    "cluster_size"
  )
})

test_that("simulate fails with invalid cluster_name", {
  covs <- create_test_covariates()

  expect_error(
    simulate(covs, n_clusters = 3, cluster_size = 5, cluster_name = ""),
    "cluster_name"
  )

  expect_error(
    simulate(covs, n_clusters = 3, cluster_size = 5, cluster_name = 123),
    "cluster_name"
  )

  expect_error(
    simulate(covs, n_clusters = 3, cluster_size = 5, cluster_name = c("a", "b")),
    "cluster_name"
  )
})

test_that("simulate fails with invalid n_datasets", {
  covs <- create_test_covariates()

  expect_error(
    simulate(covs, n_clusters = 3, cluster_size = 5, n_datasets = 0),
    "n_datasets"
  )

  expect_error(
    simulate(covs, n_clusters = 3, cluster_size = 5, n_datasets = -1),
    "n_datasets"
  )

  expect_error(
    simulate(covs, n_clusters = 3, cluster_size = 5, n_datasets = 1.5),
    "n_datasets"
  )
})

# --- Tests for edge cases ---

test_that("simulate works with single cluster", {
  covs <- create_test_covariates()
  result <- simulate(covs, n_clusters = 1, cluster_size = 10)

  expect_equal(nrow(result), 10)
  expect_equal(result$cluster, rep(1, 10))
})

test_that("simulate works with single observation per cluster", {
  covs <- create_test_covariates()
  result <- simulate(covs, n_clusters = 5, cluster_size = 1)

  expect_equal(nrow(result), 5)
  expect_equal(result$cluster, 1:5)
})

test_that("simulate works with single covariate", {
  covs <- make_covariates(covariates = list(make_continuous("x")))
  result <- simulate(covs, n_clusters = 3, cluster_size = 5)

  expect_true("x" %in% colnames(result))
  expect_equal(ncol(result), 2) # cluster + x
})

test_that("simulate works with large number of clusters", {
  covs <- create_test_covariates()
  result <- simulate(covs, n_clusters = 100, cluster_size = 5)

  expect_equal(nrow(result), 500)
  expect_equal(result$cluster, rep(1:100, each = 5))
})

test_that("simulate works with large cluster_size", {
  covs <- create_test_covariates()
  result <- simulate(covs, n_clusters = 2, cluster_size = 1000)

  expect_equal(nrow(result), 2000)
})

# --- Tests for default method ---

test_that("simulate.default prints message", {
  expect_message(
    simulate.default(list()),
    "Default method"
  )
})

# --- Tests for simulate generic ---

test_that("simulate dispatches to correct method", {
  covs <- create_test_covariates()

  # Should dispatch to simulate.covariates
  result <- simulate(covs, n_clusters = 2, cluster_size = 3)
  expect_s3_class(result, "simulation")
})
