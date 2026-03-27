# Helpers to create valid inputs
create_test_covariates <- function() {
  list(
    make_continuous("x1", icc = 0.3),
    make_continuous("x2", icc = 0.4),
    make_binary("x3", prob = 0.5, icc = 1),
    make_ordinal("x4", probs = c(0.3, 0.4, 0.3), icc = 0.1)
  )
}

create_test_correlations <- function() {
    list(define_correlation("x1", "x2", corr_within = 0.2, corr_between = 0.4))
}

# --- Tests for valid inputs ---

test_that("make_covariates creates valid object with minimum valid inputs", {
  covs <- create_test_covariates()
  result <- make_covariates(
    # n_clusters = 5,
    # cluster_size = 10,
    covariates = covs
  )

  expect_s3_class(result, "covariates")
})

test_that("make_covariates creates valid object with all parameters", {
  covs <- create_test_covariates()
  corrs <- create_test_correlations()

  result <- make_covariates(
    # n_clusters = 5,
    # cluster_size = 10,
    # cluster_name = "school",
    covariates = covs,
    correlations = corrs
  )

  expect_s3_class(result, "covariates")
})

test_that("make_covariates accepts scalar cluster_size and replicates for all clusters", {
  covs <- list(make_continuous("x"))
  result <- make_covariates(
    n_clusters = 3,
    cluster_size = 10,
    covariates = covs
  )

  expect_s3_class(result, "covariates")
})

test_that("make_covariates accepts vector cluster_size of length n_clusters", {
  covs <- list(make_continuous("x"))
  result <- make_covariates(
    n_clusters = 3,
    cluster_size = c(10, 15, 20),
    covariates = covs
  )

  expect_s3_class(result, "covariates")
})

test_that("make_covariates handles single cluster", {
  covs <- list(make_continuous("x"))
  result <- make_covariates(
    n_clusters = 1,
    cluster_size = 50,
    covariates = covs
  )

  expect_s3_class(result, "covariates")
})

# --- Tests for n_clusters validation ---

test_that("make_covariates fails when n_clusters is numeric with a fractional part", {
  covs <- list(make_continuous("x"))

  expect_error(
    make_covariates(n_clusters = 5.5, cluster_size = 10, covariates = covs),
    "n_clusters"
  )
})

test_that("make_covariates accepts numeric n_clusters with no fractional part", {
  covs <- list(make_continuous("x"))

  result <- make_covariates(
    n_clusters = 5.0,
    cluster_size = 10,
    covariates = covs
  )

  expect_s3_class(result, "covariates")
})

test_that("make_covariates fails when n_clusters is less than 1", {
  covs <- list(make_continuous("x"))

  expect_error(
    make_covariates(n_clusters = 0, cluster_size = 10, covariates = covs),
    "n_clusters"
  )
  expect_error(
    make_covariates(n_clusters = -1, cluster_size = 10, covariates = covs),
    "n_clusters"
  )
})

test_that("make_covariates fails when n_clusters is not numeric", {
  covs <- list(make_continuous("x"))

  expect_error(
    make_covariates(n_clusters = "5", cluster_size = 10, covariates = covs),
    "n_clusters"
  )
})

test_that("make_covariates fails when n_clusters is NA", {
  covs <- list(make_continuous("x"))

  expect_error(
    make_covariates(n_clusters = NA, cluster_size = 10, covariates = covs),
    "n_clusters"
  )
})

# --- Tests for cluster_size validation ---

test_that("make_covariates fails when cluster_size is not a scalar or length n_clusters", {
  covs <- list(make_continuous("x"))

  expect_error(
    make_covariates(n_clusters = 3, cluster_size = c(10, 20), covariates = covs),
    "cluster_size"
  )
})

test_that("make_covariates fails when cluster_size contains missing values", {
  covs <- list(make_continuous("x"))

  expect_error(
    make_covariates(n_clusters = 3, cluster_size = c(10, NA, 20), covariates = covs),
    "cluster_size"
  )
})

test_that("make_covariates fails when cluster_size is less than 1", {
  covs <- list(make_continuous("x"))

  expect_error(
    make_covariates(n_clusters = 3, cluster_size = c(0, 10, 20), covariates = covs),
    "cluster_size"
  )
})

test_that("make_covariates fails when cluster_size is not integer", {
  covs <- list(make_continuous("x"))

  expect_error(
    make_covariates(n_clusters = 3, cluster_size = c("10", "20", "30"), covariates = covs),
    "cluster_size"
  )
})

test_that("make_covariates fails when cluster_size is numeric", {
  covs <- create_test_covariates()

  expect_error(
    result <- make_covariates(
      n_clusters = 3,
      cluster_size = 10.5,
      covariates = covs
    ),
    "cluster_size"
  )

})

# --- Tests for cluster_name validation ---

test_that("make_covariates fails when cluster_name is not a string", {
  covs <- list(make_continuous("x"))

  expect_error(
    make_covariates(n_clusters = 3, cluster_size = 10, covariates = covs, cluster_name = 123),
    "cluster_name"
  )
})

test_that("make_covariates fails when cluster_name is empty string", {
  covs <- list(make_continuous("x"))

  expect_error(
    make_covariates(n_clusters = 3, cluster_size = 10, covariates = covs, cluster_name = ""),
    "cluster_name"
  )
})

test_that("make_covariates fails when cluster_name is not length 1", {
  covs <- list(make_continuous("x"))

  expect_error(
    make_covariates(n_clusters = 3, cluster_size = 10, covariates = covs, cluster_name = c("a", "b")),
    "cluster_name"
  )
})

# --- Tests for covariates validation ---

test_that("make_covariates fails when covariates is not a list", {
  expect_error(
    make_covariates(n_clusters = 3, cluster_size = 10, covariates = make_continuous("x")),
    "covariates"
  )
})

test_that("make_covariates fails when covariates list is empty", {
  expect_error(
    make_covariates(n_clusters = 3, cluster_size = 10, covariates = list()),
    "covariates"
  )
})

test_that("make_covariates fails when covariates contains non-multilevel_covariate objects", {
  expect_error(
    make_covariates(n_clusters = 3, cluster_size = 10, covariates = list("not_a_covariate")),
    "covariates"
  )
})

test_that("make_covariates fails when covariates are duplicated (same object)", {
  cov <- make_continuous("x")

  expect_error(
    make_covariates(n_clusters = 3, cluster_size = 10, covariates = list(cov, cov)),
    "covariates"
  )
})

test_that("make_covariates fails with duplicate covariate names", {
  covs <- list(
    make_continuous("age", icc = 0.3),
    make_continuous("age")
  )

  expect_error(
    make_covariates(n_clusters = 3, cluster_size = 10, covariates = covs),
    "Duplicate covariate names"
  )
})

test_that("make_covariates fails when covariates contains missing values", {
  covs <- list(make_continuous("x"), NULL)

  expect_error(
    make_covariates(n_clusters = 3, cluster_size = 10, covariates = covs),
    "covariates"
  )
})

# --- Tests for correlations validation ---

test_that("make_covariates fails when correlations is not a list", {
  covs <- create_test_covariates()

  expect_error(
    make_covariates(
      n_clusters = 3, 
      cluster_size = 10, 
      covariates = covs, 
      correlations = define_correlation("age", "gender", corr_between = 0.2)
    ),
    "correlations"
  )
})

test_that("make_covariates fails when correlations contains missing values", {
  covs <- create_test_covariates()
  corrs <- list(define_correlation("age", "treatment", corr_within = 0.2), NULL)

  expect_error(
    make_covariates(
      n_clusters = 3,
      cluster_size = 10,
      covariates = covs,
      correlations = corrs
    ),
    "correlations"
  )
})

# --- Edge cases ---

test_that("make_covariates works with single covariate", {
  covs <- list(make_continuous("x"))
  result <- make_covariates(
    n_clusters = 5,
    cluster_size = 10,
    covariates = covs
  )

  expect_s3_class(result, "covariates")
})

test_that("make_covariates works with empty correlations list", {
  covs <- create_test_covariates()
  result <- make_covariates(
    n_clusters = 5,
    cluster_size = 10,
    covariates = covs,
    correlations = list()
  )

  expect_s3_class(result, "covariates")
})

test_that("make_covariates fails with a list of duplicated correlations", {
  covs <- create_test_covariates()
  
  expect_error(
    make_covariates(
      n_clusters = 5,
      cluster_size = 10,
      covariates = covs,
      correlations = list(
        define_correlation("x1", "x2", corr_within = 0.2),
        define_correlation("x2", "x1", corr_between = 0.2)
      )
    ),
    "correlations"
  )
})

test_that("make_covariates fails when correlations include unknown variable", {
  covs <- create_test_covariates()
  
  expect_error(
    make_covariates(
      n_clusters = 5,
      cluster_size = 10,
      covariates = covs,
      correlations = list(
        define_correlation("x1", "x2", corr_within = 0.2),
        define_correlation("x2", "x99", corr_between = 0.2)
      )
    ),
    "are not defined in covariates"
  )
})

test_that("make_covariates warns when correlations include binary or ordinal", {
  covs <- create_test_covariates()
  corrs <- list(
    define_correlation("x1", "x2", corr_within = 0.2, corr_between = 0.4),
    define_correlation("x1", "x3", corr_within = 0.7, corr_between = -0.4)
  )

  expect_warning(
    result <- make_covariates(
      n_clusters = 5,
      cluster_size = 10,
      cluster_name = "school",
      covariates = covs,
      correlations = corrs
    ),
    "latent space"
  )
  
})

test_that("make_covariates warns with non PSD correlations", {
  covs <-  list(
    make_continuous("x1", icc = 0.3),
    make_continuous("x2", icc = 0.4),
    make_continuous("x3", icc = 0)
  )
  
  expect_warning(
    make_covariates(
      n_clusters = 5,
      cluster_size = 10,
      covariates = covs,
      correlations = list(
        define_correlation("x1", "x2", corr_between = .9, corr_within = 0),
        define_correlation("x1", "x3", corr_between = .9),
        define_correlation("x2", "x3", corr_between = -.9, corr_within = 0)
      )
    ),
    "Replacing with nearest positive-definite matrix"
  )
})
