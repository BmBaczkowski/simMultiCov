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

test_that("make_covariates creates valid object with list of covariates", {
  covs <- create_test_covariates()
  result <- make_covariates(
    covariates = covs
  )

  expect_s3_class(result, "covariates")
})

test_that("make_covariates creates valid object including correlations", {
  covs <- create_test_covariates()
  corrs <- create_test_correlations()

  result <- make_covariates(
    covariates = covs,
    correlations = corrs
  )

  expect_s3_class(result, "covariates")
})


# --- Tests for covariates validation ---

test_that("make_covariates fails when covariates is not a list", {
  expect_error(
    make_covariates(covariates = make_continuous("x")),
    "covariates"
  )
})

test_that("make_covariates fails when covariates list is empty", {
  expect_error(
    make_covariates(covariates = list()),
    "covariates"
  )
})

test_that("make_covariates fails when covariates contains non-multilevel_covariate objects", {
  expect_error(
    make_covariates(covariates = list("not_a_covariate")),
    "covariates"
  )
})

test_that("make_covariates fails when covariates are duplicated (same object)", {
  cov <- make_continuous("x")

  expect_error(
    make_covariates(covariates = list(cov, cov)),
    "covariates"
  )
})

test_that("make_covariates fails with duplicate covariate names", {
  covs <- list(
    make_continuous("age", icc = 0.3),
    make_continuous("age")
  )

  expect_error(
    make_covariates(covariates = covs),
    "Duplicate covariate names"
  )
})

test_that("make_covariates fails when covariates contains missing values", {
  covs <- list(make_continuous("x"), NULL)

  expect_error(
    make_covariates(covariates = covs),
    "covariates"
  )
})

# --- Tests for correlations validation ---

test_that("make_covariates fails when correlations is not a list", {
  covs <- create_test_covariates()

  expect_error(
    make_covariates(
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
    covariates = covs
  )

  expect_s3_class(result, "covariates")
})

test_that("make_covariates works with empty correlations list", {
  covs <- create_test_covariates()
  result <- make_covariates(
    covariates = covs,
    correlations = list()
  )

  expect_s3_class(result, "covariates")
})

test_that("make_covariates fails with a list of duplicated correlations", {
  covs <- create_test_covariates()
  
  expect_error(
    make_covariates(
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
