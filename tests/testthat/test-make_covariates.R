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


test_that("make_covariates extracts probs correctly for binary covariates", {
  covs <- list(
    make_binary("treatment", prob = 0.3, icc = 0.1),
    make_binary("gender", prob = 0.7, icc = 0.2)
  )
  
  result <- make_covariates(covariates = covs)
  
  # Verify probs is a list with correct structure
  expect_type(result$probs, "list")
  expect_named(result$probs, c("treatment", "gender"))
  
  # Verify probability values are correct
  expect_equal(result$probs$treatment, list(A = 0.3, B = 0.7))
  expect_equal(result$probs$gender, list(A = 0.7, B = 0.3))
})


test_that("make_covariates extracts probs correctly for ordinal covariates", {
  covs <- list(
    make_ordinal("satisfaction", probs = c(0.2, 0.3, 0.5), icc = 0.1),
    make_ordinal("education", probs = c(0.1, 0.2, 0.3, 0.4), icc = 0.2)
  )
  
  result <- make_covariates(covariates = covs)
  
  # Verify probs is a list with correct structure
  expect_type(result$probs, "list")
  expect_named(result$probs, c("satisfaction", "education"))
  
  # Verify probability values are correct
  expect_equal(result$probs$satisfaction, list(A = 0.2, B = 0.3, C = 0.5))
  expect_equal(result$probs$education, list(A = 0.1, B = 0.2, C = 0.3, D = 0.4))
})


test_that("make_covariates handles mixed covariate types for probs", {
  covs <- list(
    make_continuous("age", mean = 50, total_var = 100, icc = 0.2),
    make_binary("treatment", prob = 0.5, icc = 0.1),
    make_ordinal("satisfaction", probs = c(0.3, 0.4, 0.3), icc = 0.15)
  )
  
  result <- make_covariates(covariates = covs)
  
  # Verify probs structure
  expect_type(result$probs, "list")
  expect_named(result$probs, c("age", "treatment", "satisfaction"))
  
  # Continuous covariate should have NULL probs
  expect_null(result$probs$age)
  
  # Binary covariate should have named list with 2 elements
  expect_equal(result$probs$treatment, list(A = 0.5, B = 0.5))
  
  # Ordinal covariate should have named list with 3 elements
  expect_equal(result$probs$satisfaction, list(A = 0.3, B = 0.4, C = 0.3))
})


test_that("make_covariates output contains correct values for all fields", {
  covs <- list(
    make_continuous("x1", mean = 5, total_var = 10, icc = 0.3),
    make_binary("x2", prob = 0.6, icc = 0.2)
  )
  
  result <- make_covariates(covariates = covs)
  
  # Verify basic fields
  expect_equal(result$n_covariates, 2)
  expect_equal(result$names, c("x1", "x2"))
  expect_equal(result$types, c("continuous", "binary"))
  
  # Verify icc values
  expect_equal(result$icc, c(x1 = 0.3, x2 = 0.2))
  
  # Verify total_var values
  expect_equal(result$total_var, c(x1 = 10, x2 = 1))
  
  # Verify mean values
  expect_equal(result$mean, c(x1 = 5, x2 = 0))
  
  # Verify probs values
  expect_null(result$probs$x1)
  expect_equal(result$probs$x2, list(A = 0.6, B = 0.4))
})


test_that("make_covariates computes thresholds correctly", {
  covs <- list(
    make_binary("treatment", prob = 0.3, icc = 0.1, labels = c("Control", "Treatment")),
    make_ordinal("satisfaction", probs = c(0.2, 0.3, 0.5), icc = 0.15, 
                 labels = c("Low", "Medium", "High"))
  )
  
  result <- make_covariates(covariates = covs)
  
  # Verify thresholds exist
  expect_type(result$.thresholds, "list")
  expect_named(result$.thresholds, c("treatment", "satisfaction"))
  
  # Verify binary threshold
  expect_equal(length(result$.thresholds$treatment), 1)
  expect_equal(attr(result$.thresholds$treatment, "labels"), c("Control", "Treatment"))
  
  # Verify ordinal thresholds
  expect_equal(length(result$.thresholds$satisfaction), 2)
  expect_equal(attr(result$.thresholds$satisfaction, "labels"), c("Low", "Medium", "High"))
})


test_that("make_covariates handles correlations with mixed covariate types", {
  covs <- list(
    make_continuous("age", mean = 50, total_var = 100, icc = 0.2),
    make_binary("treatment", prob = 0.5, icc = 0.1),
    make_ordinal("satisfaction", probs = c(0.3, 0.4, 0.3), icc = 0.15)
  )
  corrs <- list(
    define_correlation("age", "treatment", corr_within = 0.1, corr_between = 0.05),
    define_correlation("age", "satisfaction", corr_within = 0.2, corr_between = 0.1)
  )
    
  expect_warning(
    result <- make_covariates(
      covariates = covs,
      correlations = corrs
    ),
    "latent space"
  )
  
  # Verify correlation matrices are correct
  expect_equal(result$R_w[1, 2], 0.1)
  expect_equal(result$R_w[1, 3], 0.2)
  expect_equal(result$R_b[1, 2], 0.05)
  expect_equal(result$R_b[1, 3], 0.1)
})


test_that("make_covariates computes variance-covariance matrices correctly", {
  covs <- list(
    make_continuous("x1", mean = 0, total_var = 4, icc = 0.25),
    make_continuous("x2", mean = 0, total_var = 9, icc = 0.5)
  )
  corrs <- list(
    define_correlation("x1", "x2", corr_within = 0.5, corr_between = 0.3)
  )
  
  result <- make_covariates(covariates = covs, correlations = corrs)
  
  # Verify Sigma = D %*% R %*% D
  expected_sigma_w <- result$D_w %*% result$R_w %*% result$D_w
  expected_sigma_b <- result$D_b %*% result$R_b %*% result$D_b
  
  expect_equal(result$Sigma_w, expected_sigma_w)
  expect_equal(result$Sigma_b, expected_sigma_b)
})

test_that("make_covariates extracts ICC values correctly", {
  covs <- list(
    make_continuous("x1", icc = 0.3),
    make_binary("x2", prob = 0.5, icc = 0.1),
    make_ordinal("x3", probs = c(0.3, 0.4, 0.3), icc = 0.2)
  )
  
  result <- make_covariates(covariates = covs)
  
  # Verify ICC values are correct
  expect_equal(result$icc, c(x1 = 0.3, x2 = 0.1, x3 = 0.2))
})


test_that("make_covariates extracts mean values correctly", {
  covs <- list(
    make_continuous("x1", mean = 10, icc = 0.3),
    make_binary("x2", prob = 0.5, icc = 0.1),
    make_ordinal("x3", probs = c(0.3, 0.4, 0.3), icc = 0.2)
  )
  
  result <- make_covariates(covariates = covs)
  
  # Verify mean values are correct
  expect_equal(result$mean, c(x1 = 10, x2 = 0, x3 = 0))
})

test_that("make_covariates extracts total_var values correctly", {
  covs <- list(
    make_continuous("x1", total_var = 25, icc = 0.3),
    make_binary("x2", prob = 0.5, icc = 0.1),
    make_ordinal("x3", probs = c(0.3, 0.4, 0.3), icc = 0.2)
  )
  
  result <- make_covariates(covariates = covs)
  
  # Verify total_var values are correct
  expect_equal(result$total_var, c(x1 = 25, x2 = 1, x3 = 1))
})
