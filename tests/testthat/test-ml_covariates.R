# Helpers to create valid inputs
create_test_covariates <- function() {
  list(
    continuous_covariate("x1", icc = 0.3),
    continuous_covariate("x2", icc = 0.4),
    binary_covariate("treatment", prob = 0.5, icc = 0),
    ordinal_covariate("education", probs = c(0.3, 0.4, 0.3), icc = 0.1)
  )
}

create_test_correlations <- function() {
    list(corr_pair("x1", "x2", rho_within = 0.2))
}

# --- Tests for valid inputs ---

test_that("ml_covariates creates valid object with minimum valid inputs", {
  covs <- create_test_covariates()
  result <- ml_covariates(
    n_L2 = 5,
    n_L1 = 10,
    covariates = covs
  )

  expect_s3_class(result, "ml_covariates")
})

test_that("ml_covariates creates valid object with all parameters", {
  covs <- create_test_covariates()
  corrs <- create_test_correlations()

  result <- ml_covariates(
    n_L2 = 5,
    n_L1 = 10,
    cluster_name = "school",
    covariates = covs,
    correlations = corrs
  )

  expect_s3_class(result, "ml_covariates")
})

test_that("ml_covariates accepts scalar n_L1 and replicates for all clusters", {
  covs <- list(continuous_covariate("x"))
  result <- ml_covariates(
    n_L2 = 3,
    n_L1 = 10,
    covariates = covs
  )

  expect_s3_class(result, "ml_covariates")
})

test_that("ml_covariates accepts vector n_L1 of length n_L2", {
  covs <- list(continuous_covariate("x"))
  result <- ml_covariates(
    n_L2 = 3,
    n_L1 = c(10, 15, 20),
    covariates = covs
  )

  expect_s3_class(result, "ml_covariates")
})

test_that("ml_covariates handles single cluster", {
  covs <- list(continuous_covariate("x"))
  result <- ml_covariates(
    n_L2 = 1,
    n_L1 = 50,
    covariates = covs
  )

  expect_s3_class(result, "ml_covariates")
})

# --- Tests for n_L2 validation ---

test_that("ml_covariates fails when n_L2 is not an integer", {
  covs <- list(continuous_covariate("x"))

  expect_error(
    ml_covariates(n_L2 = 5.5, n_L1 = 10, covariates = covs),
    "n_L2"
  )
})

test_that("ml_covariates fails when n_L2 is less than 1", {
  covs <- list(continuous_covariate("x"))

  expect_error(
    ml_covariates(n_L2 = 0, n_L1 = 10, covariates = covs),
    "n_L2"
  )
  expect_error(
    ml_covariates(n_L2 = -1, n_L1 = 10, covariates = covs),
    "n_L2"
  )
})

test_that("ml_covariates fails when n_L2 is not numeric", {
  covs <- list(continuous_covariate("x"))

  expect_error(
    ml_covariates(n_L2 = "5", n_L1 = 10, covariates = covs),
    "n_L2"
  )
})

test_that("ml_covariates fails when n_L2 is NA", {
  covs <- list(continuous_covariate("x"))

  expect_error(
    ml_covariates(n_L2 = NA, n_L1 = 10, covariates = covs),
    "n_L2"
  )
})

# --- Tests for n_L1 validation ---

test_that("ml_covariates fails when n_L1 is not a scalar or length n_L2", {
  covs <- list(continuous_covariate("x"))

  expect_error(
    ml_covariates(n_L2 = 3, n_L1 = c(10, 20), covariates = covs),
    "n_L1"
  )
})

test_that("ml_covariates fails when n_L1 contains missing values", {
  covs <- list(continuous_covariate("x"))

  expect_error(
    ml_covariates(n_L2 = 3, n_L1 = c(10, NA, 20), covariates = covs),
    "n_L1"
  )
})

test_that("ml_covariates fails when n_L1 is less than 1", {
  covs <- list(continuous_covariate("x"))

  expect_error(
    ml_covariates(n_L2 = 3, n_L1 = c(0, 10, 20), covariates = covs),
    "n_L1"
  )
})

test_that("ml_covariates fails when n_L1 is not numeric", {
  covs <- list(continuous_covariate("x"))

  expect_error(
    ml_covariates(n_L2 = 3, n_L1 = c("10", "20", "30"), covariates = covs),
    "n_L1"
  )
})

# --- Tests for cluster_name validation ---

test_that("ml_covariates fails when cluster_name is not a string", {
  covs <- list(continuous_covariate("x"))

  expect_error(
    ml_covariates(n_L2 = 3, n_L1 = 10, covariates = covs, cluster_name = 123),
    "cluster_name"
  )
})

test_that("ml_covariates fails when cluster_name is empty string", {
  covs <- list(continuous_covariate("x"))

  expect_error(
    ml_covariates(n_L2 = 3, n_L1 = 10, covariates = covs, cluster_name = ""),
    "cluster_name"
  )
})

test_that("ml_covariates fails when cluster_name is not length 1", {
  covs <- list(continuous_covariate("x"))

  expect_error(
    ml_covariates(n_L2 = 3, n_L1 = 10, covariates = covs, cluster_name = c("a", "b")),
    "cluster_name"
  )
})

# --- Tests for covariates validation ---

test_that("ml_covariates fails when covariates is not a list", {
  expect_error(
    ml_covariates(n_L2 = 3, n_L1 = 10, covariates = continuous_covariate("x")),
    "covariates"
  )
})

test_that("ml_covariates fails when covariates list is empty", {
  expect_error(
    ml_covariates(n_L2 = 3, n_L1 = 10, covariates = list()),
    "covariates"
  )
})

test_that("ml_covariates fails when covariates contains non-ml_covariate objects", {
  expect_error(
    ml_covariates(n_L2 = 3, n_L1 = 10, covariates = list("not_a_covariate")),
    "ml_covariates"
  )
})

test_that("ml_covariates fails when covariates are duplicated (same object)", {
  cov <- continuous_covariate("x")

  expect_error(
    ml_covariates(n_L2 = 3, n_L1 = 10, covariates = list(cov, cov)),
    "covariates"
  )
})

test_that("ml_covariates fails with duplicate covariate names", {
  covs <- list(
    continuous_covariate("age", icc = 0.3),
    continuous_covariate("age")
  )

  expect_error(
    ml_covariates(n_L2 = 3, n_L1 = 10, covariates = covs),
    "Duplicate covariate names"
  )
})

test_that("ml_covariates fails when covariates contains missing values", {
  covs <- list(continuous_covariate("x"), NULL)

  expect_error(
    ml_covariates(n_L2 = 3, n_L1 = 10, covariates = covs),
    "covariates"
  )
})

# --- Tests for correlations validation ---

# test_that("ml_covariates fails when correlations is not a list", {
#   covs <- create_test_covariates()

#   expect_error(
#     ml_covariates(
#       n_L2 = 3, 
#       n_L1 = 10, 
#       covariates = covs, 
#       correlations = corr_pair("age", "gender", rho_within = 0.2)
#     ),
#     "correlations"
#   )
# })

# test_that("ml_covariates fails when correlations contains missing values", {
#   covs <- create_test_covariates()
#   corrs <- list(corr_pair("age", "gender", rho_within = 0.2), NULL)

#   expect_error(
#     ml_covariates(
#       n_L2 = 3,
#       n_L1 = 10,
#       covariates = covs,
#       correlations = corrs
#     ),
#     "correlations"
#   )
# })

# # --- Edge cases ---

# test_that("ml_covariates works with single covariate", {
#   covs <- list(continuous_covariate("x"))
#   result <- ml_covariates(
#     n_L2 = 5,
#     n_L1 = 10,
#     covariates = covs
#   )

#   expect_s3_class(result, "ml_covariates")
# })

# test_that("ml_covariates works with many covariates", {
#   covs <- list(
#     continuous_covariate("var1"),
#     continuous_covariate("var2"),
#     continuous_covariate("var3"),
#     binary_covariate("var4"),
#     ordinal_covariate("var5", probs = c(0.3, 0.3, 0.4))
#   )
#   result <- ml_covariates(
#     n_L2 = 5,
#     n_L1 = 10,
#     covariates = covs
#   )

#   expect_s3_class(result, "ml_covariates")
# })

# test_that("ml_covariates works with empty correlations list", {
#   covs <- create_test_covariates()
#   result <- ml_covariates(
#     n_L2 = 5,
#     n_L1 = 10,
#     covariates = covs,
#     correlations = list()
#   )

#   expect_s3_class(result, "ml_covariates")
# })

# test_that("ml_covariates coerces n_L1 from numeric to integer", {
#   covs <- list(continuous_covariate("x"))
#   # Should work with numeric input that gets coerced to integer
#   result <- ml_covariates(
#     n_L2 = 3,
#     n_L1 = 10.5,  # numeric, not integer
#     covariates = covs
#   )

#   expect_s3_class(result, "ml_covariates")
# })

# test_that("ml_covariates coerces n_L2 from numeric to integer", {
#   covs <- list(continuous_covariate("x"))
#   # Should work with numeric input that gets coerced to integer
#   result <- ml_covariates(
#     n_L2 = 5.0,  # numeric, not integer
#     n_L1 = 10,
#     covariates = covs
#   )

#   expect_s3_class(result, "ml_covariates")
# })
