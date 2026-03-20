# --- Test continuous covariates ---

test_that("continuous_covariate creates valid object with default parameters", {
  result <- continuous_covariate("age")

  expect_s3_class(result, "ml_covariate")
  expect_s3_class(result, "ml_covariate_continous")
  expect_equal(result$name, "age")
  expect_equal(result$mean, 0)
  expect_equal(result$sd, 1)
  expect_equal(result$icc, 0)
  expect_equal(result$type, "continuous")
})

test_that("continuous_covariate creates object with custom parameters", {
  result <- continuous_covariate(
    name = "test_var",
    mean = 50,
    sd = 10,
    icc = 0.05
  )

  expect_equal(result$name, "test_var")
  expect_equal(result$mean, 50)
  expect_equal(result$sd, 10)
  expect_equal(result$icc, 0.05)
  expect_equal(result$type, "continuous")
})

test_that("continuous_covariate fails with invalid name", {
  expect_error(
    continuous_covariate(123),
    "continuous_covariate_name"
  )
  expect_error(
    continuous_covariate(""),
    "continuous_covariate_name"
  )
  expect_error(
    continuous_covariate(c("a", "b")),
    "continuous_covariate_name"
  )
})

test_that("continuous_covariate fails with invalid mean", {
  expect_error(
    continuous_covariate("x", mean = "a"),
    "continuous_covariate_mean"
  )
  expect_error(
    continuous_covariate("x", mean = c(1, 2)),
    "continuous_covariate_mean"
  )
})

test_that("continuous_covariate fails with invalid sd", {
  expect_error(
    continuous_covariate("x", sd = -1),
    "continuous_covariate_sd"
  )
  expect_error(
    continuous_covariate("x", sd = 0),
    "continuous_covariate_sd"
  )
  expect_error(
    continuous_covariate("x", sd = c(1, 2)),
    "continuous_covariate_sd"
  )
})

test_that("continuous_covariate fails with invalid icc", {
  expect_error(
    continuous_covariate("x", icc = -0.1),
    "continuous_covariate_icc"
  )
  expect_error(
    continuous_covariate("x", icc = 1.5),
    "continuous_covariate_icc"
  )
  expect_error(
    continuous_covariate("x", icc = c(0.1, 0.2)),
    "continuous_covariate_icc"
  )
})

# --- Test binary covariates ---

test_that("binary_covariate creates valid object with default labels", {
  result <- binary_covariate(
    name = "gender",
    prob = 0.5,
    icc = 0
  )

  expect_s3_class(result, "ml_covariate")
  expect_s3_class(result, "ml_covariate_binary")
  expect_equal(result$name, "gender")
  expect_equal(result$prob, 0.5)
  expect_equal(result$icc, 0)
  expect_equal(result$labels, c("0", "1"))
  expect_equal(result$type, "binary")
})

test_that("binary_covariate creates object with custom labels", {
  result <- binary_covariate(
    name = "treated",
    prob = 0.3,
    icc = 0.1,
    labels = c("control", "treatment")
  )

  expect_equal(result$name, "treated")
  expect_equal(result$prob, 0.3)
  expect_equal(result$icc, 0.1)
  expect_equal(result$labels, c("control", "treatment"))
  expect_equal(result$type, "binary")
})

test_that("binary_covariate fails with invalid name", {
  expect_error(
    binary_covariate(name = 123, prob = 0.5, icc = 0),
    "binary_covariate_name"
  )
  expect_error(
    binary_covariate(name = "", prob = 0.5, icc = 0),
    "binary_covariate_name"
  )
})

test_that("binary_covariate fails with invalid prob", {
  expect_error(
    binary_covariate(name = "x", prob = 0, icc = 0),
    "binary_covariate_prob"
  )
  expect_error(
    binary_covariate(name = "x", prob = 1.5, icc = 0),
    "binary_covariate_prob"
  )
  expect_error(
    binary_covariate(name = "x", prob = c(0.3, 0.4), icc = 0),
    "binary_covariate_prob"
  )
})

test_that("binary_covariate fails with invalid icc", {
  expect_error(
    binary_covariate(name = "x", prob = 0.5, icc = -0.1),
    "binary_covariate_icc"
  )
  expect_error(
    binary_covariate(name = "x", prob = 0.5, icc = 1.5),
    "binary_covariate_icc"
  )
  expect_error(
    binary_covariate(name = "x", prob = 0.5, icc = c(0.1, 0.2)),
    "binary_covariate_icc"
  )
})

test_that("binary_covariate fails with invalid labels", {
  expect_error(
    binary_covariate(name = "x", prob = 0.5, icc = 0, labels = "only_one"),
    "binary_covariate_labels"
  )
  expect_error(
    binary_covariate(name = "x", prob = 0.5, icc = 0, labels = c("a", "")),
    "binary_covariate_labels"
  )
})

# --- Test ordinal covariates ---

test_that("ordinal_covariate creates valid object with default labels", {
  result <- ordinal_covariate(
    name = "education",
    probs = c(0.3, 0.4, 0.3),
    icc = 0
  )

  expect_s3_class(result, "ml_covariate")
  expect_s3_class(result, "ml_covariate_ordinal")
  expect_equal(result$name, "education")
  expect_equal(result$probs, c(0.3, 0.4, 0.3))
  expect_equal(result$icc, 0)
  expect_equal(result$labels, c("A", "B", "C"))
  expect_equal(result$type, "ordinal")
})

test_that("ordinal_covariate creates object with custom labels", {
  result <- ordinal_covariate(
    name = "income",
    probs = c(0.25, 0.5, 0.25),
    icc = 0.05,
    labels = c("low", "medium", "high")
  )

  expect_equal(result$name, "income")
  expect_equal(result$probs, c(0.25, 0.5, 0.25))
  expect_equal(result$icc, 0.05)
  expect_equal(result$labels, c("low", "medium", "high"))
  expect_equal(result$type, "ordinal")
})

test_that("ordinal_covariate fails with invalid name", {
  expect_error(
    ordinal_covariate(name = 123, probs = c(0.5, 0.5), icc = 0),
    "ordinal_covariate_name"
  )
  expect_error(
    ordinal_covariate(name = "", probs = c(0.5, 0.5), icc = 0),
    "ordinal_covariate_name"
  )
})

test_that("ordinal_covariate fails with invalid probs", {
  expect_error(
    ordinal_covariate(name = "x", probs = "a", icc = 0),
    "ordinal_covariate_probs"
  )
  expect_error(
    ordinal_covariate(name = "x", probs = c(0.5), icc = 0),
    "ordinal_covariate_probs"
  )
  expect_error(
    ordinal_covariate(name = "x", probs = c(0.3, 0.3, 0.3), icc = 0),
    "'probs' must sum to 1"
  )
  expect_error(
    ordinal_covariate(name = "x", probs = c(-0.1, 1.1), icc = 0),
    "ordinal_covariate_probs"
  )
})

test_that("ordinal_covariate fails when probs do not sum to 1", {
  expect_error(
    ordinal_covariate(name = "x", probs = c(0.5, 0.3, 0.1), icc = 0),
    "'probs' must sum to 1"
  )
})

test_that("ordinal_covariate fails with invalid icc", {
  expect_error(
    ordinal_covariate(name = "x", probs = c(0.1, 0.4, 0.5), icc = -0.1),
    "ordinal_covariate_icc"
  )
  expect_error(
    ordinal_covariate(name = "x", probs = c(0.1, 0.4, 0.5), icc = 1.5),
    "ordinal_covariate_icc"
  )
  expect_error(
    ordinal_covariate(name = "x", probs = c(0.1, 0.4, 0.5), icc = c(0.1, 0.2)),
    "ordinal_covariate_icc"
  )
})

test_that("ordinal_covariate fails with invalid labels", {
  expect_error(
    ordinal_covariate(
      name = "x", probs = c(0.1, 0.4, 0.5), icc = 0, labels = "only_one"
    ),
    "ordinal_covariate_labels"
  )
  expect_error(
    ordinal_covariate(
      name = "x", probs = c(0.1, 0.4, 0.5), icc = 0, labels = c("a", "")
    ),
    "ordinal_covariate_labels"
  )
})

# ------------------------------------------------------------------------------
# Edge cases
# ------------------------------------------------------------------------------

test_that("continuous_covariate handles edge case icc values", {
  result_0 <- continuous_covariate("x", icc = 0)
  result_1 <- continuous_covariate("x", icc = 1)

  expect_equal(result_0$icc, 0)
  expect_equal(result_1$icc, 1)
})

test_that("binary_covariate handles boundary prob values", {
  result_low <- binary_covariate("x", prob = 1e-5, icc = 0)
  result_high <- binary_covariate("x", prob = 1 - 1e-5, icc = 0)

  expect_equal(result_low$prob, 1e-5)
  expect_equal(result_high$prob, 1 - 1e-5)
})

