# --- Test continuous covariates ---

test_that("make_continuous creates valid object with default parameters", {
  result <- make_continuous("age")

  expect_s3_class(result, "multilevel_covariate")
  expect_s3_class(result, "multilevel_continuous")
  expect_equal(result$name, "age")
  expect_equal(result$mean, 0)
  expect_equal(result$sd, 1)
  expect_equal(result$icc, 0)
  expect_equal(result$type, "continuous")
})

test_that("make_continuous creates object with custom parameters", {
  result <- make_continuous(
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

test_that("make_continuous fails with invalid name", {
  expect_error(
    make_continuous(123),
    "make_continuous_name"
  )
  expect_error(
    make_continuous(""),
    "make_continuous_name"
  )
  expect_error(
    make_continuous(c("a", "b")),
    "make_continuous_name"
  )
})

test_that("make_continuous fails with invalid mean", {
  expect_error(
    make_continuous("x", mean = "a"),
    "make_continuous_mean"
  )
  expect_error(
    make_continuous("x", mean = c(1, 2)),
    "make_continuous_mean"
  )
  expect_error(
    make_continuous("x", mean = NA),
    "make_continuous_mean"
  )
})

test_that("make_continuous fails with invalid sd", {
  expect_error(
    make_continuous("x", sd = -1),
    "make_continuous_sd"
  )
  expect_error(
    make_continuous("x", sd = 0),
    "make_continuous_sd"
  )
  expect_error(
    make_continuous("x", sd = c(1, 2)),
    "make_continuous_sd"
  )
  expect_error(
    make_continuous("x", sd = NA),
    "make_continuous_sd"
  )
})

test_that("make_continuous fails with invalid icc", {
  expect_error(
    make_continuous("x", icc = -0.1),
    "make_continuous_icc"
  )
  expect_error(
    make_continuous("x", icc = 1.5),
    "make_continuous_icc"
  )
  expect_error(
    make_continuous("x", icc = c(0.1, 0.2)),
    "make_continuous_icc"
  )
  expect_error(
    make_continuous("x", icc = NA),
    "make_continuous_icc"
  )
})

# --- Test binary covariates ---

test_that("make_binary creates valid object with default labels", {
  result <- make_binary(
    name = "gender",
    prob = 0.5,
    icc = 0
  )

  expect_s3_class(result, "multilevel_covariate")
  expect_s3_class(result, "multilevel_binary")
  expect_equal(result$name, "gender")
  expect_equal(result$prob, 0.5)
  expect_equal(result$icc, 0)
  expect_equal(result$labels, c("0", "1"))
  expect_equal(result$type, "binary")
})

test_that("make_binary creates object with custom labels", {
  result <- make_binary(
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

test_that("make_binary fails with invalid name", {
  expect_error(
    make_binary(name = 123, prob = 0.5, icc = 0),
    "make_binary_name"
  )
  expect_error(
    make_binary(name = "", prob = 0.5, icc = 0),
    "make_binary_name"
  )
})

test_that("make_binary fails with invalid prob", {
  expect_error(
    make_binary(name = "x", prob = 0, icc = 0),
    "make_binary_prob"
  )
  expect_error(
    make_binary(name = "x", prob = 1.5, icc = 0),
    "make_binary_prob"
  )
  expect_error(
    make_binary(name = "x", prob = c(0.3, 0.4), icc = 0),
    "make_binary_prob"
  )
  expect_error(
    make_binary(name = "x", prob = c(NA, 0.4), icc = 0),
    "make_binary_prob"
  )
  expect_error(
    make_binary(name = "x", prob = NA, icc = 0),
    "make_binary_prob"
  )
})

test_that("make_binary fails with invalid icc", {
  expect_error(
    make_binary(name = "x", prob = 0.5, icc = -0.1),
    "make_binary_icc"
  )
  expect_error(
    make_binary(name = "x", prob = 0.5, icc = 1.5),
    "make_binary_icc"
  )
  expect_error(
    make_binary(name = "x", prob = 0.5, icc = c(0.1, 0.2)),
    "make_binary_icc"
  )
  expect_error(
    make_binary(name = "x", prob = 0.5, icc = NA),
    "make_binary_icc"
  )
})

test_that("make_binary fails with invalid labels", {
  expect_error(
    make_binary(name = "x", prob = 0.5, icc = 0, labels = "only_one"),
    "make_binary_labels"
  )
  expect_error(
    make_binary(name = "x", prob = 0.5, icc = 0, labels = c("a", "")),
    "make_binary_labels"
  )
})

# --- Test ordinal covariates ---

test_that("make_ordinal creates valid object with default labels", {
  result <- make_ordinal(
    name = "education",
    probs = c(0.3, 0.4, 0.3),
    icc = 0
  )

  expect_s3_class(result, "multilevel_covariate")
  expect_s3_class(result, "multilevel_ordinal")
  expect_equal(result$name, "education")
  expect_equal(result$probs, c(0.3, 0.4, 0.3))
  expect_equal(result$icc, 0)
  expect_equal(result$labels, c("A", "B", "C"))
  expect_equal(result$type, "ordinal")
})

test_that("make_ordinal creates object with custom labels", {
  result <- make_ordinal(
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

test_that("make_ordinal fails with invalid name", {
  expect_error(
    make_ordinal(name = 123, probs = c(0.5, 0.5), icc = 0),
    "make_ordinal_name"
  )
  expect_error(
    make_ordinal(name = "", probs = c(0.5, 0.5), icc = 0),
    "make_ordinal_name"
  )
})

test_that("make_ordinal fails with invalid probs", {
  expect_error(
    make_ordinal(name = "x", probs = "a", icc = 0),
    "make_ordinal_probs"
  )
  expect_error(
    make_ordinal(name = "x", probs = c(0.5), icc = 0),
    "make_ordinal_probs"
  )
  expect_error(
    make_ordinal(name = "x", probs = c(0.3, 0.3, 0.3), icc = 0),
    "'probs' must sum to 1"
  )
  expect_error(
    make_ordinal(name = "x", probs = c(-0.1, 1.1), icc = 0),
    "make_ordinal_probs"
  )
  expect_error(
    make_ordinal(name = "x", probs = c(NA, 0.5, 0.5), icc = 0),
    "make_ordinal_probs"
  )
})

test_that("make_ordinal fails when probs do not sum to 1", {
  expect_error(
    make_ordinal(name = "x", probs = c(0.5, 0.3, 0.1), icc = 0),
    "'probs' must sum to 1"
  )
})

test_that("make_ordinal fails with invalid icc", {
  expect_error(
    make_ordinal(name = "x", probs = c(0.1, 0.4, 0.5), icc = -0.1),
    "make_ordinal_icc"
  )
  expect_error(
    make_ordinal(name = "x", probs = c(0.1, 0.4, 0.5), icc = 1.5),
    "make_ordinal_icc"
  )
  expect_error(
    make_ordinal(name = "x", probs = c(0.1, 0.4, 0.5), icc = c(0.1, 0.2)),
    "make_ordinal_icc"
  )
  expect_error(
    make_ordinal(name = "x", probs = c(0.1, 0.4, 0.5), icc = NA),
    "make_ordinal_icc"
  )
})

test_that("make_ordinal fails with invalid labels", {
  expect_error(
    make_ordinal(
      name = "x", probs = c(0.1, 0.4, 0.5), icc = 0, labels = "only_one"
    ),
    "make_ordinal_labels"
  )
  expect_error(
    make_ordinal(
      name = "x", probs = c(0.1, 0.4, 0.5), icc = 0, labels = c("a", "")
    ),
    "make_ordinal_labels"
  )
})

# --- Edge cases ---

test_that("make_continuous handles edge case icc values", {
  result_0 <- make_continuous("x", icc = 0)
  result_1 <- make_continuous("x", icc = 1)

  expect_equal(result_0$icc, 0)
  expect_equal(result_1$icc, 1)
})

test_that("make_binary handles boundary prob values", {
  result_low <- make_binary("x", prob = 1e-5, icc = 0)
  result_high <- make_binary("x", prob = 1 - 1e-5, icc = 0)

  expect_equal(result_low$prob, 1e-5)
  expect_equal(result_high$prob, 1 - 1e-5)
})
