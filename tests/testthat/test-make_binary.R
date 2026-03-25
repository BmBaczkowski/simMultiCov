# --- Test binary covariates ---

test_that("make_binary creates valid object with default parameters", {
  result <- make_binary("x", prob = 0.5, icc = 1)

  expect_s3_class(result, "covariate")
  expect_s3_class(result, "covariate_binary")
  expect_equal(result$name, "x")
  expect_equal(result$type, "binary")
  expect_equal(result$specs$probs, c(0.5, 0.5))
  expect_equal(result$specs$labels, c("0", "1"))
  expect_equal(result$specs$mean, 0)
  expect_equal(result$specs$total_var, 1)
  expect_equal(result$specs$icc, 1)
})

test_that("make_binary creates object with custom labels", {
  result <- make_binary(
    name = "treated",
    prob = 0.3,
    icc = 0.1,
    labels = c("control", "treatment")
  )

  expect_equal(result$name, "treated")
  expect_equal(result$type, "binary")
  expect_equal(result$specs$probs, c(0.3, 0.7))
  expect_equal(result$specs$icc, 0.1)
  expect_equal(result$specs$labels, c("control", "treatment"))
})


test_that("make_binary fails with invalid name", {
  expect_error(
    make_binary(123),
    "name|make_binary()"
  )
  expect_error(
    make_binary(""),
    "name|make_binary()"
  )
  expect_error(
    make_binary(c("a", "b")),
    "name|make_binary()"
  )
  expect_error(
    make_binary("with space"),
    "name|make_binary()"
  )
})

test_that("make_binary fails with invalid prob", {
  expect_error(
    make_binary(name = "x", prob = 0, icc = 0),
    "prob|make_binary()"
  )
  expect_error(
    make_binary(name = "x", prob = 1.5, icc = 0),
    "prob|make_binary()"
  )
  expect_error(
    make_binary(name = "x", prob = c(0.3, 0.4), icc = 0),
    "prob|make_binary()"
  )
  expect_error(
    make_binary(name = "x", prob = c(NA, 0.4), icc = 0),
    "prob|make_binary()"
  )
  expect_error(
    make_binary(name = "x", prob = NA, icc = 0),
    "prob|make_binary()"
  )
})

test_that("make_binary fails with invalid icc", {
  expect_error(
    make_binary("x", prob = .5, icc = -0.1),
    "icc|make_binary()"
  )
  expect_error(
    make_binary("x", prob = .5, icc = 1.5),
    "icc|make_binary()"
  )
  expect_error(
    make_binary("x", prob = .5, icc = c(0.1, 0.2)),
    "icc|make_binary()"
  )
  expect_error(
    make_binary("x", prob = .5, icc = NA),
    "icc|make_binary()"
  )
})

test_that("make_binary fails with invalid labels", {
  expect_error(
    make_binary(name = "x", prob = 0.5, icc = 0, labels = "only_one"),
    "labels|make_binary()"
  )
  expect_error(
    make_binary(name = "x", prob = 0.5, icc = 0, labels = c("a", "")),
    "labels|make_binary()"
  )
  expect_error(
    make_binary(name = "x", prob = 0.5, icc = 0, labels = c("a", "with space")),
    "labels|make_binary()"
  )
})

# --- Edge cases ---

test_that("make_binary handles edge case icc values", {
  result_0 <- make_binary("x", prob = 0.5, icc = 0)
  result_1 <- make_binary("x", prob = 0.5, icc = 1)

  expect_equal(result_0$specs$icc, 0)
  expect_equal(result_1$specs$icc, 1)
})
