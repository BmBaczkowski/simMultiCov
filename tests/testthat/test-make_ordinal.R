# --- Test binary covariates ---

test_that("make_ordinal creates valid object with default parameters", {
  result <- make_ordinal("x", probs = c(.2, .3, .5), icc = 1)

  expect_s3_class(result, "covariate")
  expect_s3_class(result, "covariate_ordinal")
  expect_equal(result$name, "x")
  expect_equal(result$type, "ordinal")
  expect_equal(result$specs$probs, c(.2, .3, .5))
  expect_equal(result$specs$labels, c("A", "B", "C"))
  expect_equal(result$specs$mean, 0)
  expect_equal(result$specs$total_var, 1)
  expect_equal(result$specs$icc, 1)
})

test_that("make_ordinal creates object with custom labels", {
  result <- make_ordinal(
    name = "test_var",
    probs = c(0.3, 0.6, 0.1),
    icc = 0.1,
    labels = c("A", "B", "C")
  )

  expect_equal(result$name, "test_var")
  expect_equal(result$type, "ordinal")
  expect_equal(result$specs$probs, c(0.3, 0.6, 0.1))
  expect_equal(result$specs$icc, 0.1)
  expect_equal(result$specs$labels, c("A", "B", "C"))
})


test_that("make_ordinal fails with invalid name", {
  expect_error(
    make_ordinal(123),
    "name|make_ordinal()"
  )
  expect_error(
    make_ordinal(""),
    "name|make_ordinal()"
  )
  expect_error(
    make_ordinal(c("a", "b")),
    "name|make_ordinal()"
  )
  expect_error(
    make_ordinal("with space"),
    "name|make_ordinal()"
  )
})

test_that("make_ordinal fails with invalid prob", {
  expect_error(
    make_ordinal(name = "x", probs = 0, icc = 0),
    "probs|make_ordinal()"
  )
  expect_error(
    make_ordinal(name = "x", probs = c(0.5, 0.3, 0.9), icc = 0),
    "probs|make_ordinal()"
  )
  expect_error(
    make_ordinal(name = "x", probs = c(0.3, 0.4), icc = 0),
    "probs|make_ordinal()"
  )
  expect_error(
    make_ordinal(name = "x", probs = c(NA, 0.4, 0.1), icc = 0),
    "probs|make_ordinal()"
  )
  expect_error(
    make_ordinal(name = "x", probs = NA, icc = 0),
    "probs|make_ordinal()"
  )
})

test_that("make_ordinal fails with invalid icc", {
  expect_error(
    make_ordinal("x", probs = c(.5, .1, .4), icc = -0.1),
    "icc|make_ordinal()"
  )
  expect_error(
    make_ordinal("x", probs = c(.5, .1, .4), icc = 1.5),
    "icc|make_ordinal()"
  )
  expect_error(
    make_ordinal("x", probs = c(.5, .1, .4), icc = c(0.1, 0.2)),
    "icc|make_ordinal()"
  )
  expect_error(
    make_ordinal("x", probs = c(.5, .1, .4), icc = NA),
    "icc|make_ordinal()"
  )
})

test_that("make_ordinal fails with invalid labels", {
  expect_error(
    make_ordinal(name = "x", probs = c(.5, .1, .4), icc = 0, labels = "only_one"),
    "labels|make_ordinal()"
  )
  expect_error(
    make_ordinal(name = "x", probs = c(.5, .1, .4), icc = 0, labels = c("a", "")),
    "labels|make_ordinal()"
  )
  expect_error(
    make_ordinal(name = "x", probs = c(.5, .1, .4), icc = 0, labels = c("a", "with space")),
    "labels|make_ordinal()"
  )
})

# --- Edge cases ---

test_that("make_ordinal handles edge case icc values", {
  result_0 <- make_ordinal("x", probs = c(.5, .1, .4), icc = 0)
  result_1 <- make_ordinal("x", probs = c(.5, .1, .4), icc = 1)

  expect_equal(result_0$specs$icc, 0)
  expect_equal(result_1$specs$icc, 1)
})
