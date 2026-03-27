# --- Test continuous covariates ---

test_that("make_continuous creates valid object with default parameters", {
  result <- make_continuous("age")

  expect_s3_class(result, "covariate")
  expect_s3_class(result, "covariate_continuous")
  expect_equal(result$name, "age")
  expect_equal(result$type, "continuous")
  expect_equal(result$specs$mean, 0)
  expect_equal(result$specs$total_var, 1)
  expect_equal(result$specs$icc, 0)

})


test_that("make_continuous creates object with custom parameters", {
  result <- make_continuous(
    name = "test_var",
    mean = 50,
    total_var = 10,
    icc = 0.05
  )

  expect_equal(result$name, "test_var")
  expect_equal(result$type, "continuous")
  expect_equal(result$specs$mean, 50)
  expect_equal(result$specs$total_var, 10)
  expect_equal(result$specs$icc, 0.05)
})


test_that("make_continuous fails with invalid name", {
  expect_error(
    make_continuous(123),
    "name|make_continuous()"
  )
  expect_error(
    make_continuous(""),
    "name|make_continuous()"
  )
  expect_error(
    make_continuous(c("a", "b")),
    "name|make_continuous()"
  )
})


test_that("make_continuous fails with invalid mean", {
  expect_error(
    make_continuous("x", mean = "a"),
    "name|make_continuous()"
  )
  expect_error(
    make_continuous("x", mean = c(1, 2)),
    "name|make_continuous()"
  )
  expect_error(
    make_continuous("x", mean = NA),
    "name|make_continuous()"
  )
})


test_that("make_continuous fails with invalid total_var", {
  expect_error(
    make_continuous("x", total_var = -1),
    "total_var|make_continuous()"
  )
  expect_error(
    make_continuous("x", total_var = 0),
    "total_var|make_continuous()"
  )
  expect_error(
    make_continuous("x", total_var = c(1, 2)),
    "total_var|make_continuous()"
  )
  expect_error(
    make_continuous("x", total_var = NA),
    "total_var|make_continuous()"
  )
})


test_that("make_continuous fails with invalid icc", {
  expect_error(
    make_continuous("x", icc = -0.1),
    "icc|make_continuous()"
  )
  expect_error(
    make_continuous("x", icc = 1.5),
    "icc|make_continuous()"
  )
  expect_error(
    make_continuous("x", icc = c(0.1, 0.2)),
    "icc|make_continuous()"
  )
  expect_error(
    make_continuous("x", icc = NA),
    "icc|make_continuous()"
  )
})

# --- Edge cases ---

test_that("make_continuous handles edge case icc values", {
  result_0 <- make_continuous("x", icc = 0)
  result_1 <- make_continuous("x", icc = 1)

  expect_equal(result_0$specs$icc, 0)
  expect_equal(result_1$specs$icc, 1)
})
