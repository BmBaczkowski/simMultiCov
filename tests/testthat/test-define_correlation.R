# --- Test correlation pairs ---

test_that("define_correlation creates valid object with corr_within only", {
  result <- define_correlation("x1", "x2", corr_within = 0.5)

  expect_s3_class(result, "correlation")
  expect_equal(result$var1, "x1")
  expect_equal(result$var2, "x2")
  expect_equal(result$corr_within, 0.5)
  expect_equal(result$corr_between, 0)
})

test_that("define_correlation creates valid object with corr_between only", {
  result <- define_correlation("x1", "x2", corr_between = -0.3)

  expect_s3_class(result, "correlation")
  expect_equal(result$var1, "x1")
  expect_equal(result$var2, "x2")
  expect_equal(result$corr_within, 0)
  expect_equal(result$corr_between, -0.3)
})

test_that("define_correlation creates valid object with both corr_within and corr_between", {
  result <- define_correlation("age", "blood_pressure", corr_within = 0.7, corr_between = 0.4)

  expect_s3_class(result, "correlation")
  expect_equal(result$var1, "age")
  expect_equal(result$var2, "blood_pressure")
  expect_equal(result$corr_within, 0.7)
  expect_equal(result$corr_between, 0.4)
})

test_that("define_correlation fails when var1 and var2 are identical", {
  expect_error(
    define_correlation("x", "x", corr_within = 0.5),
    "must be different"
  )
})

test_that("define_correlation fails when both corr_within and corr_between are NULL", {
  expect_error(
    define_correlation("x1", "x2"),
    "At least one of.*must be supplied"
  )
})

test_that("define_correlation fails with invalid var1", {
  expect_error(
    define_correlation(123, "x2", corr_within = 0.5),
    "var1"
  )
  expect_error(
    define_correlation("", "x2", corr_within = 0.5),
    "var1"
  )
})

test_that("define_correlation fails with invalid var2", {
  expect_error(
    define_correlation("x1", 456, corr_within = 0.5),
    "var2"
  )
  expect_error(
    define_correlation("x1", "", corr_within = 0.5),
    "var2"
  )
})

test_that("define_correlation fails with invalid corr_within", {
  expect_error(
    define_correlation("x1", "x2", corr_within = 1.5),
    "corr_within"
  )
  expect_error(
    define_correlation("x1", "x2", corr_within = -1.5),
    "corr_within"
  )
  expect_error(
    define_correlation("x1", "x2", corr_within = c(0.1, 0.2)),
    "corr_within"
  )
  expect_error(
    define_correlation("x1", "x2", corr_within = NA),
    "corr_within"
  )
})

test_that("define_correlation fails with invalid corr_between", {
  expect_error(
    define_correlation("x1", "x2", corr_between = 1.5),
    "corr_between"
  )
  expect_error(
    define_correlation("x1", "x2", corr_between = -1.5),
    "corr_between"
  )
  expect_error(
    define_correlation("x1", "x2", corr_between = c(0.1, 0.2)),
    "corr_between"
  )
  expect_error(
    define_correlation("x1", "x2", corr_between = NA),
    "corr_between"
  )
})

test_that("define_correlation handles boundary rho values", {
  result_min <- define_correlation("x1", "x2", corr_within = -1)
  result_max <- define_correlation("x1", "x2", corr_within = 1)

  expect_equal(result_min$corr_within, -1)
  expect_equal(result_max$corr_within, 1)
})

test_that("define_correlation handles rho values of zero", {
  result <- define_correlation("x1", "x2", corr_within = 0, corr_between = 0)

  expect_equal(result$corr_within, 0)
  expect_equal(result$corr_between, 0)
})
