# --- Test correlation pairs ---

test_that("define_correlation creates valid object with rho_within only", {
  result <- define_correlation("x1", "x2", rho_within = 0.5)

  expect_s3_class(result, "multilevel_correlation")
  expect_s3_class(result, "multilevel_spec")
  expect_equal(result$var1, "x1")
  expect_equal(result$var2, "x2")
  expect_equal(result$rho_within, 0.5)
  expect_null(result$rho_between)
})

test_that("define_correlation creates valid object with rho_between only", {
  result <- define_correlation("x1", "x2", rho_between = -0.3)

  expect_s3_class(result, "multilevel_correlation")
  expect_s3_class(result, "multilevel_spec")
  expect_equal(result$var1, "x1")
  expect_equal(result$var2, "x2")
  expect_null(result$rho_within)
  expect_equal(result$rho_between, -0.3)
})

test_that("define_correlation creates valid object with both rho_within and rho_between", {
  result <- define_correlation("age", "blood_pressure", rho_within = 0.7, rho_between = 0.4)

  expect_s3_class(result, "multilevel_correlation")
  expect_s3_class(result, "multilevel_spec")
  expect_equal(result$var1, "age")
  expect_equal(result$var2, "blood_pressure")
  expect_equal(result$rho_within, 0.7)
  expect_equal(result$rho_between, 0.4)
})

test_that("define_correlation fails when var1 and var2 are identical", {
  expect_error(
    define_correlation("x", "x", rho_within = 0.5),
    "must be different"
  )
})

test_that("define_correlation fails when both rho_within and rho_between are NULL", {
  expect_error(
    define_correlation("x1", "x2"),
    "At least one of.*must be supplied"
  )
})

test_that("define_correlation fails with invalid var1", {
  expect_error(
    define_correlation(123, "x2", rho_within = 0.5),
    "var1"
  )
  expect_error(
    define_correlation("", "x2", rho_within = 0.5),
    "var1"
  )
})

test_that("define_correlation fails with invalid var2", {
  expect_error(
    define_correlation("x1", 456, rho_within = 0.5),
    "var2"
  )
  expect_error(
    define_correlation("x1", "", rho_within = 0.5),
    "var2"
  )
})

test_that("define_correlation fails with invalid rho_within", {
  expect_error(
    define_correlation("x1", "x2", rho_within = 1.5),
    "rho_within"
  )
  expect_error(
    define_correlation("x1", "x2", rho_within = -1.5),
    "rho_within"
  )
  expect_error(
    define_correlation("x1", "x2", rho_within = c(0.1, 0.2)),
    "rho_within"
  )
  expect_error(
    define_correlation("x1", "x2", rho_within = NA),
    "rho_within"
  )
})

test_that("define_correlation fails with invalid rho_between", {
  expect_error(
    define_correlation("x1", "x2", rho_between = 1.5),
    "rho_between"
  )
  expect_error(
    define_correlation("x1", "x2", rho_between = -1.5),
    "rho_between"
  )
  expect_error(
    define_correlation("x1", "x2", rho_between = c(0.1, 0.2)),
    "rho_between"
  )
  expect_error(
    define_correlation("x1", "x2", rho_between = NA),
    "rho_between"
  )
})

test_that("define_correlation handles boundary rho values", {
  result_min <- define_correlation("x1", "x2", rho_within = -1)
  result_max <- define_correlation("x1", "x2", rho_within = 1)

  expect_equal(result_min$rho_within, -1)
  expect_equal(result_max$rho_within, 1)
})

test_that("define_correlation handles rho values of zero", {
  result <- define_correlation("x1", "x2", rho_within = 0, rho_between = 0)

  expect_equal(result$rho_within, 0)
  expect_equal(result$rho_between, 0)
})
