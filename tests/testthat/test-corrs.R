# --- Test correlation pairs ---

test_that("corr_pair creates valid object with rho_within only", {
  result <- corr_pair("x1", "x2", rho_within = 0.5)

  expect_s3_class(result, "ml_corr_pair")
  expect_s3_class(result, "ml_spec")
  expect_equal(result$var1, "x1")
  expect_equal(result$var2, "x2")
  expect_equal(result$rho_within, 0.5)
  expect_null(result$rho_between)
})

test_that("corr_pair creates valid object with rho_between only", {
  result <- corr_pair("x1", "x2", rho_between = -0.3)

  expect_s3_class(result, "ml_corr_pair")
  expect_s3_class(result, "ml_spec")
  expect_equal(result$var1, "x1")
  expect_equal(result$var2, "x2")
  expect_null(result$rho_within)
  expect_equal(result$rho_between, -0.3)
})

test_that("corr_pair creates valid object with both rho_within and rho_between", {
  result <- corr_pair("age", "blood_pressure", rho_within = 0.7, rho_between = 0.4)

  expect_s3_class(result, "ml_corr_pair")
  expect_s3_class(result, "ml_spec")
  expect_equal(result$var1, "age")
  expect_equal(result$var2, "blood_pressure")
  expect_equal(result$rho_within, 0.7)
  expect_equal(result$rho_between, 0.4)
})

test_that("corr_pair fails when var1 and var2 are identical", {
  expect_error(
    corr_pair("x", "x", rho_within = 0.5),
    "must be different"
  )
})

test_that("corr_pair fails when both rho_within and rho_between are NULL", {
  expect_error(
    corr_pair("x1", "x2"),
    "At least one of.*must be supplied"
  )
})

test_that("corr_pair fails with invalid var1", {
  expect_error(
    corr_pair(123, "x2", rho_within = 0.5),
    "var1"
  )
  expect_error(
    corr_pair("", "x2", rho_within = 0.5),
    "var1"
  )
})

test_that("corr_pair fails with invalid var2", {
  expect_error(
    corr_pair("x1", 456, rho_within = 0.5),
    "var2"
  )
  expect_error(
    corr_pair("x1", "", rho_within = 0.5),
    "var2"
  )
})

test_that("corr_pair fails with invalid rho_within", {
  expect_error(
    corr_pair("x1", "x2", rho_within = 1.5),
    "rho_within"
  )
  expect_error(
    corr_pair("x1", "x2", rho_within = -1.5),
    "rho_within"
  )
  expect_error(
    corr_pair("x1", "x2", rho_within = c(0.1, 0.2)),
    "rho_within"
  )
  expect_error(
    corr_pair("x1", "x2", rho_within = NA),
    "rho_within"
  )
})

test_that("corr_pair fails with invalid rho_between", {
  expect_error(
    corr_pair("x1", "x2", rho_between = 1.5),
    "rho_between"
  )
  expect_error(
    corr_pair("x1", "x2", rho_between = -1.5),
    "rho_between"
  )
  expect_error(
    corr_pair("x1", "x2", rho_between = c(0.1, 0.2)),
    "rho_between"
  )
  expect_error(
    corr_pair("x1", "x2", rho_between = NA),
    "rho_between"
  )
})

test_that("corr_pair handles boundary rho values", {
  result_min <- corr_pair("x1", "x2", rho_within = -1)
  result_max <- corr_pair("x1", "x2", rho_within = 1)

  expect_equal(result_min$rho_within, -1)
  expect_equal(result_max$rho_within, 1)
})

test_that("corr_pair handles rho values of zero", {
  result <- corr_pair("x1", "x2", rho_within = 0, rho_between = 0)

  expect_equal(result$rho_within, 0)
  expect_equal(result$rho_between, 0)
})

