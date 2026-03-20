# --- Basic input tests ---

test_that("valid input with defaults returns ml_predictor_spec", {
  spec <- new_ml_predictor_spec(
    n_L2 = 10,
    n_L1 = 5,
    sd = c(1, 2),
    icc = c(0.2, 0.3)
  )
  
  expect_s3_class(spec, "ml_predictor_spec")
  expect_equal(spec[['n_L2']], 10)
  expect_equal(spec[['p']], 2)
  expect_equal(spec[['cluster_name']], "cluster")
  expect_equal(spec[['predictor_names']], c("x1", "x2"))
})

test_that("valid input with custom names returns ml_predictor_spec", {
  predictor_names = c("A", "B")
  cluster_name = "school_id"

  spec <- new_ml_predictor_spec(
    n_L2 = 10,
    n_L1 = 5,
    sd = c(1, 2),
    icc = c(0.2, 0.3),
    predictor_names = predictor_names,
    cluster_name = cluster_name
  )
  
  expect_s3_class(spec, "ml_predictor_spec")
  expect_equal(spec[['cluster_name']], cluster_name)
  expect_equal(spec[['predictor_names']], predictor_names)
})

# --- Computation tests ---
test_that("variance components are computed correctly", {
  sd_val <- c(1, 2)
  icc_val <- c(0.2, 0.3)
  
  spec <- new_ml_predictor_spec(
    n_L2 = 10,
    n_L1 = 5,
    sd = sd_val,
    icc = icc_val
  )
  
  expected_var_total <- sd_val^2
  expected_var_b <- icc_val * expected_var_total
  expected_var_w <- (1 - icc_val) * expected_var_total
  
  expect_equal(spec[['var_total']], expected_var_total)
  expect_equal(spec[['var_b']], expected_var_b)
  expect_equal(spec[['var_w']], expected_var_w)
})


# --- Error tests ---

test_that("error when icc outside [0, 1]", {
  expect_error(
    new_ml_predictor_spec(
      n_L2 = 10,
      n_L1 = 5,
      sd = c(1, 2),
      icc = c(0.2, 1.5)
    ),
    regexp = "icc"
  )
})

test_that("error when cluster size vector mismatch with cluster number", {
  expect_error(
    new_ml_predictor_spec(
      n_L2 = 2,
      n_L1 = c(2, 10, 3),
      sd = c(1, 2),
      icc = c(0.2, 0.1)
    ),
    regexp = "n_L[12]"
  )
})

# Warning tests

test_that("warning when correlation args supplied but p = 1", {
  expect_warning(
    new_ml_predictor_spec(
      n_L2 = 5,
      n_L1 = 10,
      sd = 1,
      icc = 0.2,
      rho_w = .5,
      rho_b = 0
    ),
    regexp = "p = 1"
  )
})

