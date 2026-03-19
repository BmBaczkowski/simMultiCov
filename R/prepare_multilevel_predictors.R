#' Prepare a reusable multilevel predictor specification
#'
#' @param sd Numeric vector of predictor standard deviations.
#' @param icc Numeric vector of predictor intraclass correlations.
#' @param mu Numeric vector of predictor means, or NULL for zeros.
#' @param rho_w Common within-cluster correlation, or NULL.
#' @param rho_mat_w Within-cluster correlation matrix, or NULL.
#' @param rho_b Common between-cluster correlation, or NULL.
#' @param rho_mat_b Between-cluster correlation matrix, or NULL.
#' @param predictor_names Optional predictor names.
#' @param cluster_name Name of the cluster ID column.
#'
#' @return An object of class `"ml_predictor_spec"`.
#' @export
prepare_multilevel_predictors <- function(
  sd,
  icc,
  mu = NULL,
  rho_w = NULL,
  rho_mat_w = NULL,
  rho_b = NULL,
  rho_mat_b = NULL,
  predictor_names = NULL,
  cluster_name = "cluster"
) {
  checkmate::assert_numeric(
    sd,
    lower = 0,
    finite = TRUE,
    any.missing = FALSE,
    min.len = 1L,
    .var.name = "sd"
  )

  p <- length(sd)

  checkmate::assert_numeric(
    icc,
    lower = 0,
    upper = 1,
    finite = TRUE,
    any.missing = FALSE,
    len = p,
    .var.name = "icc"
  )

  checkmate::assert_numeric(
    mu,
    finite = TRUE,
    any.missing = FALSE,
    len = p,
    null.ok = TRUE,
    .var.name = "mu"
  )
  if (is.null(mu)) {
    mu <- rep(0, p)
  }

  checkmate::assert_character(
    predictor_names,
    len = p,
    min.chars = 1L,
    any.missing = FALSE,
    unique = TRUE,
    null.ok = TRUE,
    .var.name = "predictor_names"
  )
  if (is.null(predictor_names)) {
    predictor_names <- paste0("x", seq_len(p))
  }

  checkmate::assert_string(cluster_name, min.chars = 1L, .var.name = "cluster_name")
  if (cluster_name %in% predictor_names) {
    stop(
      sprintf(
        "'cluster_name' (%s) must not match any value in 'predictor_names'.",
        sQuote(cluster_name)
      ),
      call. = FALSE
    )
  }

  if (p == 1L) {
    if (!is.null(rho_w) || !is.null(rho_mat_w) || !is.null(rho_b) || !is.null(rho_mat_b)) {
      warning("Correlation arguments ignored because p = 1.", call. = FALSE)
    }
    R_w_cor <- matrix(1, nrow = 1, ncol = 1)
    R_b_cor <- matrix(1, nrow = 1, ncol = 1)
  } else {
    R_w_cor <- .resolve_correlation(rho_w, rho_mat_w, p, "rho_w", "rho_mat_w")
    R_b_cor <- .resolve_correlation(rho_b, rho_mat_b, p, "rho_b", "rho_mat_b")
  }

  var_total <- sd^2
  var_b <- icc * var_total
  var_w <- (1 - icc) * var_total

  Sigma_b <- .cov_from_sd_and_cor(sqrt(var_b), R_b_cor)
  Sigma_w <- .cov_from_sd_and_cor(sqrt(var_w), R_w_cor)

  spec <- list(
    p = p,
    mu = mu,
    sd = sd,
    icc = icc,
    Sigma_w = Sigma_w,
    Sigma_b = Sigma_b,
    predictor_names = predictor_names,
    cluster_name = cluster_name,
    .Rw = .cov_factor(Sigma_w, "Sigma_w"),
    .Rb = .cov_factor(Sigma_b, "Sigma_b")
  )

  class(spec) <- "ml_predictor_spec"
  spec
}