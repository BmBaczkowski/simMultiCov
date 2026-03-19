#' Draw multilevel predictors from a prepared specification
#'
#' @param spec A prepared multilevel predictor specification.
#' @param n_L2 Integer scalar. Number of clusters.
#' @param n_L1 Integer scalar or vector of length `n_L2`.
#' @param seed Optional integer seed.
#'
#' @return A `data.frame` of simulated predictors.
#' @export
draw_multilevel_predictors <- function(spec, n_L2, n_L1, seed = NULL) {
  if (!inherits(spec, "ml_predictor_spec")) {
    stop("'spec' must inherit from class 'ml_predictor_spec'.", call. = FALSE)
  }

  n_L2 <- checkmate::assert_int(n_L2, lower = 1L, coerce = TRUE, .var.name = "n_L2")
  n_L1 <- .assert_n_L1(n_L1, n_L2)

  seed <- checkmate::assert_int(
    seed,
    lower = 1L,
    null.ok = TRUE,
    coerce = TRUE,
    .var.name = "seed"
  )

  if (!is.null(seed)) {
    set.seed(seed)
  }

  out <- .sim_mvnorm_hierarchical(
    cluster_sizes = n_L1,
    mean_vec = spec$mu,
    chol_within = spec$.Rw,
    chol_between = spec$.Rb
  )

  df <- data.frame(out$cluster_id, out$X, check.names = FALSE)
  names(df) <- c(spec$cluster_name, spec$predictor_names)

  attr(df, "dgp") <- list(
    mu = spec$mu,
    icc = spec$icc,
    Sigma_w = spec$Sigma_w,
    Sigma_b = spec$Sigma_b
  )

  df
}