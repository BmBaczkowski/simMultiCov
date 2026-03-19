#' Generate multilevel continuous predictors
#'
#' @inheritParams prepare_multilevel_predictors
#' @param n_L2 Integer scalar. Number of clusters.
#' @param n_L1 Integer scalar or integer vector of length `n_L2`.
#' @param seed Optional integer seed.
#'
#' @return A `data.frame` with one row per level-1 observation.
#' @export
simulate_multilevel_predictors <- function(
  n_L2,
  n_L1,
  sd,
  icc,
  mu = NULL,
  rho_w = NULL,
  rho_mat_w = NULL,
  rho_b = NULL,
  rho_mat_b = NULL,
  predictor_names = NULL,
  cluster_name = "cluster",
  seed = NULL
) {
  spec <- prepare_multilevel_predictors(
    sd = sd,
    icc = icc,
    mu = mu,
    rho_w = rho_w,
    rho_mat_w = rho_mat_w,
    rho_b = rho_b,
    rho_mat_b = rho_mat_b,
    predictor_names = predictor_names,
    cluster_name = cluster_name
  )

  draw_multilevel_predictors(
    spec = spec,
    n_L2 = n_L2,
    n_L1 = n_L1,
    seed = seed
  )
}