ml_covariate_spec <- function(
  n_L2,
  n_L1,
  cluster_name = "cluster",
  covariates = list(...),
  correlations = list(...)
) {

structure(
  list(
    n_L2 = n_L2,
    n_L1 = n_L1,
    cluster_name = cluster_name,
    covariates = covariates_tbl,
    R_b = R_b,
    R_w = R_w
  ),
  class = "ml_predictor_spec"
)

}