#' Internal engine for clustered variables simulation
#'
#' Assumes:
#' - `cluster_sizes` is a positive integer vector
#' - `mean_vec` is a numeric vector of length p
#' - `chol_within` and `chol_between` are numeric p x p covariance factors
#'
#' Inputs are assumed to be pre-validated by the caller.
#'
#' @keywords internal
.sim_mvnorm_hierarchical <- function(
  cluster_sizes,
  mean_vec,
  chol_within,
  chol_between
) {
  n_clusters <- length(cluster_sizes)
  n_obs <- sum(cluster_sizes)
  n_vars <- length(mean_vec)

  # Map each observation to its cluster index
  cluster_id <- rep.int(seq_len(n_clusters), cluster_sizes)

  # Standard normal draws for between- and within-cluster components
  z_between <- matrix(rnorm(n_clusters * n_vars), nrow = n_clusters, ncol = n_vars)
  z_within  <- matrix(rnorm(n_obs * n_vars), nrow = n_obs, ncol = n_vars)

  # Construct cluster-level effects and expand to observations
  X <- z_between %*% chol_between
  X <- X[cluster_id, , drop = FALSE] + z_within %*% chol_within

  # Shift by variable means
  X <- sweep(X, 2L, mean_vec, FUN = "+")

  list(
    X = X,
    cluster_id = cluster_id
  )
}