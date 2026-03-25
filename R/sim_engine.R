#' Internal engine for clustered variables simulation
#'
#' Assumes:
#' - `cluster_sizes` is a positive integer vector
#' - `mean_vec` is a numeric vector of length p
#' - `sd_vec` is a numeric vector of length p
#' - `chol_within` and `chol_between` are numeric p x p covariance factors
#'
#' Inputs are assumed to be pre-validated by the caller.
#'
#' @keywords internal
.sim_engine <- function(
  cluster_sizes,
  mean_vec,
  D_w,
  D_b,
  L_w,
  L_b
) {
  n_clusters <- length(cluster_sizes)
  n_obs <- sum(cluster_sizes)
  n_vars <- length(mean_vec)

  # Map each observation to its cluster index
  cluster_id <- rep.int(seq_len(n_clusters), cluster_sizes)

  # Standard normal draws for between- and within-cluster components
  z_b <- matrix(rnorm(n_clusters * n_vars), nrow = n_vars, ncol = n_clusters)
  z_w  <- matrix(rnorm(n_obs * n_vars), nrow = n_vars, ncol = n_obs)

  # Construct cluster-level effects and expand to observations
  X <- D_b %*% L_b %*% z_b 
  X <-  X[ , cluster_id, drop = FALSE] + D_w %*% L_w %*% z_w

  # Shift by variable means
  X <- sweep(t(X), 2L, mean_vec, FUN = "+")

  list(
    X = X,
    cluster_id = cluster_id
  )
}