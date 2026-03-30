#' Internal engine for clustered variables simulation

.simulation_engine <- function(
  cluster_sizes,
  mean_vec,
  sd_mat_within,
  sd_mat_between,
  chol_R_within,
  chol_R_between,
  seed = NULL
) {

  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  n_clusters <- length(cluster_sizes)
  n_obs <- sum(cluster_sizes)
  n_vars <- length(mean_vec)

  # Map each observation to its cluster index
  cluster_id <- rep.int(seq_len(n_clusters), cluster_sizes)

  # Standard normal draws for between- and within-cluster components
  z_between <- matrix(rnorm(n_clusters * n_vars), nrow = n_vars, ncol = n_clusters)
  z_within  <- matrix(rnorm(n_obs * n_vars), nrow = n_vars, ncol = n_obs)
  
  # Construct cluster-level effects and expand to observations
  X <- sd_mat_between %*% chol_R_between %*% z_between 
  X <-  X[ , cluster_id, drop = FALSE] + sd_mat_within %*% chol_R_within %*% z_within

  # Shift by variable means
  X <- sweep(t(X), 2L, mean_vec, FUN = "+")

  data.frame(cluster = cluster_id, X)
}