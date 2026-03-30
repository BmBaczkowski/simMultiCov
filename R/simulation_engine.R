#' Internal engine for clustered variables simulation
#'
#' @description
#' Simulates clustered data with specified within-cluster and between-cluster
#' covariance structures using a Cholesky decomposition approach.
#'
#' @param cluster_sizes Integer vector specifying the number of observations
#'   in each cluster.
#' @param mean_vec Numeric vector of variable means.
#' @param sd_mat_within Diagonal matrix of within-cluster standard deviations.
#' @param sd_mat_between Diagonal matrix of between-cluster standard deviations.
#' @param chol_R_within Cholesky factor of the within-cluster correlation matrix.
#' @param chol_R_between Cholesky factor of the between-cluster correlation matrix.
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A data.frame with columns:
#'   \item{cluster}{Cluster identifier}
#'   \item{X1, X2, ...}{Simulated variables}
#'
#' @details
#' This function implements a two-level simulation model:
#' \enumerate{
#'   \item Generate cluster-level effects using between-cluster covariance
#'   \item Generate observation-level effects using within-cluster covariance
#'   \item Combine and shift by variable means
#' }
#'
#' @keywords internal
#' @noRd
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
