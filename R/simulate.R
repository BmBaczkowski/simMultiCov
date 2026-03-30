simulate <- function(x, ...) {
  UseMethod("simulate")
}

#' @export
#' @method simulate default
simulate.default <- function(x, ...) {
  message("Default method\n")
}

#' @export
#' @method simulate covariates
simulate.covariates <- function(
  covs,
  n_clusters,
  cluster_size,
  cluster_name = "cluster",
  seed = NULL, 
  n_datasets = 1L,
  ...
) {

  old_seed <- .get_seed()
  on.exit({
    if (!is.null(old_seed)) .Random.seed <<- old_seed
  }, add = TRUE)

  # Validate n_clusters
  n_clusters <- checkmate::assert_int(
    n_clusters, lower = 1L, coerce = TRUE, .var.name = "n_clusters"
  )

  # Validate cluster_size
  checkmate::assert_integerish(
    cluster_size,
    lower = 1L,
    any.missing = FALSE,
    .var.name = "cluster_size"
  )

  # Coerce cluster_size to vector of length n_clusters
  cluster_size <- if (length(cluster_size) == 1L) {
    rep(as.integer(cluster_size), n_clusters)
  } else {
    if (!length(cluster_size) == n_clusters) {
      stop("'cluster_size' must be a scalar or a vector of length 'n_clusters'.", call. = FALSE)
    }
    as.integer(cluster_size)
  }

  # Validate cluster_name
  checkmate::assert_character(
    cluster_name,
    len = 1L,
    min.chars = 1L,
    .var.name = "cluster_name"
  )

  # Validate n_datasets
  n_datasets <- checkmate::assert_int(
    n_datasets, lower = 1L, coerce = TRUE, .var.name = "n_datasets"
  )

  # Obtain and cache Cholesky
  chol_within <- .get_cholesky_factor(covs$R_w)  
  chol_between <- .get_cholesky_factor(covs$R_b) 

  # Single dataset case
  if (n_datasets == 1L) {
    df <- .simulation_engine(
      cluster_sizes = cluster_size,
      mean_vec = covs$mean,
      sd_mat_within = covs$D_w,
      sd_mat_between = covs$D_b,
      chol_R_within = chol_within,
      chol_R_between = chol_between,
      seed = seed
    )
    colnames(df)[colnames(df) == "cluster"] <- cluster_name
    df <- .apply_conversions(df, covs$.thresholds)
    df <- structure(
      df,
      class = c("simulation", class(df)),
      DGP = list(
        mu = covs$mean, 
        Sigma_w = covs$Sigma_w,
        Sigma_b = covs$Sigma_b
      )
    )
    return(df)
  }

  # Multiple datasets case
  datasets <- vector("list", n_datasets)

  for (i in seq_len(n_datasets)) {
    # Increment seed for each dataset
    current_seed <- if (!is.null(seed)) seed + i - 1L else NULL
    
    df <- .simulation_engine(
      cluster_sizes = cluster_size,
      mean_vec = covs$mean,
      sd_mat_within = covs$D_w,
      sd_mat_between = covs$D_b,
      chol_R_within = chol_within,
      chol_R_between = chol_between,
      seed = current_seed
    )
    colnames(df)[colnames(df) == "cluster"] <- cluster_name
    df <- .apply_conversions(df, covs$.thresholds)
    df <- structure(
      df,
      class = c("simulation", class(df)),
      DGP = list(
        mu = covs$mean, 
        Sigma_w = covs$Sigma_w,
        Sigma_b = covs$Sigma_b
      )
    )    
    datasets[[i]] <- df
  }
  
  datasets
}

