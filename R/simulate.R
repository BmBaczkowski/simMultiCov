#' Simulate data from a model specification
#'
#' Generates simulated data based on an object describing a data-generating
#' process. The behavior depends on the class of `x`.
#'
#' @param x An object describing a data-generating process. The required
#'   structure depends on the method used.
#' @param ... Additional arguments passed to method-specific implementations.
#'
#' @return A simulated dataset or a list of datasets. The exact structure
#'   depends on the method.
#'
#' @details
#' This is an S3 generic. Available methods include:
#' \itemize{
#'   \item \code{simulate.covariates()} for multilevel covariate simulation
#' }
#'
#' @export
simulate <- function(x, ...) {
  UseMethod("simulate")
}


#' @export
#' @method simulate default
simulate.default <- function(x, ...) {
  message("Default method\n")
}


#' @rdname simulate
#'
#' @param x A covariate specification object of class `"covariates"`
#'   created using [make_covariates()].
#' @param n_clusters An integer specifying the number of clusters to simulate.
#'   Must be a positive integer.
#' @param cluster_size Either a scalar integer specifying the number of
#'   observations per cluster (applied to all clusters), or an integer vector
#'   of length \code{n_clusters} specifying the number of observations in each
#'   cluster. All values must be positive integers.
#' @param cluster_name A character string specifying the name of the cluster
#'   identifier column in the output data frame. Defaults to \code{"cluster"}.
#'   Must be a non-empty string without spaces.
#' @param seed An optional integer seed for reproducibility. If provided, the
#'   random number generator is set to this seed before simulation. Defaults
#'   to \code{NULL} (no seed is set).
#' @param n_datasets An integer specifying the number of datasets to generate.
#'   Defaults to \code{1L}. When \code{n_datasets > 1}, a list of data frames
#'   is returned.
#'
#' @return
#' When \code{n_datasets = 1} (default), returns a data frame of class
#' \code{c("simulation", "data.frame")} with the following structure:
#' \describe{
#'   \item{cluster}{Cluster identifier column (name determined by \code{cluster_name})}
#'   \item{<covariate_names>}{One column per covariate defined in the specification}
#' }
#'
#' The returned data frame includes a \code{"DGP"} attribute containing the
#' data-generating parameters:
#' \describe{
#'   \item{mu}{Vector of variable means}
#'   \item{Sigma_w}{Within-cluster variance-covariance matrix}
#'   \item{Sigma_b}{Between-cluster variance-covariance matrix}
#' }
#'
#' When \code{n_datasets > 1}, returns a list of data frames, each with the
#' same structure described above.
#'
#' @details
#' ## Method: covariates
#'
#' This method simulates multilevel (clustered) data from a covariate
#' specification created with [make_covariates()]. It supports both single
#' and multiple dataset generation, as well as equal or unequal cluster sizes.
#'
#' Simulation is based on a two-level data-generating process:
#' \enumerate{
#'   \item Cluster-level effects are sampled from the between-cluster
#'     covariance structure
#'   \item Observation-level effects are sampled from the within-cluster
#'     covariance structure
#'   \item The two components are combined and shifted by the specified means
#' }
#'
#' Binary and ordinal covariates are generated using a latent variable
#' approach: continuous latent values are simulated and then transformed
#' into observed categories using thresholds derived from the specified
#' probability distributions.
#'
#' The method additionally supports:
#' \itemize{
#'   \item Flexible cluster sizes (constant or varying across clusters)
#'   \item Reproducible simulation via an optional random seed
#'   \item Generation of multiple independent datasets
#'   \item Automatic conversion of latent variables to factors for
#'     binary and ordinal covariates
#' }
#'
#' @export
#'
#' @examples
#' # Create a covariate specification
#' covs <- make_covariates(
#'   covariates = list(
#'     make_continuous("x1", mean = 0, total_var = 1, icc = 0.1),
#'     make_continuous("x2", mean = 0, total_var = 1, icc = 0.1),
#'     make_binary("treatment", prob = 0.5, icc = 0.1)
#'   ),
#'   correlations = list(
#'     define_correlation("x1", "x2", corr_within = 0.5, corr_between = 0.3)
#'   )
#' )
#'
#' # Simulate a single dataset with 5 clusters of 10 observations each
#' df <- simulate(covs, n_clusters = 5, cluster_size = 10)
#' head(df)
#'
#' # Simulate with unequal cluster sizes
#' df <- simulate(covs, n_clusters = 3, cluster_size = c(5, 10, 15))
#'
#' # Simulate multiple datasets
#' datasets <- simulate(covs, n_clusters = 5, cluster_size = 10, n_datasets = 3)
#' length(datasets)  # 3 datasets
#'
#' # Use a custom cluster name
#' df <- simulate(covs, n_clusters = 5, cluster_size = 10, cluster_name = "group")
#' head(df)
#'
#' # Reproducible simulation with a seed
#' df1 <- simulate(covs, n_clusters = 5, cluster_size = 10, seed = 123)
#' df2 <- simulate(covs, n_clusters = 5, cluster_size = 10, seed = 123)
#' identical(df1, df2)  # TRUE
#'
#' @seealso [make_covariates()], [make_continuous()], [make_binary()], [make_ordinal()]
simulate.covariates <- function(
  x,
  n_clusters,
  cluster_size,
  cluster_name = "cluster",
  seed = NULL, 
  n_datasets = 1L,
  ...
) {
  covs <- x

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
