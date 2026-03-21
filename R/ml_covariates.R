ml_covariates <- function(
  n_L2,
  n_L1,
  cluster_name = "cluster",
  covariates = list(),
  correlations = list()
) {

  # Handle nested list case
  covariates <- .unwrap_nested_list(covariates, "ml_covariate")
  correlations <- .unwrap_nested_list(correlations, "ml_corr_pair")

  # Validate n_L2
  n_L2 <- checkmate::assert_int(
    n_L2, lower = 1L, coerce = TRUE, .var.name = "n_L2"
  )

  # Validate n_L1
  checkmate::assert_integerish(
    n_L1,
    lower = 1L,
    any.missing = FALSE,
    .var.name = "n_L1"
  )



  # Coerce n_L1 to vector of length n_L2
  n_L1 <- if (length(n_L1) == 1L) {
    rep(as.integer(n_L1), n_L2)
  } else {
    if (!length(n_L1) == n_L2) {
      stop("'n_L1' must be a scalar or a vector of length 'n_L2'.", call. = FALSE)
    }
    as.integer(n_L1)
  }

  # Validate cluster_name
  checkmate::assert_character(
    cluster_name,
    len = 1L,
    min.chars = 1L,
    .var.name = "cluster_name"
  )

  covariates <- .validate_covariates(covariates)
  correlations <- .validate_correlations(correlations, covariates)
  R_mat <- .build_R_mat(correlations, covariates)

  structure(
    list(
      R_mat = R_mat
    ),
    class = "ml_covariates"
  )

}