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
  if (!length(n_L1) %in% c(1L, n_L2)) {
    stop("'n_L1' must be a scalar or a vector of length 'n_L2'.", call. = FALSE)
  }

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
    as.integer(n_L1)
  }

  # Validate cluster_name
  checkmate::assert_character(
    cluster_name,
    len = 1L,
    min.chars = 1L,
    .var.name = "cluster_name"
  )

  # Validate covariates (returns unique covariate names)
  covariate_names <- .validate_covariates(covariates)

  # Validate correlations
  corr_names <- .validate_correlations(correlations, covariate_names)

  # Build correlation matrix
  if (!is.null(corr_names)){
    print("xxxxxxx")
  }

  structure(
    list(
    ),
    class = "ml_covariates"
  )

}