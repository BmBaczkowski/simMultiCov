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

  # Validate covariates is a list
  checkmate::assert_list(
    covariates,
    min.len = 1L, 
    unique = TRUE,
    any.missing = FALSE,
    .var.name = "covariates"
  )

  # Validate each element in covariates is an ml_covariate
  covs_ok <- vapply(
    covariates, 
    inherits, 
    logical(1), 
    what = "ml_covariate"
  )

  if (!all(covs_ok)) {
    stop(
      # TODO
      "All inputs to `ml_covariates()` must be created by `funcname`.", 
      call. = FALSE
    )
  }

  # Validate covariate names are unique
  cov_names <- vapply(covariates, function(x) x$name, character(1))
  if (anyDuplicated(cov_names)) {
    dup <- unique(cov_names[duplicated(cov_names)])
    stop(
      sprintf(
        "Duplicate covariate names found: %s",
        paste(unique(dup), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Validate correlations is a list
  checkmate::assert_list(
    correlations,
    any.missing = FALSE, 
    unique = TRUE,
    .var.name = "correlations"
  )

  structure(
    list(
    ),
    class = "ml_covariates"
  )

}