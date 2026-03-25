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

  # Validate covariate list
  covariates <- .validate_covariates(covariates)

  # Validate correlation list
  correlations <- .validate_correlations(correlations, covariates)

  # Build output components
  R_mat <- .build_R_mat(correlations, covariates)
  D_mat <- .build_D_mat(covariates)

  specs <- list(
    n_covariates = length(names(covariates)),
    types = .get_feature(covariates, "type"),
    icc = .get_feature(covariates, "icc"),
    sd = .get_feature(covariates, "sd"),
    mean = .get_feature(covariates, "mean"),
    probs = .get_feature(covariates, "prob"),
    probs = .get_feature(covariates, "labels")
  )

  is_non_continuous <- which(specs[["types"]] %in% c("binary", "ordinal"))

  if (length(is_non_continuous)) {
    problematic <- any(
      R_mat$R_w[is_non_continuous, -is_non_continuous, drop = FALSE] != 0
    ) || any(
      R_mat$R_b[is_non_continuous, -is_non_continuous, drop = FALSE] != 0
    )

    if (problematic) {
      warning("Correlations of binary / ordinal covariate(s) are in latent space.", call. = FALSE)
    }
  }

  structure(
    list(
      # Dimensions
      n_L2 = n_L2,
      n_L1 = n_L1,

      # Covariate specs
      specs = specs,

      # Sampling components (Cholesky factors)
      L_w = R_mat$L_w,
      L_b = R_mat$L_b,

      # Standard deviation matrices
      D_w = D_mat$std_diag_w,
      D_b = D_mat$std_diag_b,

      # Correlation matrices (for reference/debugging)
      R_w = R_mat$R_w,
      R_b = R_mat$R_b
    ),
    class = "ml_covariates"
  )

}