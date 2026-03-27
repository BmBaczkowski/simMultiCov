make_covariates <- function(
  covariates = list(),
  correlations = list()
) {

  # Handle nested list case
  covariates <- .unwrap_nested_list(covariates, "covariate")
  correlations <- .unwrap_nested_list(correlations, "correlation")

  # Validate covariate list
  covariates <- .assert_covariates(covariates)

  # Validate correlation list
  correlations <- .assert_correlations(correlations, covariates)

  # Build output components
  R_w <- .build_R_mat(correlations, covariates, "within")
  R_b <- .build_R_mat(correlations, covariates, "between")
  D_w <- .build_D_mat(covariates, "within")
  D_b <- .build_D_mat(covariates, "between")

  # Compute variance-covariance matrix
  Sigma_w <- D_w %*% R_w %*% D_w
  Sigma_b <- D_b %*% R_b %*% D_b

  # 
  specs <- list(
    n_covariates = length(names(covariates)),
    names = names(covariates),
    types = attr(covariates, "types"),
    icc = .get_covariate_specs(covariates, "icc"),
    total_var = .get_covariate_specs(covariates, "total_var"),
    mean = .get_covariate_specs(covariates, "mean"),
    probs = .get_covariate_specs(covariates, "probs"),
    R_b = R_b,
    R_w = R_w,
    D_b = D_b,
    D_w = D_w,
    Sigma_b = Sigma_b,
    Sigma_w = Sigma_w,
    .L_b = NULL,
    .L_w = NULL
  )


  # Pre-compute conversion thresholds for binary/ordinal
  thresholds <- .find_thresholds(specs, "binary")
  thresholds <- c(
    thresholds,
    .find_thresholds(specs, "ordinal")
  )

  if (!is.null(thresholds)) {
    indx <- which(names(thresholds) %in% specs$names)
    problematic <- any(R_w[indx, -indx, drop = FALSE] != 0) || 
      any(R_b[indx, indx, drop = FALSE] != 0
    )

    if (problematic) {
      warning("Correlations of binary / ordinal covariate(s) are in latent space.", call. = FALSE)
    }
  }

  structure(
    c(specs,
      list(
        .thresholds = thresholds
      )
    ),
    class = "covariates"
  )

}


#' @export
print.covariates <- function(x, ...) {
  cli::cli_h1("<covariates>")

  # Sample structure
  cli::cli_h2("Sample Structure")
  .print_field("Cluster name", x$cluster_name)
  .print_field("Cluster number", x$n_clusters)

  cli::cli_text(
    paste(
      "{.strong Observations per cluster}:",
      "min={min(x$cluster_size)}, max={max(x$cluster_size)}, total={sum(x$cluster_size)}"
    )
  )

  # Covariate summary
  cli::cli_h2("Covariates ({x$specs$n_covariates})")
  cov_names <- names(x$specs$types)
  cov_types <- x$specs$types

  for (i in seq_along(cov_names)) {
    cli::cli_text("• {.strong {cov_names[[i]]}}: {cov_types[[i]]}")
  }

  # Correlations
  if (!is.null(x$R_w) && any(x$R_w[lower.tri(x$R_w)] != 0)) {
    cli::cli_h2("Correlations")
    cli::cli_alert_info("Within/between correlation matrices available. Use summary() for details.")
  }
  else {
    cli::cli_alert_info("Use summary() for details.")
  }

  invisible(x)
}

#' @export
summary.covariates <- function(x, ...) {

  cli::cli_h1("<covariates>")
  cli::cli_text("{.emph Full specification}")

  # Sample
  cli::cli_h2("Sample")
  .print_field("N clusters", x$n_clusters)
  cli::cli_text("{.strong Cluster sizes}: {paste(x$cluster_size, collapse = ', ')}")
  .print_field("Total observations", sum(x$cluster_size))

  # Covariate details table
  cli::cli_h2("Structure")

  cov_names <- x$specs$names
  cov_types <- x$specs$types

  is_non_continuous <- which(cov_types %in% c("binary", "ordinal"))
  cov_types[is_non_continuous] <- paste(cov_types[is_non_continuous], "(latent)")

  # Build data frame for display
  df <- data.frame(
    Name = cov_names,
    Type = cov_types,
    ICC = unlist(x$specs$icc),
    Mean = unlist(x$specs$mean),
    Var = unlist(x$specs$total_var),
    stringsAsFactors = FALSE
  )

  print(df, row.names = FALSE)
  cli::cli_text("")
  
  # Correlation matrices
  cli::cli_h2("Correlations")
  cli::cli_h3("Within-Cluster")
  print(round(x$specs$R_w, 3))

  cli::cli_h3("Between-Cluster")
  print(round(x$specs$R_b, 3))

  # Variance-covariance matrices
  cli::cli_h2("Variance-Covariance")
  cli::cli_h3("Within-Cluster ")
  print(round(x$specs$Sigma_w, 3))

  cli::cli_text("")
  cli::cli_h3("Between-Cluster")
  print(round(x$specs$Sigma_b, 3))

  invisible(x)
}
