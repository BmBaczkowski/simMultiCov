make_covariates <- function(
  n_clusters,
  cluster_size,
  cluster_name = "cluster",
  covariates = list(),
  correlations = list()
) {

  # Handle nested list case
  covariates <- .unwrap_nested_list(covariates, "covariate")
  correlations <- .unwrap_nested_list(correlations, "correlation")

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

  # Validate covariate list
  covariates <- .assert_covariates(covariates)

  # Validate correlation list
  correlations <- .assert_correlations(correlations, covariates)

  # Build output components
  R_mat <- .build_R_mat(correlations, covariates)
  D_mat <- .build_D_mat(covariates)

  # Compute variance-covariance matrix
  Sigma_w <- D_mat$D_w %*% R_mat$R_w %*% D_mat$D_w
  Sigma_b <- D_mat$D_b %*% R_mat$R_b %*% D_mat$D_b

  specs <- list(
    n_covariates = length(names(covariates)),
    names = names(covariates),
    types = vapply(covariates, `[[`, character(1), "type"),
    icc = .get_covariate_specs(covariates, "icc"),
    total_var = .get_covariate_specs(covariates, "total_var"),
    mean = .get_covariate_specs(covariates, "mean"),
    probs = .get_covariate_specs(covariates, "probs"),
    labels = .get_covariate_specs(covariates, "labels"),
    R_w = R_mat$R_w,
    R_b = R_mat$R_b,
    D_b = D_mat$D_b,
    D_w = D_mat$D_w,
    Sigma_b = Sigma_b,
    Sigma_w = Sigma_w
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
      n_clusters = n_clusters,
      cluster_size = cluster_size,
      cluster_name = cluster_name,
      specs = specs
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
