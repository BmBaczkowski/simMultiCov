.format_n_L1 <- function(n_L1) {
  if (length(n_L1) == 1L) {
    as.character(n_L1)
  } else {
    sprintf("min=%d, max=%d, mode=%d",
            min(n_L1), max(n_L1), .mode_value(n_L1))
  }
}

.print_field <- function(name, value) {
  cli::cli_text("{.strong {name}}: {value}")
}

.print_matrix <- function(name, value) {
  cli::cli_text("{.strong {name}}")
  print(value)
}

.mode_value <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' @export
print.ml_predictor_spec <- function(object) {
  cli::cli_h1("<ml_predictor_spec>")

  .print_field("Clusters", object[['n_L2']])
  .print_field("Cluster sizes", .format_n_L1(object[['n_L1']]))
  .print_field("Predictors", object[['p']])
  .print_field("Names", paste(object[['predictor_names']], collapse = ", "))
  .print_field("ICC", paste(format(object[['icc']]), collapse = ", "))

  cli::cli_alert_info("Use summary() for full details.")

  invisible(object)
}

#' @export
summary.ml_predictor_spec <- function(object, ...) {
  cli::cli_h1("<ml_predictor_spec>")
  cli::cli_text("{.emph Full specification}")

  cli::cli_h2("Sample Structure")
  .print_field("Clusters (n_L2)", object[['n_L2']])
  .print_field("Cluster sizes (n_L1)", .format_n_L1(object[['n_L1']]))

  cli::cli_h2("Predictors")
  .print_field("Predictors", object[['p']])
  .print_field("Names", paste(object[['predictor_names']], collapse = ", "))
  .print_field("Cluster ID", object[['cluster_name']])

  cli::cli_h2("Distribution Parameters")
  .print_field("Means (mu)", paste(format(object[['mu']]), collapse = ", "))
  .print_field("SDs", paste(format(object[['sd']]), collapse = ", "))
  .print_field("ICCs", paste(format(object[['icc']]), collapse = ", "))

  cli::cli_h2("Variance Decomposition")
  .print_field("Total", paste(format(object[['var_total']]), collapse = ", "))
  .print_field("Within", paste(format(object[['var_w']]), collapse = ", "))
  .print_field("Between", paste(format(object[['var_b']]), collapse = ", "))

  cli::cli_h2("Correlation Matrices")
  .print_matrix("Within-cluster", object[['R_w_cor']])
  .print_matrix("Between-cluster", object[['R_b_cor']])

  cli::cli_h2("Covariance Matrices")
  .print_matrix("Within", object[['Sigma_w']])
  .print_matrix("Between", object[['Sigma_b']])

  invisible(object)
}