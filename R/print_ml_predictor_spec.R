.print_field <- function(name, value) {
  cli::cli_text("{.strong {name}}: {value}")
}

.print_matrix <- function(name, value) {
  cli::cli_text("{.strong {name}}")
  print(value)
}

#' @export
print.ml_predictor_spec <- function(x, ...) {
  cli::cli_h1("<ml_predictor_spec>")

  .print_field("Predictors", x$p)
  .print_field("Names", paste(x$predictor_names, collapse = ", "))
  .print_field("Cluster name", x$cluster_name)
  .print_field("ICC", format(x$icc))
  .print_field("Mean / SD", paste(format(x$mu), format(x$sd), sep = " / "))

  cli::cli_alert_info("Use {.code summary()} for full details.")

  invisible(x)
}

#' @export
summary.ml_predictor_spec <- function(object, ...) {
  x <- object

  cli::cli_h1("<ml_predictor_spec>")
  cli::cli_text("{.emph Full specification}")

  cli::cli_h2("Metadata")
  .print_field("Predictors", x$p)
  .print_field("Names", paste(x$predictor_names, collapse = ", "))
  .print_field("Cluster", x$cluster_name)

  cli::cli_h2("Distribution")
  .print_field("mu", format(x$mu))
  .print_field("sd", format(x$sd))
  .print_field("icc", format(x$icc))

  cli::cli_h2("Matrices")
  .print_matrix("Sigma_w", x$Sigma_w)
  .print_matrix("Sigma_b", x$Sigma_b)

  cli::cli_h2("Internal")
  .print_matrix(".Rw", x$.Rw)
  .print_matrix(".Rb", x$.Rb)

  invisible(x)
}