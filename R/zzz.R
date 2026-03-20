.print_field <- function(name, value) {
  cli::cli_text("{.strong {name}}: {value}")
}

.format_levels <- function(labels, probs) {
  bullets <- paste0(labels, " ", cli::col_grey("(p = ", format(probs), ")"))
  names(bullets) <- rep("•", length(bullets))
  cli::cli_bullets(bullets)
}

#' @export
print.ml_covariate <- function(x, ...) {
  cli::cli_h2("<ml_covariate>")
  
  .print_field("Name", x$name)
  .print_field("Type", x$type)

  
  if (x$type == "continuous") {
    cli::cli_h3("Distribution (gaussian)")
    .print_field("Mean", x$mean)
    .print_field("SD", x$sd)
    .print_field("ICC", x$icc)

  } else if (x$type == "binary") {
    p <- c(x$prob, 1 - x$prob)
    cli::cli_h3("Levels")
    .format_levels(x$labels, p)
  } else if (x$type == "ordinal") {
    cli::cli_h3("Levels (ordered)")
    .format_levels(x$labels, x$probs)
  } else {
    cli::cli_warn("Unknown covariate type: {x$type}")
  }
  
  invisible(x)
}
