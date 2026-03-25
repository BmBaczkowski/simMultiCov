.is_whole_number <- function(x) {
  is.numeric(x) && all(x %% 1 == 0)
}

.print_field <- function(name, value) {
  if (is.numeric(value)) {
    if (.is_whole_number(value)) {
      cli::cli_text("{.strong {name}}: {value}")
    } else {
      formatted <- sprintf("%.2f", value)
      cli::cli_text("{.strong {name}}: {.val {formatted}}")
    }

  } else if (is.character(value)) {
     cli::cli_text("{.strong {name}}: {value}")
  }

}