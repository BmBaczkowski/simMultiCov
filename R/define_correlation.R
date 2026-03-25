define_correlation <- function(var1, var2, corr_within = NULL, corr_between = NULL) {
  checkmate::assert_string(var1, min.chars = 1L, .var.name = "var1")
  checkmate::assert_string(var2, min.chars = 1L, .var.name = "var2")

  if (identical(var1, var2)) {
    stop("`var1` and `var2` must be different.", call. = FALSE)
  }

  checkmate::assert_numeric(
    corr_within,
    lower = -1,
    upper = 1,
    len = 1L,
    finite = TRUE,
    null.ok = TRUE,
    any.missing = FALSE,
    .var.name = "corr_within"
  )

  checkmate::assert_numeric(
    corr_between,
    lower = -1,
    upper = 1,
    len = 1L,
    finite = TRUE,
    null.ok = TRUE,
    any.missing = FALSE,
    .var.name = "corr_between"
  )

  if (is.null(corr_within) && is.null(corr_between)) {
    stop(
      "At least one of `corr_within` or `corr_between` must be supplied.", 
      call. = FALSE
    )
  }

  .null_to_zero <- function(x) {
    if (is.null(x)) 0 else x
  }

  structure(
    list(
      var1 = var1,
      var2 = var2,
      corr_within = .null_to_zero(corr_within),
      corr_between = .null_to_zero(corr_between)
    ),
    class = "correlation"
  )
}