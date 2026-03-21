corr_pair <- function(var1, var2, rho_within = NULL, rho_between = NULL) {
  checkmate::assert_string(var1, min.chars = 1L, .var.name = "var1")
  checkmate::assert_string(var2, min.chars = 1L, .var.name = "var2")

  if (identical(var1, var2)) {
    stop("`var1` and `var2` must be different.", call. = FALSE)
  }

  checkmate::assert_numeric(
    rho_within,
    lower = -1,
    upper = 1,
    len = 1L,
    finite = TRUE,
    null.ok = TRUE,
    any.missing = FALSE,
    .var.name = "rho_within"
  )

  checkmate::assert_numeric(
    rho_between,
    lower = -1,
    upper = 1,
    len = 1L,
    finite = TRUE,
    null.ok = TRUE,
    any.missing = FALSE,
    .var.name = "rho_between"
  )

  if (is.null(rho_within) && is.null(rho_between)) {
    stop(
      "At least one of `rho_within` or `rho_between` must be supplied.", 
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
      rho_within = .null_to_zero(rho_within),
      rho_between = .null_to_zero(rho_between)
    ),
    class = c("ml_corr_pair", "ml_spec")
  )
}