#' Define a correlation between two variables
#'
#' Creates a correlation specification between two variables in a multilevel
#' simulation. The correlation can be specified separately for within-cluster
#' and between-cluster levels.
#'
#' @param var1 A character string specifying the name of the first variable.
#'   Must be a non-empty string.
#' @param var2 A character string specifying the name of the second variable.
#'   Must be a non-empty string and different from \code{var1}.
#' @param corr_within A numeric value between -1 and 1 specifying the
#'   within-cluster correlation between the two variables. Defaults to 0 if
#'   not provided. Must be a single finite value with no missing values.
#' @param corr_between A numeric value between -1 and 1 specifying the
#'   between-cluster correlation between the two variables. Defaults to 0 if
#'   not provided. Must be a single finite value with no missing values.
#'
#' @return An object of class \code{"correlation"} containing the following
#'   elements:
#'   \describe{
#'     \item{var1}{Name of the first variable.}
#'     \item{var2}{Name of the second variable.}
#'     \item{corr_within}{Within-cluster correlation (defaults to 0 if not specified).}
#'     \item{corr_between}{Between-cluster correlation (defaults to 0 if not specified).}
#'   }
#'
#' @details
#' At least one of \code{corr_within} or \code{corr_between} must be provided.
#' If only one is specified, the other defaults to 0.
#'
#' This function is typically used in conjunction with [make_covariates()] to
#' build a complete multilevel data generating process.
#'
#' @export
#'
#' @examples
#' # Define a within-cluster correlation only
#' define_correlation("x1", "x2", corr_within = 0.5)
#'
#' # Define a between-cluster correlation only
#' define_correlation("age", "income", corr_between = 0.3)
#'
#' # Define both within and between correlations
#' define_correlation("x1", "x2", corr_within = 0.5, corr_between = 0.2)
#'
#' # Define a negative correlation
#' define_correlation("stress", "sleep", corr_within = -0.6)
#'
#' @seealso [make_covariates()] for building complete covariance structures
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
