#' Validate Covariate Name
#'
#' Checks that a covariate name is a single non-empty character string
#' without spaces.
#'
#' @param name Character string. The covariate name to validate.
#' @param context Character string. Context for error messages (e.g., function name).
#'
#' @return Invisible \code{TRUE} if validation passes; throws an error otherwise.
#'
#' @noRd
.assert_covariate_name <- function(name, context) {

  checkmate::assert_character(
    name,
    len = 1L,
    min.chars = 1L,
    .var.name = paste0("'name' in ", context)
  )

  if (grepl(" ", name)) {
    stop(
      paste0("'name' in ", context, " must not contain spaces"),
      call. = FALSE
    )
  }

}

#' Validate Covariate Mean
#'
#' Checks that a covariate mean is a single finite numeric value with no
#' missing values.
#'
#' @param mean Numeric. The mean value to validate.
#' @param context Character string. Context for error messages (e.g., function name).
#'
#' @return Invisible \code{TRUE} if validation passes; throws an error otherwise.
#'
#' @noRd
.assert_covariate_mean <- function(mean, context) {

  checkmate::assert_numeric(
    mean, 
    len = 1L,
    any.missing = FALSE,
    .var.name = paste0("'mean' in ", context)
  )

}

#' Validate Covariate Total Variance
#'
#' Checks that a covariate total variance is a single finite positive numeric
#' value with no missing values.
#'
#' @param total_var Numeric. The total variance value to validate.
#' @param context Character string. Context for error messages (e.g., function name).
#'
#' @return Invisible \code{TRUE} if validation passes; throws an error otherwise.
#'
#' @noRd
.assert_covariate_total_var <- function(total_var, context) {

  checkmate::assert_numeric(
    total_var, 
    len = 1L,
    lower = 1e-10,
    finite = TRUE,
    any.missing = FALSE,
    .var.name = paste0("'total_var' in ", context)
  )

}

#' Validate Covariate ICC
#'
#' Checks that a covariate intraclass correlation coefficient (ICC) is a
#' single finite numeric value between 0 and 1 with no missing values.
#'
#' @param icc Numeric. The ICC value to validate.
#' @param context Character string. Context for error messages (e.g., function name).
#'
#' @return Invisible \code{TRUE} if validation passes; throws an error otherwise.
#'
#' @noRd
.assert_covariate_icc <- function(icc, context) {

  checkmate::assert_numeric(
    icc, 
    len = 1L,
    lower = 0,
    upper = 1,
    finite = TRUE,
    any.missing = FALSE,
    .var.name = paste0("'icc' in ", context)
  )

}

#' Validate Covariate Probabilities
#'
#' Checks that covariate probabilities are valid numeric values between 0 and 1
#' that sum to 1. For binary covariates, ensures the probability is a scalar.
#'
#' @param probs Numeric vector. The probability values to validate.
#' @param context Character string. Context for error messages (e.g., function name).
#'
#' @return Invisible \code{TRUE} if validation passes; throws an error otherwise.
#'
#' @noRd
.assert_covariate_probs <- function(probs, context) {

 checkmate::assert_numeric(
    probs, 
    min.len = 1L,
    lower = 1e-5,
    upper = 1 - 1e-5,
    finite = TRUE,
    any.missing = FALSE,
    .var.name = paste(checkmate::vname(probs), "in", context)
  )

  if (context == "make_binary()") {
    if (!length(probs) == 1L) {
      stop(
        paste0("'prob' in ", context, " must be a scalar"),
        call. = FALSE
      )
    }
    probs <- c(probs, 1-probs)
  }

  if (!isTRUE(all.equal(sum(probs), 1))) {
    stop(
      paste0("'probs' in ", context, " must sum to 1"),
      call. = FALSE
    )
  }

}

#' Validate Covariate Labels
#'
#' Checks that covariate labels are valid character strings matching the
#' length of the probability vector.
#'
#' @param labels Character vector. The labels to validate.
#' @param probs Numeric vector. The probability vector used to check label length.
#' @param context Character string. Context for error messages (e.g., function name).
#'
#' @return Invisible \code{TRUE} if validation passes; throws an error otherwise.
#'
#' @noRd
.assert_covariate_labels <- function(labels, probs, context) {

  checkmate::assert_character(
    labels,
    len = length(probs),
    min.chars = 1L,
    null.ok = TRUE,
    any.missing = FALSE,
    .var.name = paste0("'labels' in ", context)
  )

}

#' Validate List of Objects with Specific Class
#'
#' Checks that all elements in a list inherit from a specified class and
#' that the list meets minimum length requirements.
#'
#' @param x List. The list to validate.
#' @param class_name Character string. The class name that all elements must inherit from.
#' @param min_len Integer. Minimum length of the list (default: 1).
#'
#' @return Invisible \code{TRUE} if validation passes; throws an error otherwise.
#'
#' @noRd
.assert_class_list <- function(x, class_name, min_len = 1L) {
  var_name <- deparse(substitute(x))

  checkmate::assert_list(
    x,
    min.len = min_len,
    unique = TRUE,
    any.missing = FALSE,
    .var.name = var_name
  )

  ok <- vapply(x, inherits, logical(1), what = class_name)

  if (!all(ok)) {
    bad <- which(!ok)
    stop(
      sprintf(
        "All inputs to `%s` must be of class `%s` (invalid element(s): %s).",
        var_name,
        class_name,
        paste(bad, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Assert Unique Names
#'
#' Checks that a character vector contains no duplicate values.
#'
#' @param x Character vector. The vector to check for duplicates.
#' @param err_msg Character string. Error message template with \code{\%s} placeholder
#'   for duplicate names.
#' @param sep Character string. Separator for displaying duplicate names (default: ", ").
#'
#' @return Invisible \code{TRUE} if validation passes; throws an error otherwise.
#'
#' @noRd
.assert_unique_names <- function(x, err_msg, sep = ", ") {
  if (anyDuplicated(x)) {
    dup <- unique(x[duplicated(x)])
    stop(
      sprintf(err_msg, paste(dup, collapse = sep)),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Validate Covariates List
#'
#' Validates a list of covariate objects, ensuring all elements are of class
#' \code{"covariate"} and have unique names. Adds names and types as attributes
#' to the returned list.
#'
#' @param covariates List. A list of covariate objects to validate.
#'
#' @return The validated covariates list with \code{names} and \code{types} attributes.
#'
#' @noRd
.assert_covariates <- function(covariates) {
  .assert_class_list(
    covariates,
    class_name = "covariate",
    min_len = 1L
  )

  cov_names <- vapply(covariates, `[[`, character(1), "name")

  .assert_unique_names(
    cov_names,
    err_msg = "Duplicate covariate names found: %s"
    )

  cov_types <- vapply(covariates, `[[`, character(1), "type")
  attributes(covariates) <- list(
    names = cov_names,
    types = cov_types
  )
  covariates
}

#' Extract Correlation Keys
#'
#' Creates unique keys for correlation pairs by sorting variable names and
#' concatenating them with \code{"||"}.
#'
#' @param correlations List. A list of correlation specification objects.
#'
#' @return Character vector of unique correlation keys.
#'
#' @noRd
.extract_corr_keys <- function(correlations) {
  vapply(
    correlations,
    function(x) paste(sort(c(x[["var1"]], x[["var2"]])), collapse = "||"),
    character(1)
  )
}

#' Validate Correlations
#'
#' Validates a list of correlation specification objects, ensuring they are
#' unique and reference valid covariate names.
#'
#' @param correlations List. A list of correlation specification objects to validate.
#' @param covariates List. A list of covariate objects to check against.
#'
#' @return The validated correlations list.
#'
#' @noRd
.assert_correlations <- function(correlations, covariates) {
  .assert_class_list(
    correlations,
    class_name = "correlation",
    min_len = 0L
  )

  if (length(correlations) == 0L) {
    return(invisible(NULL))
  }

  keys <- .extract_corr_keys(correlations)

  .assert_unique_names(
    keys,
    err_msg = paste(
      "Duplicate correlation pair specification(s):",
      "%s.",
      "To define both within and between correlations",
      "for the same variable pair, use a single call:",
      "`define_correlation(var1, var2, corr_within = 0.2, corr_between = 0.4)`."
    )
  )

  var_names <- unique(
    vapply(
      correlations,
      function(x) sort(c(x[['var1']], x[['var2']])),
      character(2)
    )
  )

  bad <- setdiff(var_names, names(covariates))
  if (length(bad)) {
    stop(
      sprintf(
        "Correlations: %s are not defined in covariates",
        paste(bad, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  correlations
}
