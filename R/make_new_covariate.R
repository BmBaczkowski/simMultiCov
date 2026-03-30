#' Create a new covariate object (internal)
#'
#' Internal function that creates a covariate object with the specified
#' name, type, and additional specifications. This function is called by
#' [make_binary()], [make_continuous()], and [make_ordinal()] to create
#' the final covariate object.
#'
#' @param name A character string specifying the name of the covariate.
#' @param type A character string specifying the type of covariate
#'   (e.g., "continuous", "binary", "ordinal").
#' @param ... Additional specifications to include in the covariate object.
#'   These vary by covariate type and may include mean, total_var, icc,
#'   and probs.
#'
#' @return An object of class \code{c("covariate", "covariate_<type>")}
#'   containing the covariate specification.
#'
#' @noRd
.make_new_covariate <- function(name, type, ...) {

  specs <- list(...)

  structure(
    list(
      name = name,
      type = type,
      specs = specs
    ),
    class = c("covariate", paste0("covariate_", type))
  )
}
