#' Unwrap single nested list
#'
#' If a list contains exactly one element that is itself a list (but not an
#' object of a specified class), unwrap it by one level. Otherwise, return the
#' list unchanged.
#'
#' @param x A list to potentially unwrap.
#' @param obj_class Character vector of classes to exclude from unwrapping.
#'   Objects inheriting from these classes won't be unwrapped even if they're
#'   the only element in the list.
#'
#' @return The unwrapped list (contents of `x[[1]]`) if `x` contains exactly one
#'   element that is a list and does not inherit from any class in `obj_class`.
#'   Otherwise, returns `x` unchanged.
#'
#' @examples
#' # Unwraps single nested list
#' .unwrap_nested_list(list(list(a = 1, b = 2)), obj_class = "custom_class")
#' # Returns: list(a = 1, b = 2)
#'
#' # Doesn't unwrap if there are multiple elements
#' .unwrap_nested_list(list(list(a = 1), list(b = 2)), obj_class = "custom_class")
#' # Returns: list(list(a = 1), list(b = 2))
#'
#' # Doesn't unwrap objects of the specified class
#' .unwrap_nested_list(
#'   list(structure(list(a = 1), class = "ml_covariate")),
#'   obj_class = "ml_covariate"
#' )
#' # Returns: list(structure(list(a = 1), class = "ml_covariate"))
#'
#' @keywords internal
.unwrap_nested_list <- function(x, obj_class) {
  if (
    length(x) == 1L &&
    is.list(x[[1L]]) &&
    !inherits(x[[1L]], obj_class)
  ) {
    return(x[[1L]])
  }
  x
}