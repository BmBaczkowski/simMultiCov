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

#' Internal validation function that ensures a list of covariates and
#' correlations meet specific requirements. This function performs two
#' key validations: (1) each element must inherit from a specified class, and
#' (2) all objects must have unique names.
#'
#' @param x A list of objects to validate. Each element should be an instance
#'   of the specified class.
#'
#' @param obj_class A character string specifying the expected class name.
#'   All elements in x must inherit from this class.
#'
#'
#' @return Invisibly returns TRUE if all validations pass.
#'
#' @examples
#' # Example 1: Valid input
#' objects <- list(
#'   obj1 = structure(list(name = "x1"), class = "covariate"),
#'   obj2 = structure(list(name = "x2"), class = "covariate")
#' )
#' .validate_ml_objects(objects, "covariate")
#' # Returns TRUE invisibly
#'
#' # Example 2: Invalid class
#' objects <- list(
#'   obj1 = structure(list(name = "x1"), class = "covariate"),
#'   obj2 = structure(list(name = "x2"), class = "value")
#' )
#' .validate_ml_objects(objects, "covariate")
#' # Error: All inputs must be of single class.
#'
#' # Example 3: Duplicate names
#' objects <- list(
#'   obj1 = structure(list(name = "x1"), class = "covariate"),
#'   obj2 = structure(list(name = "x1"), class = "covariate")
#' )
#' .validate_ml_objects(objects, "covariate")
#' # Error: Duplicate covariate names found: x1
#'
#' @keywords internal
.validate_ml_objects <- function(x, obj_class) {
  x_name <- deparse(substitute(x))
  min.len <- 1L
  if (identical(x_name, "correlations")) {
    min.len <- 0L
  }
  # Validate it is a list
  checkmate::assert_list(
    x,
    min.len = min.len, 
    unique = TRUE,
    any.missing = FALSE,
    .var.name = x_name
  )

  # Validate each element is an accurate class
  ok <- vapply(
    x, 
    inherits, 
    logical(1), 
    what = obj_class
  )

  if (!all(ok)) {
    stop(
      sprintf(
        "All inputs to `%s` must be of class `%s`.", 
        x_name, obj_class
      ),
      call. = FALSE
    )
  }

  # Validate inputs are unique
  if (identical(x_name, "covariates")) {
    keys <- vapply(x, function(xx) xx[['name']], character(1))
    str_info <- "Duplicate covariate names found: %s"
  } else if (identical(x_name, "correlations")) {
    keys <- vapply(
      x,
      function(xx) paste(sort(c(xx[['var1']], xx[['var2']])), collapse = "||"),
      character(1)
    )
    str_info <- "Duplicate correlation pair specification(s): %s"
  }

  if (anyDuplicated(keys)) {
    dup <- unique(keys[duplicated(keys)])
    stop(
      sprintf(str_info, paste(dup, collapse = ", ")),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}