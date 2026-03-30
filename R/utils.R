#' Unwrap Nested List
#'
#' Unwraps a single-element list if its contents are not of the specified
#' class. This is useful for normalizing list inputs that may be either
#' directly a list of objects or wrapped in an extra layer.
#'
#' @param x List. The list to potentially unwrap.
#' @param class_name Character string. The class name to check against.
#'
#' @return The unwrapped list if conditions are met; otherwise the original
#'   list unchanged.
#'
#' @noRd
.unwrap_nested_list <- function(x, class_name) {
  if (
    length(x) == 1L &&
    is.list(x[[1L]]) &&
    !inherits(x[[1L]], class_name)
  ) {
    return(x[[1L]])
  }
  x
}

#' Get Current Random Seed
#'
#' Retrieves the current value of \code{.Random.seed} from the global
#' environment, if it exists.
#'
#' @param seed Unused parameter (kept for API consistency).
#'
#' @return The current \code{.Random.seed} vector, or \code{NULL} if it
#'   does not exist.
#'
#' @noRd
.get_seed <- function(seed = NULL) {
  old_seed <- if (exists(".Random.seed")) .Random.seed else NULL
  
  invisible(old_seed)
}

#' Restore Random Seed
#'
#' Restores a previously saved random seed to the global environment.
#'
#' @param old_seed Integer vector. The seed value to restore, typically
#'   obtained from \code{\link{.get_seed}}.
#'
#' @return Invisible \code{NULL}. Called for its side effect of setting
#'   \code{.Random.seed}.
#'
#' @noRd
.restore_seed <- function(old_seed) {
  if (!is.null(old_seed)) {
    .Random.seed <<- old_seed
  }
}
