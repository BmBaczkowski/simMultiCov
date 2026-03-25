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

