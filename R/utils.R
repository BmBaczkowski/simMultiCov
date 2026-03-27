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

.get_seed <- function(seed = NULL) {
  old_seed <- if (exists(".Random.seed")) .Random.seed else NULL
  
  invisible(old_seed)
}

.restore_seed <- function(old_seed) {
  if (!is.null(old_seed)) {
    .Random.seed <<- old_seed
  }
}
