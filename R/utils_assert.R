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

.assert_covariate_mean <- function(mean, context) {

  checkmate::assert_numeric(
    mean, 
    len = 1L,
    any.missing = FALSE,
    .var.name = paste0("'mean' in ", context)
  )

}

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
  }

  if (!isTRUE(all.equal(sum(probs), 1))) {
    stop("'probs' must sum to 1", call. = FALSE)
  }

}

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
