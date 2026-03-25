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
    probs <- c(probs, 1-probs)
  }

  if (!isTRUE(all.equal(sum(probs), 1))) {
    stop(
      paste0("'probs' in ", context, " must sum to 1"),
      call. = FALSE
    )
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

.assert_covariates <- function(covariates) {
  .assert_class_list(
    covariates,
    class_name = "covariate",
    min_len = 1L
  )

  covs_names <- vapply(covariates, `[[`, character(1), "name")

  .assert_unique_names(
    covs_names,
    err_msg = "Duplicate covariate names found: %s"
    )

  names(covariates) <- covs_names
  covariates
}

.extract_corr_keys <- function(correlations) {
  vapply(
    correlations,
    function(x) paste(sort(c(x[["var1"]], x[["var2"]])), collapse = "||"),
    character(1)
  )
}

.assert_correlations <- function(correlations, covariates) {
  .assert_class_list(
    correlations,
    class = "correlation",
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