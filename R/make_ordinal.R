make_ordinal <- function(
  name,
  probs,
  icc,
  labels = NULL
) {

  context <- "make_ordinal()"

  .assert_covariate_name(name, context)
  .assert_covariate_probs(probs, context)
  .assert_covariate_icc(icc, context)
  .assert_covariate_labels(labels, probs, context)

  if (is.null(labels)) {
    labels <- LETTERS[1:length(probs)]
  } else {
      if (any(grepl(" ", labels))) {
        stop(
          paste0("'labels' in ", context, " must not contain spaces"),
          call. = FALSE
        )
      }
  }

  out <- .make_new_covariate(
    name = name, 
    type = "ordinal",
    mean = 0,
    total_var = 1,
    icc = icc,
    probs = setNames(
      as.list(probs),
      labels
    )
  )

  out

}
