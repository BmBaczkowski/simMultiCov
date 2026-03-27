make_binary <- function(
  name,
  prob,
  icc,
  labels = NULL
) {

  context <- "make_binary()"

  .assert_covariate_name(name, context)
  .assert_covariate_probs(prob, context)
  .assert_covariate_icc(icc, context)

  probs <- c(prob, 1 - prob)
  .assert_covariate_labels(labels, probs, context)

  if (is.null(labels)) {
    labels <- c("A", "B")
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
    type = "binary",
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
