make_binary <- function(
  name,
  prob,
  icc,
  labels = NULL
) {

  checkmate::assert_character(
    name,
    len = 1L,
    min.chars = 1L,
    .var.name = "make_binary_name"
  )

  checkmate::assert_numeric(
    prob, 
    len = 1L,
    lower = 1e-5,
    upper = 1 - 1e-5,
    finite = TRUE,
    any.missing = FALSE,
    .var.name = "make_binary_prob"
  )

  checkmate::assert_numeric(
    icc, 
    len = 1L,
    lower = 0,
    upper = 1,
    finite = TRUE,
    any.missing = FALSE,
    .var.name = "make_binary_icc"
  )

  checkmate::assert_character(
    labels,
    len = 2L,
    min.chars = 1L,
    null.ok = TRUE,
    any.missing = FALSE,
    .var.name = "make_binary_labels"
  )
  if (is.null(labels)) {
    labels <- c("0", "1")
  }

  structure(
    list(
      name = name,
      prob = list(c(prob, 1 - prob)),
      mean = 0,
      sd = 1,
      icc = icc, 
      labels = list(labels),
      type = "binary"
    ),
    class = c("multilevel_covariate", "multilevel_binary")
  )

}