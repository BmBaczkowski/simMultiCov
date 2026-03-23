binary_covariate <- function(
  name,
  prob,
  icc,
  labels = NULL
) {

  checkmate::assert_character(
    name,
    len = 1L,
    min.chars = 1L,
    .var.name = "binary_covariate_name"
  )

  checkmate::assert_numeric(
    prob, 
    len = 1L,
    lower = 1e-5,
    upper = 1 - 1e-5,
    finite = TRUE,
    any.missing = FALSE,
    .var.name = "binary_covariate_prob"
  )

  checkmate::assert_numeric(
    icc, 
    len = 1L,
    lower = 0,
    upper = 1,
    finite = TRUE,
    any.missing = FALSE,
    .var.name = "binary_covariate_icc"
  )

  checkmate::assert_character(
    labels,
    len = 2L,
    min.chars = 1L,
    null.ok = TRUE,
    any.missing = FALSE,
    .var.name = "binary_covariate_labels"
  )
  if (is.null(labels)) {
    labels <- c("0", "1")
  }

  structure(
    list(
      name = name,
      prob = prob,
      mean = 0,
      sd = 1,
      icc = icc, 
      labels = labels,
      type = "binary"
    ),
    class = c("ml_covariate", "ml_covariate_binary")
  )

}