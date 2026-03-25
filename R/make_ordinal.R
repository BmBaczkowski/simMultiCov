make_ordinal <- function(
  name,
  probs,
  icc,
  labels = NULL
) {

  checkmate::assert_character(
    name,
    len = 1L,
    min.chars = 1L,
    .var.name = "make_ordinal_name"
  )

  checkmate::assert_numeric(
    probs, 
    min.len = 3L,
    any.missing = FALSE,
    finite = TRUE,
    lower = 1e-5,
    upper = 1 - 1e-5,
    .var.name = "make_ordinal_probs"
  )
  if (!isTRUE(all.equal(sum(probs), 1))) {
    stop("'probs' must sum to 1", call. = FALSE)
  }

  checkmate::assert_numeric(
    icc, 
    len = 1L,
    lower = 0,
    upper = 1,
    finite = TRUE,
    any.missing = FALSE,
    .var.name = "make_ordinal_icc"
  )

  checkmate::assert_character(
    labels,
    len = length(probs),
    min.chars = 1L,
    null.ok = TRUE,
    any.missing = FALSE,
    .var.name = "make_ordinal_labels"
  )
  if (is.null(labels)) {
    labels <- LETTERS[1:length(probs)]
  }

  structure(
    list(
      name = name,
      prob = list(probs),
      icc = icc, 
      mean = 0,
      sd = 1,
      labels = list(labels),
      type = "ordinal"
    ),
    class = c("multilevel_covariate", "multilevel_ordinal")
  )

}