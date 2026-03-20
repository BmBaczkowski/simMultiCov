ordinal_covariate <- function(
  name,
  probs,
  labels = NULL
) {

  structure(
    list(
      name = name,
      probs = probs,
      labels = labels
    ),
    class = c("ml_ordinal_covariate", "ml_covariate")
  )

}