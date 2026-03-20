binary_covariate <- function(
  name,
  probs,
  labels = c("0", "1")
) {

  structure(
    list(
      name = name,
      probs = probs,
      labels = labels
    ),
    class = c("ml_binary_covariate", "ml_covariate")
  )

}