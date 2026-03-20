continuous_covariate <- function(
  name, 
  mean = 0,
  sd = 1,
  icc = 0
) {

  structure(
    list(
        name = name,
        mean = mean,
        sd = sd,
        icc = icc
    ),
    class = c("ml_continuous_covariate", "ml_covariate")
  )

}


