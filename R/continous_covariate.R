continuous_covariate <- function(
  name, 
  mean = 0,
  sd = 1,
  icc = 0
) {

  checkmate::assert_character(
    name,
    len = 1L,
    min.chars = 1L,
    .var.name = "continuous_covariate_name"
  )

  checkmate::assert_numeric(
    mean, 
    len = 1L,
    any.missing = FALSE,
    .var.name = "continuous_covariate_mean"
  )

  checkmate::assert_numeric(
    sd, 
    len = 1L,
    lower = 1e-10,
    finite = TRUE,
    any.missing = FALSE,
    .var.name = "continuous_covariate_sd"
  )

  checkmate::assert_numeric(
    icc, 
    len = 1L,
    lower = 0,
    upper = 1,
    finite = TRUE,
    any.missing = FALSE,
    .var.name = "continuous_covariate_icc"
  )

  structure(
    list(
      name = name,
      mean = mean,
      sd = sd,
      icc = icc,
      type = "continuous"
    ),
    class = c("ml_covariate", "ml_covariate_continous")
  )

}


