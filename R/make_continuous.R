make_continuous <- function(
  name, 
  mean = 0,
  total_var = 1,
  icc = 0
) {

  context <- "make_continuous()"

  .assert_covariate_name(name, context)
  .assert_covariate_mean(mean, context)
  .assert_covariate_total_var(total_var, context)
  .assert_covariate_icc(icc, context)

  out <- .make_new_covariate(
    name = name, 
    type = "continuous",
    mean = mean, 
    total_var = total_var,
    icc = icc
  )

  out
}
