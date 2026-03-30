#' Create a continuous covariate specification
#'
#' Defines a continuous covariate for use in multilevel simulations. Continuous
#' covariates are generated from a normal distribution with specified mean and
#' total variance, with optional clustering structure.
#'
#' @param name A character string specifying the name of the covariate.
#'   Must be a non-empty string.
#' @param mean A numeric value specifying the mean of the covariate.
#'   Defaults to 0. Must be a single finite value with no missing values.
#' @param total_var A numeric value specifying the total variance of the
#'   covariate. Defaults to 1. Must be a single positive finite value with
#'   no missing values.
#' @param icc A numeric value between 0 and 1 specifying the intraclass
#'   correlation coefficient. This determines the proportion of total variance
#'   that is between clusters. Defaults to 0 (no clustering). Must be a single
#'   finite value with no missing values.
#'
#' @return An object of class \code{c("covariate", "covariate_continuous")}
#'   containing the covariate specification.
#'
#' @details
#' Continuous covariates are generated from a normal distribution with the
#' specified mean and variance. The \code{icc} parameter controls the
#' clustering structure of the covariate.
#'
#' This function is typically used in conjunction with [make_covariates()]
#' to build a complete multilevel data generating process.
#'
#' @export
#'
#' @examples
#' # Create a standard normal covariate
#' make_continuous("x1")
#'
#' # Create a covariate with mean 100 and variance 25
#' make_continuous("income", mean = 100, total_var = 25)
#'
#' # Create a clustered covariate with ICC of 0.2
#' make_continuous("ability", mean = 0, total_var = 1, icc = 0.2)
#'
#' @seealso [make_covariates()], [make_binary()], [make_ordinal()]
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
