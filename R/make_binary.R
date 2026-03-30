#' Create a binary covariate specification
#'
#' Defines a binary covariate for use in multilevel simulations. Binary
#' covariates are generated using a latent variable approach and then
#' dichotomized based on the specified probability.
#'
#' @param name A character string specifying the name of the covariate.
#'   Must be a non-empty string.
#' @param prob A numeric value between 0 and 1 specifying the probability
#'   of the first category (e.g., the probability of "A" or the reference
#'   category). Must be a single finite value with no missing values.
#' @param icc A numeric value between 0 and 1 specifying the intraclass
#'   correlation coefficient. This determines the proportion of total variance
#'   that is between clusters. Must be a single finite value with no missing
#'   values.
#' @param labels An optional character vector of length 2 specifying the
#'   labels for the two categories. If not provided, defaults to
#'   \code{c("A", "B")}. Labels must not contain spaces.
#'
#' @return An object of class \code{c("covariate", "covariate_binary")}
#'   containing the covariate specification.
#'
#' @details
#' Binary covariates are generated using a latent normal distribution that
#' is dichotomized based on the specified probability. The \code{icc}
#' parameter controls the clustering structure of the covariate.
#'
#' This function is typically used in conjunction with [make_covariates()]
#' to build a complete multilevel data generating process.
#'
#' @export
#'
#' @examples
#' # Create a binary covariate with 30% probability of category A
#' make_binary("treatment", prob = 0.3, icc = 0.1)
#'
#' # Create a binary covariate with custom labels
#' make_binary("gender", prob = 0.5, icc = 0.05, labels = c("Male", "Female"))
#'
#' @seealso [make_covariates()], [make_continuous()], [make_ordinal()]
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
