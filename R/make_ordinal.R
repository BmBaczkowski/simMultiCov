#' Create an ordinal covariate specification
#'
#' Defines an ordinal covariate for use in multilevel simulations. Ordinal
#' covariates are generated using a latent variable approach and then
#' categorized based on the specified probability thresholds.
#'
#' @param name A character string specifying the name of the covariate.
#'   Must be a non-empty string.
#' @param probs A numeric vector specifying the probability of each category.
#'   Values must be between 0 and 1 and sum to 1. The length of this vector
#'   determines the number of categories.
#' @param icc A numeric value between 0 and 1 specifying the intraclass
#'   correlation coefficient. This determines the proportion of total variance
#'   that is between clusters. Must be a single finite value with no missing
#'   values.
#' @param labels An optional character vector specifying the labels for each
#'   category. If not provided, defaults to uppercase letters (A, B, C, ...)
#'   based on the number of categories.
#'
#' @return An object of class \code{c("covariate", "covariate_ordinal")}
#'   containing the covariate specification.
#'
#' @details
#' Ordinal covariates are generated using a latent normal distribution that
#' is categorized based on the specified probability thresholds. The
#' \code{icc} parameter controls the clustering structure of the covariate.
#'
#' This function is typically used in conjunction with [make_covariates()]
#' to build a complete multilevel data generating process.
#'
#' @export
#'
#' @examples
#' # Create a 3-level ordinal covariate
#' make_ordinal("satisfaction", probs = c(0.2, 0.3, 0.5), icc = 0.1)
#'
#' # Create an ordinal covariate with custom labels
#' make_ordinal("education", 
#'              probs = c(0.3, 0.4, 0.3), 
#'              icc = 0.15,
#'              labels = c("Low", "Medium", "High"))
#'
#' @seealso [make_covariates()], [make_binary()], [make_continuous()]
make_ordinal <- function(
  name,
  probs,
  icc,
  labels = NULL
) {

  context <- "make_ordinal()"

  .assert_covariate_name(name, context)
  .assert_covariate_probs(probs, context)
  .assert_covariate_icc(icc, context)
  .assert_covariate_labels(labels, probs, context)

  if (is.null(labels)) {
    labels <- LETTERS[1:length(probs)]
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
    type = "ordinal",
    mean = 0,
    total_var = 1,
    icc = icc,
    probs = stats::setNames(
      as.list(probs),
      labels
    )
  )

  out

}
