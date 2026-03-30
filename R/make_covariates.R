#' Build a complete covariate specification for multilevel simulations
#'
#' Combines multiple covariate specifications and correlation structures
#' into a complete covariance structure for multilevel data generation.
#' This function validates all inputs and computes the within-cluster and
#' between-cluster variance-covariance matrices.
#'
#' @param covariates A list of covariate specifications created using
#'   [make_binary()], [make_continuous()], or [make_ordinal()]. Can also
#'   be a nested list that will be automatically unwrapped.
#' @param correlations A list of correlation specifications created using
#'   [define_correlation()]. Can also be a nested list that will be
#'   automatically unwrapped.
#'
#' @return An object of class \code{"covariates"} containing the following
#'   elements:
#'   \describe{
#'     \item{n_covariates}{Number of covariates.}
#'     \item{names}{Character vector of covariate names.}
#'     \item{types}{Character vector of covariate types (continuous, binary, ordinal).}
#'     \item{icc}{List of intraclass correlation coefficients.}
#'     \item{total_var}{List of total variances.}
#'     \item{mean}{List of means.}
#'     \item{probs}{List of probability specifications for binary/ordinal covariates.}
#'     \item{R_b}{Between-cluster correlation matrix.}
#'     \item{R_w}{Within-cluster correlation matrix.}
#'     \item{D_b}{Between-cluster standard deviation matrix.}
#'     \item{D_w}{Within-cluster standard deviation matrix.}
#'     \item{Sigma_b}{Between-cluster variance-covariance matrix.}
#'     \item{Sigma_w}{Within-cluster variance-covariance matrix.}
#'     \item{.thresholds}{Pre-computed thresholds for binary/ordinal covariates.}
#'   }
#'
#' @details
#' This function is the main entry point for defining the covariance structure
#' of a multilevel simulation. It takes individual covariate and correlation
#' specifications and combines them into a complete covariance structure.
#'
#' The function performs the following operations:
#' \itemize{
#'   \item Validates all covariate and correlation specifications
#'   \item Builds within-cluster and between-cluster correlation matrices
#'   \item Computes variance-covariance matrices from correlations and standard deviations
#'   \item Pre-computes thresholds for binary/ordinal covariates
#' }
#'
#' For binary and ordinal covariates, correlations are specified in the latent
#' space. A warning is issued if any binary or ordinal covariates have
#' non-zero correlations with other covariates.
#'
#' @export
#'
#' @examples
#' # Create a simple covariate structure with two continuous covariates
#' covs <- make_covariates(
#'   covariates = list(
#'     make_continuous("x1", mean = 0, total_var = 1, icc = 0.1),
#'     make_continuous("x2", mean = 0, total_var = 1, icc = 0.1)
#'   ),
#'   correlations = list(
#'     define_correlation("x1", "x2", corr_within = 0.5, corr_between = 0.3)
#'   )
#' )
#'
#' # Create a covariate structure with mixed types
#' covs <- make_covariates(
#'   covariates = list(
#'     make_continuous("age", mean = 50, total_var = 100, icc = 0.2),
#'     make_binary("treatment", prob = 0.5, icc = 0.1),
#'     make_ordinal("satisfaction", probs = c(0.2, 0.3, 0.5), icc = 0.15)
#'   ),
#'   correlations = list(
#'     define_correlation("age", "treatment", corr_within = 0.1, corr_between = 0.05)
#'   )
#' )
#'
#' @seealso [make_binary()], [make_continuous()], [make_ordinal()], [define_correlation()]
make_covariates <- function(
  covariates = list(),
  correlations = list()
) {

  # Handle nested list case
  covariates <- .unwrap_nested_list(covariates, "covariate")
  correlations <- .unwrap_nested_list(correlations, "correlation")

  # Validate covariate list
  covariates <- .assert_covariates(covariates)

  # Validate correlation list
  correlations <- .assert_correlations(correlations, covariates)

  # Build output components
  R_w <- .build_correlation_matrix(correlations, covariates, "within")
  R_b <- .build_correlation_matrix(correlations, covariates, "between")
  D_w <- .build_sd_matrix(covariates, "within")
  D_b <- .build_sd_matrix(covariates, "between")

  # Compute variance-covariance matrix
  Sigma_w <- D_w %*% R_w %*% D_w
  Sigma_b <- D_b %*% R_b %*% D_b

  # 
  specs <- list(
    n_covariates = length(names(covariates)),
    names = names(covariates),
    types = attr(covariates, "types"),
    icc = .get_covariate_specs(covariates, "icc", simplify = TRUE),
    total_var = .get_covariate_specs(covariates, "total_var", simplify = TRUE),
    mean = .get_covariate_specs(covariates, "mean", simplify = TRUE),
    probs = .get_covariate_specs(covariates, "probs"),
    R_b = R_b,
    R_w = R_w,
    D_b = D_b,
    D_w = D_w,
    Sigma_b = Sigma_b,
    Sigma_w = Sigma_w
  )


  # Pre-compute conversion thresholds for binary/ordinal
  thresholds <- .find_thresholds(specs, "binary")
  thresholds <- c(
    thresholds,
    .find_thresholds(specs, "ordinal")
  )

  if (!is.null(thresholds)) {
    indx <- which(specs$names %in% names(thresholds))

    sub_w <- R_w[indx, -indx, drop = FALSE]
    sub_b <- R_b[indx, -indx, drop = FALSE]

    problematic <- any(sub_w != 0) || any(sub_b != 0)

    if (problematic) {
      warning("Correlations of binary / ordinal covariate(s) are in latent space.", call. = FALSE)
    }
  }

  structure(
    c(specs,
      list(
        .thresholds = thresholds
      )
    ),
    class = "covariates"
  )

}


#' @export
print.covariates <- function(x, ...) {
  cli::cli_h1(paste0("<", class(x), ">"))

  # Covariate summary
  cli::cli_h2("Covariates ({x$n_covariates})")
  cov_names <- x$names
  cov_types <- x$types

  cli::cli_ul()
  for (i in seq_along(cov_names)) {
    cli::cli_li("\u2022 {.strong {cov_names[[i]]}}: {cov_types[[i]]}")
  }

  # Correlations
  if (!is.null(x$R_w) && any(x$R_w[lower.tri(x$R_w)] != 0)) {
    cli::cli_h2("Correlations")
    cli::cli_alert_info("Within/between correlation matrices available. Use summary() for details.")
  }
  else {
    cli::cli_alert_info("Use summary() for details.")
  }

  invisible(x)
}

#' @export
summary.covariates <- function(object, ...) {
  x <- object
  
  cli::cli_h1(paste0("<", class(x), ">"))
  cli::cli_text("{.emph Full specification}")

  # Covariate details table
  cli::cli_h2("Structure")

  cov_names <- x$names
  cov_types <- x$types

  is_non_continuous <- which(cov_types %in% c("binary", "ordinal"))
  cov_types[is_non_continuous] <- paste(cov_types[is_non_continuous], "(latent)")

  # Build data frame for display
  df <- data.frame(
    Name = cov_names,
    Type = cov_types,
    ICC = unlist(x$icc),
    Mean = unlist(x$mean),
    Var = unlist(x$total_var),
    stringsAsFactors = FALSE
  )

  print(df, row.names = FALSE)
  cli::cli_text("")
  
  # Correlation matrices
  cli::cli_h2("Correlations")
  cli::cli_h3("Within-Cluster")
  print(round(x$R_w, 3))

  cli::cli_h3("Between-Cluster")
  print(round(x$R_b, 3))

  # Variance-covariance matrices
  cli::cli_h2("Variance-Covariance")
  cli::cli_h3("Within-Cluster ")
  print(round(x$Sigma_w, 3))

  cli::cli_text("")
  cli::cli_h3("Between-Cluster")
  print(round(x$Sigma_b, 3))

  invisible(x)
}
