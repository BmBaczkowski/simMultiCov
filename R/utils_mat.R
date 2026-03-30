#' Validate Correlation Matrix
#'
#' Checks that a matrix is symmetric and positive semi-definite. Optionally
#' fixes non-positive semi-definite matrices by computing the nearest
#' positive-definite matrix.
#'
#' @param R Numeric matrix. The correlation matrix to validate.
#' @param name Character string. Name of the matrix for error messages
#'   (default: deparsed expression of \code{R}).
#' @param tol Numeric. Tolerance for symmetry and eigenvalue checks
#'   (default: 1e-8).
#' @param fix_non_psd Logical. If \code{TRUE}, replaces non-positive
#'   semi-definite matrices with the nearest positive-definite matrix
#'   (default: \code{TRUE}).
#' @param check_singularity Logical. If \code{TRUE}, warns if the matrix
#'   is singular or nearly singular (default: \code{FALSE}).
#'
#' @return The validated correlation matrix.
#'
#' @keywords internal
.assert_correlation_matrix <- function(
  R,
  name = deparse(substitute(R)),
  tol = 1e-8,
  fix_non_psd = TRUE,
  check_singularity = FALSE
) {
  
  asym <- max(abs(R - t(R)))
  if (asym > tol) {
    stop(
      sprintf("%s is not symmetric (max |%s - t(%s)| = %.3e).", name, asym),
      call. = FALSE
    )
  }

  ev <- eigen(R, symmetric = TRUE, only.values = TRUE)$values
  min_ev <- min(ev)

  if (min_ev < -tol) {
    if (!fix_non_psd) {
      stop(
        sprintf(
          "%s is not positive semi-definite (min eigenvalue = %.3e).", 
          name, min_ev
        ),
        call. = FALSE
      )
    }

    warning(
      sprintf(
        "%s is not positive semi-definite (min eigenvalue = %.3e). 
        Replacing with nearest positive-definite matrix.",
        name, min_ev
      ),
      call. = FALSE
    )

    res <- Matrix::nearPD(R, keepDiag = TRUE)

    if (!isTRUE(res$converged)) {
      stop(sprintf(
        "Could not compute a near positive-definite version of %s.", 
        name
      ),
      call. = FALSE)
    }

    R <- as.matrix(res$mat)
  }

  if (check_singularity) {
    ev <- eigen(R, symmetric = TRUE, only.values = TRUE)$values
    min_ev <- min(ev)

    if (min_ev <= tol) {
      warning(
        sprintf(
          "%s is singular or nearly singular (min eigenvalue = %.3e).", 
          name, min_ev
        ),
        call. = FALSE
      )
    }
  }

  R
}

#' Build Correlation Matrix
#'
#' Constructs a correlation matrix from a list of correlation specifications
#' and covariate objects.
#'
#' @param correlations List. A list of correlation specification objects, or
#'   \code{NULL}.
#' @param covariates List. A list of covariate objects.
#' @param context Character string. Either "within" or "between", specifying
#'   which correlation type to use.
#'
#' @return A symmetric correlation matrix with covariate names as dimension
#'   names.
#'
#' @keywords internal
.build_correlation_matrix <- function(correlations, covariates, context) {
  if (context == "within") {
    rho_type <- "corr_within"
    msg <- "Within-correlation matrix"
  } else if (context == "between") {
    rho_type <- "corr_between"
    msg <- "Between-correlation matrix"
  }
  cov_names <- names(covariates)
  R <- diag(length(cov_names))
  dimnames(R) <- list(cov_names, cov_names)

  if (is.null(correlations)) {
    return(R)
  }

  var1 <- vapply(correlations, `[[`, character(1), "var1")
  var2 <- vapply(correlations, `[[`, character(1), "var2")
  rho <- vapply(correlations, `[[`, numeric(1), rho_type)

  idx12 <- cbind(var1, var2)
  idx21 <- cbind(var2, var1)

  R[idx12] <- rho
  R[idx21] <- rho

  R <- .assert_correlation_matrix(R, msg)

  R
}

#' Build Standard Deviation Matrix
#'
#' Constructs a diagonal matrix of standard deviations from covariate
#' specifications, using either within-cluster or between-cluster variance.
#'
#' @param covariates List. A list of covariate objects.
#' @param type Character string. Either "within" or "between", specifying
#'   which variance component to use.
#'
#' @return A diagonal matrix of standard deviations with covariate names
#'   as dimension names.
#'
#' @keywords internal
.build_sd_matrix <- function(covariates, type) {
  cov_names <- names(covariates)
  var_total <- .get_covariate_specs(covariates, "total_var", simplify = TRUE)
  icc <- .get_covariate_specs(covariates, "icc", simplify = TRUE)

  if (type == "within") {
    var <- (1 - icc) * var_total
  } else if (type == "between") {
    var <- icc * var_total

  }
  
  D <- diag(length(var))
  diag(D) <- sqrt(var)
  dimnames(D) <- list(cov_names, cov_names)

  D
}


# Environment for persistent cache
.cholesky_cache <- new.env(parent = emptyenv())

#' Get Cholesky Factor
#'
#' Computes and caches the lower triangular Cholesky factor of a correlation
#' matrix. Uses a digest-based cache to avoid redundant computations.
#'
#' @param R Numeric matrix. A symmetric positive-definite correlation matrix.
#'
#' @return The lower triangular Cholesky factor \code{L} such that
#'   \code{L \%*\% t(L) = R}.
#'
#' @keywords internal
.get_cholesky_factor <- function(R) {
  cache_key <- digest::digest(R)
  
  if (exists(cache_key, envir = .cholesky_cache)) {
    return(get(cache_key, envir = .cholesky_cache))
  }
  
  # Compute and cache
  L <- t(chol(R))
  assign(cache_key, L, envir = .cholesky_cache)
  L
}
