.build_cs_cor <- function(rho, p) {
  R <- matrix(rho, nrow = p, ncol = p)
  diag(R) <- 1
  R
}

.resolve_correlation <- function(rho, rho_mat, p, rho_name, mat_name) {
  if (!is.null(rho) && !is.null(rho_mat)) {
    stop(
      sprintf("Only one of '%s' and '%s' may be supplied.", rho_name, mat_name),
      call. = FALSE
    )
  }

  if (!is.null(rho)) {
    .assert_rho(rho, p, rho_name)
    return(.build_cs_cor(rho, p))
  }

  if (!is.null(rho_mat)) {
    .assert_correlation_matrix(rho_mat, p, mat_name)
    return((rho_mat + t(rho_mat)) * 0.5)
  }

  diag(p)
}

.cov_from_sd_and_cor <- function(sd_vec, R) {
  R * tcrossprod(sd_vec)
}

.cov_factor <- function(Sigma, name, tol = 1e-10) {
  if (!all(is.finite(Sigma))) {
    stop(sprintf("'%s' must contain only finite values.", name), call. = FALSE)
  }

  if (!isTRUE(all.equal(Sigma, t(Sigma), tolerance = tol))) {
    stop(sprintf("'%s' must be symmetric.", name), call. = FALSE)
  }

  Sigma <- (Sigma + t(Sigma)) * 0.5

  R <- tryCatch(chol(Sigma), error = function(e) NULL)
  if (!is.null(R)) {
    return(R)
  }

  es <- eigen(Sigma, symmetric = TRUE)
  vals <- es$values
  rel_tol <- tol * max(1, max(abs(vals)))

  if (any(vals < -rel_tol)) {
    stop(sprintf("'%s' must be positive semidefinite.", name), call. = FALSE)
  }

  vals[vals < 0] <- 0
  diag(sqrt(vals), nrow = length(vals)) %*% t(es$vectors)
}