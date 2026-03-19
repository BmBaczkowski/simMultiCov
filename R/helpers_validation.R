.assert_n_L1 <- function(n_L1, n_L2) {
  if (!length(n_L1) %in% c(1L, n_L2)) {
    stop("'n_L1' must be a scalar or a vector of length 'n_L2'.", call. = FALSE)
  }

  checkmate::assert_integerish(
    n_L1,
    lower = 1L,
    any.missing = FALSE,
    .var.name = "n_L1"
  )

  if (length(n_L1) == 1L) {
    rep(as.integer(n_L1), n_L2)
  } else {
    as.integer(n_L1)
  }
}

.assert_rho <- function(rho, p, name) {
  checkmate::assert_number(
    rho,
    lower = -1,
    upper = 1,
    finite = TRUE,
    .var.name = name
  )

  if (!is.null(rho) && p > 1L && rho <= -1 / (p - 1)) {
    stop(
      sprintf(
        "'%s' = %.6f is too small for p = %d. Compound-symmetry requires %s > %.6f.",
        name, rho, p, name, -1 / (p - 1)
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.assert_correlation_matrix <- function(x, p, name, tol = 1e-8) {
  checkmate::assert_matrix(
    x,
    nrows = p,
    ncols = p,
    mode = "numeric",
    any.missing = FALSE,
    .var.name = name
  )

  if (!isTRUE(all.equal(x, t(x), tolerance = tol))) {
    stop(sprintf("'%s' must be symmetric.", name), call. = FALSE)
  }

  if (!all(is.finite(x))) {
    stop(sprintf("'%s' must contain only finite values.", name), call. = FALSE)
  }

  if (!all(abs(diag(x) - 1) <= tol)) {
    stop(sprintf("'%s' must have ones on the diagonal.", name), call. = FALSE)
  }

  if (!all(x >= -1 - tol & x <= 1 + tol)) {
    stop(sprintf("'%s' entries must lie in [-1, 1].", name), call. = FALSE)
  }

  eig_min <- min(eigen((x + t(x)) * 0.5, symmetric = TRUE, only.values = TRUE)$values)
  if (eig_min < -tol) {
    stop(sprintf("'%s' must be positive semidefinite.", name), call. = FALSE)
  }

  invisible(TRUE)
}