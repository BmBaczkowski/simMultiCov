.assert_R_mat <- function(
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

.build_R_mat <- function(correlations, covariates) {
  
  cov_names <- names(covariates)
  p <- length(cov_names)
  R_w <- L_w <- diag(p)
  R_b <- L_b <- diag(p)
  dimnames(R_w) <- dimnames(R_b) <- list(cov_names, cov_names)

  if (is.null(correlations)) {
    return(list(
      R_w = R_w,
      R_b = R_b,
      L_w = L_w,
      L_b = L_b
    ))
  }

  var1 <- vapply(correlations, `[[`, character(1), "var1")
  var2 <- vapply(correlations, `[[`, character(1), "var2")
  rho_w <- vapply(correlations, `[[`, numeric(1), "corr_within")
  rho_b <- vapply(correlations, `[[`, numeric(1), "corr_between")

  idx12 <- cbind(var1, var2)
  idx21 <- cbind(var2, var1)

  R_w[idx12] <- rho_w
  R_w[idx21] <- rho_w

  R_b[idx12] <- rho_b
  R_b[idx21] <- rho_b

  R_w <- .assert_R_mat(R_w, "Within-correlation matrix")
  R_b <- .assert_R_mat(R_b, "Between-correlation matrix")

  list(
    R_w = R_w,
    R_b = R_b
  )

}

.build_D_mat <- function(covariates) {
  covs_names <- names(covariates)
  var_total <- unlist(
    .get_covariate_specs(covariates, "total_var")
  )
  icc <- unlist(
    .get_covariate_specs(covariates, "icc")
  )

  var_b <- icc * var_total
  var_w <- (1 - icc) * var_total

  D_b <- D_w <- diag(length(var_b))
  
  diag(D_b) <- sqrt(var_b)
  diag(D_w) <- sqrt(var_w)

  dimnames(D_b) <- dimnames(D_w) <- list(covs_names, covs_names)

  list(
    D_w = D_w,
    D_b = D_b
  )
}
