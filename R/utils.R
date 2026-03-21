.unwrap_nested_list <- function(x, obj_class) {
  if (
    length(x) == 1L &&
    is.list(x[[1L]]) &&
    !inherits(x[[1L]], obj_class)
  ) {
    return(x[[1L]])
  }
  x
}

.assert_typed_list <- function(x, class_name, min_len = 1L) {
  var_name <- deparse(substitute(x))

  checkmate::assert_list(
    x,
    min.len = min_len,
    unique = TRUE,
    any.missing = FALSE,
    .var.name = var_name
  )

  ok <- vapply(x, inherits, logical(1), what = class_name)

  if (!all(ok)) {
    bad <- which(!ok)
    stop(
      sprintf(
        "All inputs to `%s` must be of class `%s` (invalid element(s): %s).",
        var_name,
        class_name,
        paste(bad, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


.validate_unique_names <- function(x, err_msg, sep = ", ") {
  if (anyDuplicated(x)) {
    dup <- unique(x[duplicated(x)])
    stop(
      sprintf(err_msg, paste(dup, collapse = sep)),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.validate_covariates <- function(covariates) {
  .assert_typed_list(
    covariates,
    class_name = "ml_covariate",
    min_len = 1L
  )

  covs_names <- vapply(covariates, `[[`, character(1), "name")
  cov_types <- vapply(covariates, `[[`, character(1), "type")

  .validate_unique_names(
    covs_names,
    err_msg = "Duplicate covariate names found: %s"
    )

  names(covariates) <- covs_names
  class(covariates) <- c("ml_covariates_validated", class(covariates))
  covariates
}


.extract_corr_keys <- function(correlations) {
  vapply(
    correlations,
    function(x) paste(sort(c(x[["var1"]], x[["var2"]])), collapse = "||"),
    character(1)
  )
}


.validate_correlations <- function(correlations, covariates) {
  .assert_typed_list(
    correlations,
    class = "ml_corr_pair",
    min_len = 0L
  )

  if (length(correlations) == 0L) {
    return(invisible(NULL))
  }

  keys <- .extract_corr_keys(correlations)

  .validate_unique_names(
    keys,
    err_msg = paste(
      "Duplicate correlation pair specification(s):",
      "%s.",
      "To define both within and between correlations",
      "for the same variable pair, use a single call:",
      "`corr_pair(var1, var2, rho_within = 0.2, rho_between = 0.4)`."
    )
  )

  var_names <- unique(
    vapply(
      correlations,
      function(x) sort(c(x[['var1']], x[['var2']])),
      character(2)
    )
  )

  bad <- setdiff(var_names, names(covariates))
  if (length(bad)) {
    stop(
      sprintf(
        "Invalid covariate(s) in correlations: %s",
        paste(bad, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # names(correlations) <- var_names
  # class(covariates) <- c("ml_correlations_validated", class(correlations))
  correlations
}

.check_and_fix_mat <- function(
  M,
  name = deparse(substitute(M)),
  tol = 1e-8,
  fix_non_psd = TRUE,
  check_singularity = FALSE
) {
  
  asym <- max(abs(M - t(M)))
  if (asym > tol) {
    stop(
      sprintf("%s is not symmetric (max |M - t(M)| = %.3e).", name, asym),
      call. = FALSE
    )
  }

  ev <- eigen(M, symmetric = TRUE, only.values = TRUE)$values
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

    res <- Matrix::nearPD(M, keepDiag = TRUE)

    if (!isTRUE(res$converged)) {
      stop(sprintf(
        "Could not compute a near positive-definite version of %s.", 
        name
      ),
      call. = FALSE)
    }

    M <- as.matrix(res$mat)
  }

  if (check_singularity) {
    ev <- eigen(M, symmetric = TRUE, only.values = TRUE)$values
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

  list(R = M, L = t(chol(M)))
}

.build_R_mat <- function(correlations, covariates) {
  
  cov_names <- names(covariates)
  p <- length(cov_names)
  R_w <- L_w <- diag(p)
  R_b <- L_b <- diag(p)
  dimnames(R_w) <- list(cov_names, cov_names)
  dimnames(R_b) <- list(cov_names, cov_names)

  if (is.null(correlations)) {
    return(list(
      corr_w = list(
        R = R_w,
        L = L_w
      ),
      corr_b = list(
        R = R_b,
        L = L_b
      )
    ))
  }

  var1 <- vapply(correlations, `[[`, character(1), "var1")
  var2 <- vapply(correlations, `[[`, character(1), "var2")
  rho_w <- vapply(correlations, `[[`, numeric(1), "rho_within")
  rho_b <- vapply(correlations, `[[`, numeric(1), "rho_between")

  idx12 <- cbind(var1, var2)
  idx21 <- cbind(var2, var1)

  R_w[idx12] <- rho_w
  R_w[idx21] <- rho_w

  R_b[idx12] <- rho_b
  R_b[idx21] <- rho_b

  types <- vapply(cov_names, function(x) covariates[[x]][["type"]], character(1))
  var_names <- unique(c(var1, var2))

  if (any(types[cov_names %in% var_names] == "binary")) {
    warning("Correlations of binary covariate(s) are in latent space.", call. = FALSE)
  }

  if (any(types[cov_names %in% var_names] == "ordinal")) {
    warning("Correlations of ordinal covariate(s) are in latent space.", call. = FALSE)
  }

  list(
    corr_w = .check_and_fix_mat(R_w, "Within-correlation matrix"),
    corr_b = .check_and_fix_mat(R_b, "Between-correlation matrix")
  )

}
