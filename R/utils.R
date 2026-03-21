#' Unwrap single nested list
#'
#' If a list contains exactly one element that is itself a list (but not an
#' object of a specified class), unwrap it by one level. Otherwise, return the
#' list unchanged.
#'
#' @param x A list to potentially unwrap.
#' @param obj_class Character vector of classes to exclude from unwrapping.
#'   Objects inheriting from these classes won't be unwrapped even if they're
#'   the only element in the list.
#'
#' @return The unwrapped list (contents of `x[[1]]`) if `x` contains exactly one
#'   element that is a list and does not inherit from any class in `obj_class`.
#'   Otherwise, returns `x` unchanged.
#'
#' @examples
#' # Unwraps single nested list
#' .unwrap_nested_list(list(list(a = 1, b = 2)), obj_class = "custom_class")
#' # Returns: list(a = 1, b = 2)
#'
#' # Doesn't unwrap if there are multiple elements
#' .unwrap_nested_list(list(list(a = 1), list(b = 2)), obj_class = "custom_class")
#' # Returns: list(list(a = 1), list(b = 2))
#'
#' # Doesn't unwrap objects of the specified class
#' .unwrap_nested_list(
#'   list(structure(list(a = 1), class = "ml_covariate")),
#'   obj_class = "ml_covariate"
#' )
#' # Returns: list(structure(list(a = 1), class = "ml_covariate"))
#'
#' @keywords internal
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

#' Internal validation function that ensures a list of covariates meets
#' specific requirements. This function performs two key validations:
#' (1) each element must inherit from "ml_covariate" class, and (2) all
#' covariates must have unique names.
#'
#' @param covariates A list of covariate objects to validate. Each element
#'   should be an instance of "ml_covariate".
#'
#' @return A character vector of unique covariate names.
#'
#' @examples
#' # Example 1: Valid input
#' covariates <- list(
#'   obj1 = structure(list(name = "x1"), class = "ml_covariate"),
#'   obj2 = structure(list(name = "x2"), class = "ml_covariate")
#' )
#' .validate_covariates(covariates)
#' # Returns: c("x1", "x2")
#'
#' # Example 2: Invalid class
#' covariates <- list(
#'   obj1 = structure(list(name = "x1"), class = "ml_covariate"),
#'   obj2 = structure(list(name = "x2"), class = "value")
#' )
#' .validate_covariates(covariates)
#' # Error: All inputs must be of class "ml_covariate".
#'
#' # Example 3: Duplicate names
#' covariates <- list(
#'   obj1 = structure(list(name = "x1"), class = "ml_covariate"),
#'   obj2 = structure(list(name = "x1"), class = "ml_covariate")
#' )
#' .validate_covariates(covariates)
#' # Error: Duplicate covariate names found: x1
#'
#' @keywords internal
.validate_covariates <- function(covariates) {
  # Validate it is a list
  checkmate::assert_list(
    covariates,
    min.len = 1L,
    unique = TRUE,
    any.missing = FALSE,
    .var.name = "covariates"
  )

  # Validate each element is an accurate class
  ok <- vapply(
    covariates,
    inherits,
    logical(1),
    what = "ml_covariate"
  )

  if (!all(ok)) {
    stop(
      sprintf(
        "All inputs to `covariates` must be of class `%s`.",
        "ml_covariate"
      ),
      call. = FALSE
    )
  }

  # Validate inputs are unique
  keys <- vapply(covariates, function(x) x[['name']], character(1))

  if (anyDuplicated(keys)) {
    dup <- unique(keys[duplicated(keys)])
    stop(
      sprintf("Duplicate covariate names found: %s", paste(dup, collapse = ", ")),
      call. = FALSE
    )
  }

  keys
}

#' Internal validation function that ensures a list of correlations meets
#' specific requirements. This function performs two key validations:
#' (1) each element must inherit from "ml_corr_pair" class, and (2) all
#' correlation pairs must be unique (order-independent).
#'
#' @param correlations A list of correlation pair objects to validate. Each
#'   element should be an instance of "ml_corr_pair".
#' @param covariate_names Character vector of valid covariate names to validate
#'   that all correlation variables exist.
#'
#' @return A character vector of unique covariate names for which 
#'   covariate names are specified. Defaults to NULL when the correlation
#'   list is empty.
#'
#' @examples
#' # Example 1: Valid input (empty list)
#' correlations <- list()
#' .validate_correlations(correlations)
#' # Returns: NULL
#'
#' # Example 2: Valid input
#' correlations <- list(
#'   obj1 = structure(list(var1 = "x1", var2 = "x2"), class = "ml_corr_pair"),
#'   obj2 = structure(list(var1 = "x1", var2 = "x3"), class = "ml_corr_pair")
#' )
#' .validate_correlations(correlations)
#' # Returns: c("x1", "x2", "x3")
#'
#' # Example 3: Invalid class
#' correlations <- list(
#'   obj1 = structure(list(var1 = "x1", var2 = "x2"), class = "ml_corr_pair"),
#'   obj2 = structure(list(var1 = "x1", var2 = "x2"), class = "value")
#' )
#' .validate_correlations(correlations)
#' # Error: All inputs must be of class "ml_corr_pair".
#'
#' # Example 4: Duplicate pairs
#' correlations <- list(
#'   obj1 = structure(list(var1 = "x1", var2 = "x2"), class = "ml_corr_pair"),
#'   obj2 = structure(list(var1 = "x2", var2 = "x1"), class = "ml_corr_pair")
#' )
#' .validate_correlations(correlations)
#' # Error: Duplicate correlation pair specification(s): x1||x2
#'
#' @keywords internal
.validate_correlations <- function(correlations, covariate_names) {
  # Validate it is a list (correlations can be empty)
  checkmate::assert_list(
    correlations,
    min.len = 0L,
    unique = TRUE,
    any.missing = FALSE,
    .var.name = "correlations"
  )

  if (length(correlations) == 0L) {
    return(NULL)
  }

  # Validate each element is an accurate class
  ok <- vapply(
    correlations,
    inherits,
    logical(1),
    what = "ml_corr_pair"
  )

  if (!all(ok)) {
    stop(
      sprintf(
        "All inputs to `correlations` must be of class `%s`.",
        "ml_corr_pair"
      ),
      call. = FALSE
    )
  }

  # Validate inputs are unique (order-independent)
  keys <- vapply(
    correlations,
    function(x) paste(sort(c(x[['var1']], x[['var2']])), collapse = "||"),
    character(1)
  )

  if (anyDuplicated(keys)) {
    dup <- unique(keys[duplicated(keys)])
    stop(
      sprintf(
        "Duplicate correlation pair specification(s): %s.
        To define both within and between correlations
        for the same variable pair, use single call:
        'corr_pair(var1, var2, rho_within = 0.2, rho_between = 0.4)'",
        paste(gsub("\\|\\|", " - ", dup), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Validate variables
  var_names <- unique(
    vapply(
      correlations,
      function(x) sort(c(x[['var1']], x[['var2']])),
      character(2)
    )
  )

  if (!all(var_names %in% covariate_names)) {
    stop(
      "Invalid covariate(s) in correlations: ",
      paste(setdiff(var_names, covariate_names), collapse = ", "),
      call. = FALSE
    )
  }

  var_names
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

    ev <- eigen(M, symmetric = TRUE, only.values = TRUE)$values
    min_ev <- min(ev)
  }

  if (check_singularity && min_ev <= tol) {
    warning(
      sprintf(
        "%s is singular or nearly singular (min eigenvalue = %.3e).", 
        name, min_ev
      ),
      call. = FALSE
    )
  }

  M
}

.build_R_mat <- function(correlations, covariates) {
  
  cov_names <- vapply(covariates, `[[`, character(1), "name")
  cov_types <- vapply(covariates, `[[`, character(1), "type")
  p <- length(cov_names)
  R_w <- diag(p)
  R_b <- diag(p)
  dimnames(R_w) <- list(cov_names, cov_names)
  dimnames(R_b) <- list(cov_names, cov_names)
  
  if (length(correlations) == 0) {
    return(list(R_b = R_b, R_w = R_w))
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

  R_w <- .check_and_fix_mat(R_w, "Within-correlation matrix")
  R_b <- .check_and_fix_mat(R_b, "Between-correlation matrix")
  

  list(
    R_b = R_b,
    R_w = R_w
  )
}
