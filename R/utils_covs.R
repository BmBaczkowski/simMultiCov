#' Get Covariate Specifications
#'
#' Extracts a specific specification field from all covariates in a list.
#'
#' @param covariates List. A list of covariate objects.
#' @param name Character string. The name of the specification field to extract.
#'
#' @return A named vector or list of specification values for each covariate.
#'
#' @keywords internal
.get_covariate_specs <- function(covariates, name) {
 
  out <- sapply(
    covariates, 
    function(x) x[['specs']][[name]]
  )
  names(out) <- names(covariates)

  out
}

#' Find Thresholds for Conversion
#'
#' Identifies covariates of a specified type and computes thresholds for
#' converting continuous values to categorical factors.
#'
#' @param specs List. A list containing covariate specifications including
#'   \code{types}, \code{names}, and \code{probs}.
#' @param context Character string. The covariate type to filter for
#'   (default: "binary").
#'
#' @return A named list of threshold vectors, or \code{NULL} if no matching
#'   covariates are found. Each threshold vector has a \code{labels} attribute.
#'
#' @keywords internal
.find_thresholds <- function(specs, context = "binary") {
  indx <- which(specs$types == context)

  thresholds <- NULL

  if (length(indx) > 0) {
    thr_names <- specs$names[indx]
    thresholds <- lapply(
      thr_names, 
      function(x) {
        p <- unlist(specs$probs[[x]])
        thr <- stats::qnorm(cumsum(p)[-length(p)])
        attributes(thr) <- list(
          labels = names(specs$probs[[x]])
        )
        thr
      }
    )
    names(thresholds) <- thr_names
  }

  thresholds
}

#' Apply Conversions to Dataframe
#'
#' Converts continuous columns in a dataframe to categorical factors using
#' the provided thresholds.
#'
#' @param df Dataframe. The dataframe containing columns to convert.
#' @param thresholds List. A named list of threshold vectors as returned by
#'   \code{\link{.find_thresholds}}.
#'
#' @return The dataframe with specified columns converted to factors.
#'
#' @keywords internal
.apply_conversions <- function(df, thresholds) {
  if (is.null(thresholds)) {
    return(df)
  }

  thr_names <- names(thresholds)

  df[thr_names] <- Map(
    function(col, thr) {
      labs <- attr(thr, "labels")
      if (length(thr) > 1) {
        .convert_to_factor(df[[col]], cuts = thr, labels = labs)
      } else {
        .convert_to_factor(df[[col]], threshold = thr, labels = labs)
      }
    }, 
    thr_names, 
    thresholds
  )

  df
}

#' Convert to Factor
#'
#' Converts a numeric vector to a factor using either a single threshold
#' (binary case) or multiple cut points (ordinal case).
#'
#' @param x Numeric vector. The values to convert.
#' @param threshold Numeric. A single threshold value for binary conversion
#'   (default: \code{NULL}).
#' @param cuts Numeric vector. Cut points for ordinal conversion
#'   (default: \code{NULL}).
#' @param labels Character vector. Labels for the factor levels.
#'
#' @return A factor vector with the specified labels.
#'
#' @keywords internal
.convert_to_factor <- function(x, threshold = NULL, cuts = NULL, labels) {
  if (!is.null(threshold)) {
    # Binary case
    x <- as.integer(x <= threshold[1])
    return(factor(x, levels = c(0, 1), labels = labels))
  }
  
  if (!is.null(cuts)) {
    # Ordinal case
    x <- findInterval(x, vec = cuts) + 1
    return(factor(
      x,
      levels = seq_along(labels),
      labels = labels,
      ordered = TRUE
    ))
  }
  
  stop("Provide either 'threshold' (binary) or 'cuts' (ordinal)")
}
