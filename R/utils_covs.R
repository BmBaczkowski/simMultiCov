.get_covariate_specs <- function(covariates, name) {
 
  out <- sapply(
    covariates, 
    function(x) x[['specs']][[name]]
  )
  names(out) <- names(covariates)

  out
}

.find_thresholds <- function(specs, context = "binary") {
  indx <- which(specs$types == context)

  thresholds <- NULL

  if (length(indx) > 0) {
    thr_names <- specs$names[indx]
    thresholds <- lapply(
      thr_names, 
      function(x) {
        p <- unlist(specs$probs[[x]])
        thr <- qnorm(cumsum(p)[-length(p)])
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