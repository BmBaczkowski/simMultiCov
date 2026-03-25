simulate <- function(x, ...) {
  UseMethod("simulate")
}

#' @export
#' @method simulate default
simulate.default <- function(x, ...) {
  cat("Default method\n")
}

#' @export
#' @method simulate covariates
simulate.covariates <- function(x, seed = NULL, ...) {

  df <- .simulation_engine(
    cluster_sizes = x$cluster_size,
    mean_vec = unlist(x$specs$mean),
    D_w = x$specs$D_w,
    D_b = x$specs$D_b,
    R_w = x$specs$R_w,
    R_b = x$specs$R_b,
    seed = seed
  )
  colnames(df)[1] <- x[['cluster_name']]

  is_binary <- which(x[['specs']][['types']] == "binary")
  is_ordinal <- which(x[['specs']][['types']] == "ordinal")
  cov_names <- x$specs$names

  if (is_binary) {
    for (indx in is_binary) {
      var_name <- cov_names[indx]
      p <- unlist(x[['specs']][['probs']][[var_name]])
      labels <- unlist(x[['specs']][['labels']][[var_name]])
      df[, var_name] <- .to_binary(df[, var_name], p[1], labels)
    }
  }

  if (is_ordinal) {
    for (indx in is_ordinal) {
      var_name <- cov_names[indx]
      p <- unlist(x[['specs']][['probs']][[var_name]])
      labels <- unlist(x[['specs']][['labels']][[var_name]])
      df[, var_name] <- .to_ordinal(df[, var_name], p, labels)
    }
  }
  df

  # add attributes for data generating process
  attributes(df) <- list(
    mean = unlist(x$specs$mean),
    Sigma_w = x$specs$Sigma_w, 
    Sigma_b = x$specs$Sigma_b, 
  )
}

