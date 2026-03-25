my_simulation <- function(x, ...) {
  UseMethod("my_simulation")
}

#' @export
#' @method my_simulation default
my_simulation.default <- function(x, ...) {
  cat("Default method\n")
}

#' @export
#' @method my_simulation ml_covariates
my_simulation.ml_covariates <- function(x, seed = NULL, ...) {
  cat("ml_covariates method\n")

  df <- .sim_engine(
    x$n_L1,
    x$specs$mean,
    x$D_w,
    x$D_b,
    x$L_w,
    x$L_b,
    seed
  )
  colnames(df)[1] <- x[['cluster_name']]

  is_binary <- which(x[['specs']][['types']] == "binary")
  is_ordinal <- which(x[['specs']][['types']] == "ordinal")
  cov_names <- names(x$specs$types)

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
}



