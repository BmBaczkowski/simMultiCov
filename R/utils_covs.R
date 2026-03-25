.get_covariate_specs <- function(covariates, name) {

  # Extract raw values
  vals <- lapply(covariates, function(x) x[['specs']][[name]])
  vals
}

.to_binary <- function(x, p, labels = c(0, 1)) {
  t <- qnorm(p)
  x <- as.integer(x <= t)
  x <- factor(x,
    levels = c(0, 1),
    labels = labels
  )
  x
}

.to_ordinal <- function(x, p, labels) {
  cuts <- qnorm(cumsum(p)[-length(p)])

  x <- findInterval(x, vec = cuts) + 1
  x <- factor(x,
    levels = 1:length(p),
    labels = labels,
    ordered = TRUE
  )
  x
}

