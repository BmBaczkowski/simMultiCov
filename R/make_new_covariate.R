.make_new_covariate <- function(name, type, ...) {

  specs <- list(...)

  structure(
    list(
      name = name,
      type = type,
      specs = specs
    ),
    class = c("covariate", paste0("covariate_", type))
  )
}