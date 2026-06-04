#' #' S3 method
#' #'
#' #' @export print.ess
#' #' @export
#' print.ess <- function(x) {
#'   print(list(
#'     ess_table = head(x$ess_table),
#'     review    = head(x$review)
#'   ))
#' }
#'
#'
print.ESS <- function(x, ...) {
  cat("ESS output\n")
  cat("----------\n")
  cat("Individual Cut:\n")

  print(x$individual_cut, n = 10, width = Inf)

  invisible(x)
}


