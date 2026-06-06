#' S3 method
#'
#' @export
print.ESS <- function(x, ...) {
  cat("ESS output\n")
  cat("----------\n")
  cat("Individual Cut:\n")

  print(x$individual_cut, n = 10, width = Inf)

  cat("\nMeidan Cut:\n")

  print(x$median_cut, n = 10, width = Inf)

  cat("\nModal Cut:\n")

  print(x$modal_cut, n = 10, width = Inf)

  cat("\nAverage Cut:\n")

  print(x$average_cut, n = 10, width = Inf)

  cat("\nMore details can be extacted using the `extract()` function")

  invisible(x)
}




print.bootESS <- function(x) {
    x$bootResults$table2
}
