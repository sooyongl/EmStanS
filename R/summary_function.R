#' Summary for an ESS Object
#'
#' @exportS3Method
summary.ESS <- function(x) {


  print.ESS(x)

  # res : individual result
  # median: median_res_com
  # mode: modal_res_com
  # average: average_res_com
}

#' Summary for an bootESS Object
#'
#' @exportS3Method
summary.bootESS <- function(x) {

  print.bootESS(x)
  x$bootResults$table2
}
