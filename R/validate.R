#' @include ess_data.R
NULL

#' Check input data
#'
#' Kept for compatibility: runs the checks of [ess_data()] on a list of
#' data frames in the workbook's sheet order (setup, panelists, ratings, item
#' data, examinee data) and reports the result as text.
#'
#' @param imprt_data A list of data frames, or an object from [ess_data()].
#' @return `"Data input is validated"`, or the first problem found.
#' @examples
#' set.seed(1)
#' sim <- generateMockDataSet(ngca = 2, ntable = 2, npanelist = 3,
#'                            cor_val = 0.7, nlevel = 3, sdinp = 1, ecinp = 0,
#'                            n = 30, 300, 500)
#' validateData(sim)
#'
#' # An alignment label that is not one of the grade's levels
#' sim$rating$ALD[1] <- "Level9"
#' validateData(sim)
#' @export
validateData <- function(imprt_data) {
  tryCatch({
    as_ess_data(imprt_data)
    "Data input is validated"
  }, error = function(e) conditionMessage(e))
}
