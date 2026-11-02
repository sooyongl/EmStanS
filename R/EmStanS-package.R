#' Embedded Standard Setting
#'
#' Embedded Standard Setting (ESS) is an approach to establish
#' performance standards that augments and is embedded within existing
#' principled assessment design (PAD) practices. Under ESS, achievement level
#' descriptor (ALD) writing and the alignment of test items to specified
#' achievement levels are viewed as the fundamental standard-setting activities
#' and cut scores are estimated by optimizing the relationship between the
#' item-ALD alignments and the empirical item locations. Under ESS, this package provides
#' cut score estimation algorithm (referred to as ESS-Count and ESS-Weight).
#'
#' @import ggplot2
#' @importFrom dplyr %>% mutate bind_rows bind_cols row_number
#' @importFrom stringr str_remove
#' @importFrom tibble tibble as_tibble
#' @importFrom data.table data.table
#' @importFrom stats runif rnorm rpois sd var formula
#' @importFrom shiny bootstrapPage
#' @keywords internal
"_PACKAGE"

# shiny is imported because bslib's page functions, used by report(), need it;
# bslib itself lists shiny only under Suggests.

# Column names used in dplyr/data-frame expressions.
utils::globalVariables(c(".", ":=", "Item_ID", "cut_score", "domain", "location"))
