#' Embedded Standard Setting
#'
#' Embedded Standard Setting (ESS) is an approach to establish
#' performance standards that augments and is embedded within existing
#' principled assessment design (PAD) practices. Under ESS, achievement level
#' descriptor (ALD) writing and the alignment of test items to specified
#' achievement levels are viewed as the fundamental standard-setting activities
#' and cut scores are estimated by optimizing the relationship between the
#' item-ALD alignments and the empirically. Under ESS, this package provides
#' cut score estimation algorithm (referred to as ESS-Count and ESS-Weight).
#'
#' @import ggplot2
#' @importFrom dplyr %>% select mutate mutate_if bind_rows bind_cols arrange
#' @importFrom dplyr sample_frac matches all_of everything row_number
#' @importFrom purrr map map2 set_names
#' @importFrom glue glue
#' @importFrom stringr str_extract str_remove
#' @importFrom tibble tibble as_tibble
#' @importFrom data.table data.table
#' @keywords internal
"_PACKAGE"
