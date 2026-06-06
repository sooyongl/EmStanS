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
#' @import stringr
#' @import bslib
#' @import ggplot2
#' @import dplyr
#'@importFrom plotly layout ggplotly
#' @importFrom foreach foreach `%do%`
#' @importFrom ggrepel geom_label_repel
#' @importFrom grid arrow unit
#' @importFrom glue glue
#' @importFrom tibble tibble as_tibble
#' @importFrom tidyr spread gather drop_na crossing separate
#' @importFrom tidyselect everything matches starts_with ends_with all_of
#' @importFrom data.table data.table
#' @importFrom DT datatable formatStyle styleEqual JS
#' @importFrom purrr map map2 set_names cross pmap map2_dbl
#' @importFrom magrittr `%>%`
#' @importFrom utils browseURL write.csv read.csv
#' @importFrom readxl read_excel excel_sheets
#' @importFrom mgcv gam predict.gam
#' @importFrom htmltools HTML tags
#' @importFrom kableExtra cell_spec add_header_above row_spec
#' @importFrom kableExtra column_spec collapse_rows kable_styling
#' @importFrom knitr kable
#' @importFrom stringi stri_replace_all_charclass
#' @importFrom psych describe
#' @keywords internal
"_PACKAGE"
