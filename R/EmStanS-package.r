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
#' @name EmStanS-package
#' @docType package
#' @title Embedded Standard Setting
#' @author Sooyong Lee \email{sooyongl09@utexas.edu}
#' @import stringr
#' @import mgcv
#' @import tidymv
#' @import shiny
#' @import shinythemes
#' @import shinyWidgets
#' @import shinydashboard
#' @import bslib
#' @import plotly
#' @importFrom foreach foreach `%do%`
#' @importFrom ggrepel geom_label_repel
#' @importFrom grid arrow unit
#' @importFrom glue glue
#' @importFrom dplyr mutate bind_cols select mutate_all mutate_at rename bind_rows arrange slice left_join case_when vars group_split pull group_by filter summarise
#' @importFrom tibble tibble
#' @importFrom tidyr spread gather
#' @importFrom tidyselect everything matches starts_with ends_with all_of
#' @importFrom data.table data.table
#' @importFrom DT renderDT
#' @importFrom purrr map map2 set_names cross
#' @importFrom DT DTOutput
#' @importFrom magrittr `%>%`
#' @importFrom utils browseURL write.csv read.csv
#' @keywords package
NULL

