source_files <- fs::dir_ls("R")
for(i in 1:length(source_files)) { source(source_files[i])}

library(stringr)
library(bslib)
# library(plotly)
library(ggplot2)

foreach <- foreach::foreach
`%do%` <- foreach::`%do%`

geom_label_repel <- ggrepel::geom_label_repel

arrow <- grid::arrow
unit <- grid::unit

glue <- glue::glue

# mutate <- dplyr::mutate
# bind_cols <- dplyr::bind_cols
# select <- dplyr::select
# mutate_all <- dplyr::mutate_all
# mutate_at <- dplyr::mutate_at
# rename <- dplyr::rename
# bind_rows <- dplyr::bind_rows
# arrange <- dplyr::arrange
# slice <- dplyr::slice
# left_join <- dplyr::left_join
# case_when <- dplyr::case_when
# vars <- dplyr::vars
# group_split <- dplyr::group_split
# pull <- dplyr::pull
# group_by <- dplyr::group_by
# filter <- dplyr::filter
# summarise <- dplyr::summarise
# n <- dplyr::n
# count <- dplyr::count
# relocate <- dplyr::relocate
# mutate_if <- dplyr::mutate_if
# if_else <- dplyr::if_else
# summarise_all <- dplyr::summarise_all
# group_by_at <- dplyr::group_by_at
# distinct_at <- dplyr::distinct_at

library(dplyr)

tibble <- tibble::tibble
as_tibble <- tibble::as_tibble

spread <- tidyr::spread
gather <- tidyr::gather
drop_na <- tidyr::drop_na
crossing <- tidyr::crossing
separate <- tidyr::separate

everything <- tidyselect::everything
matches <- tidyselect::matches
starts_with <- tidyselect::starts_with
ends_with <- tidyselect::ends_with
all_of <- tidyselect::all_of

data.table <- data.table::data.table

datatable <- DT::datatable
formatStyle <- DT::formatStyle
styleEqual <- DT::styleEqual
JS <- DT::JS

pmap <- purrr::pmap
map <- purrr::map
map2 <- purrr::map2
map2_dbl <- purrr::map2_dbl
set_names <- purrr::set_names
cross <- purrr::cross

`%>%` <- magrittr::`%>%`

browseURL <- utils::browseURL
write.csv <- utils::write.csv
read.csv <- utils::read.csv

read_excel <- readxl::read_excel
excel_sheets <- readxl::excel_sheets

gam <- mgcv::gam
predict.gam <- mgcv::predict.gam

HTML <- htmltools::HTML
tags <- htmltools::tags


stri_replace_all_charclass <- stringi::stri_replace_all_charclass

cell_spec <- kableExtra::cell_spec
add_header_above <- kableExtra::add_header_above
row_spec <- kableExtra::row_spec
column_spec <- kableExtra::column_spec
collapse_rows <- kableExtra::collapse_rows
kable_styling <- kableExtra::kable_styling
kable <- knitr::kable

layout <- plotly::layout
ggplotly <- plotly::ggplotly

describe <- psych::describe
# -------------------------------------------------------------------------

file.show(fs::dir_ls("prerun/data/example_file")[1])



res <- emstans(filePath = fs::dir_ls("prerun/data/example_file")[1],
               grade = c(1,2),
               targets = "ALD",
               WESS = T,
               gamest = F,
               median = "modal",
               loc = "RP67",
               domain = "GCA",
               select_domain = NULL,
               font_size = 14,
               digits = 3)

res

report(res, what = "summary")


extract(res)

class(res)
summary(res)

report(res, what = "all")
report(res, what = "individual")
report(res, what = "detailed")
report(res, what = "summary")

report(res, what = "review")

report(res, what = "setup")


extract(res, "cutscore")

res0 <- update(res, "R0-All" = c(-1, 0.5))

boot_res <- boostrapping(res, "R0-All")

summary(boot_res)

extract(boot_res)


library(EmStanS)

fake_data <- genFakeDataSet(ngca = 3,
                            cor_val = 0.2,
                            n = 30,
                            nlevel = 3,
                            ntable = 5,
                            npanelist = 5,
                            sdinp = 1,
                            ecinp = 0,
                            # ndomain = NULL,
                            100,300)

options(warn = 2)
res <- emstans(data = fake_data,
               tests = c(1,2),
               targets = "ALD",
               WESS = T,
               gamest = T,
               median = "modal",
               loc = "Loc_RP50",
               domain = "GCA")

lifecycle::last_lifecycle_warnings()

report(res, what = "detailed")
report(res, what = "summary")
