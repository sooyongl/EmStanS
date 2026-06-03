#' #' @include 2_update_tab.r
#' NULL
#'
#' #------------------------------------------------------------------
#' # Excel Report
#' #------------------------------------------------------------------
#' # Generic for Word Tables
#' exceltable <- function(table_inp, ...){
#'   UseMethod("exceltable", table_inp)
#' }
#'
#' exceltable.inpdata <- function() {}
#'
#' exceltable.ESSresult1 <- function() {}
#'
#' exceltable.ESSresult2 <- function() {}
#'
#' exceltable.detailESS <- function() {}
#'
#' #'
#' exceltable.crosst <- function(table_inp) {
#'   # table_inp <- tab2$for_tab2_out[[1]][[1]][["crosst"]]
#'   table_inp_lower <- table_inp[[2]]
#'   table_inp_upper <-
#'     as.data.frame.matrix(table_inp[[1]]) %>%
#'     mutate(".." := rownames(.),
#'            .before = 1) %>%
#'     mutate("." := "Operational Level",
#'            .before = 1) %>%
#'     mutate_all( ~ str_replace(.x, "<br>", " (")) %>%
#'     mutate_all( ~ str_replace(.x, "%", "%)"))
#'
#'   col_names <- names(table_inp_upper)
#'
#'   table_inp_lower <- rbind(c(" ", "Freq", "Perc"), table_inp_lower)
#'   dummy_data <- matrix(rep(" "), ncol = length(col_names[4:length(col_names)]), nrow = nrow(table_inp_lower))
#'   table_inp_lower <- cbind(table_inp_lower, dummy_data)
#'
#'   names(table_inp_lower) <- col_names
#'
#'   table_combined <- rbind(table_inp_upper, table_inp_lower)
#'
#'
#'   # cross index ------------------------------
#'   nlevel <- ncol(table_inp[[1]])
#'   cross_index <- data.frame(rowi = 1:nlevel, coli = (1:nlevel)+2)
#'
#'
#'   table_combined
#' }
#'
#' exceltable.effpage <- function() {}
#'
#' exceltable.review <- function() {}
#'
#'
#' #'
#' excel_out3 <-
#'   function(filename, reportTables,
#'            titleInp = "Embedded Standard Setting Technical Report",
#'            stateNM  = "State Name",
#'            testNM   = "Testing Program Name",
#'            admNM    = "Creative Measurement Solutions LLC") {
#'
#'     todaydate<- as.character(Sys.Date())
#'     fakewords = "Input Text"
#'
#'     filename = "temp.xlsx"
#'
#'     temp_wb <- createWorkbook("temp_wb")
#'
#'     gca_selected <- reportTables$setup[["GCA"]]
#'
#'     # Title page
#'     addWorksheet(temp_wb, "Title")
#'
#'     writeData(
#'       temp_wb,
#'       "Title",
#'       x = titleInp,
#'       startCol = 3,
#'       startRow = 3
#'     )
#'
#'     writeData(
#'       temp_wb,
#'       "Title",
#'       x = todaydate,
#'       startCol = 3,
#'       startRow = 5
#'     )
#'
#'     writeData(
#'       temp_wb,
#'       "Title",
#'       x = stateNM,
#'       startCol = 3,
#'       startRow = 7
#'     )
#'
#'     writeData(
#'       temp_wb,
#'       "Title",
#'       x = testNM,
#'       startCol = 3,
#'       startRow = 9
#'     )
#'
#'     writeData(
#'       temp_wb,
#'       "Title",
#'       x = admNM,
#'       startCol = 3,
#'       startRow = 11
#'     )
#'     # input data ---------------------------------------------
#'     addWorksheet(temp_wb, "Input_data")
#'
#'     row_p <- 1
#'     writeData(
#'       temp_wb,
#'       "Input_data",
#'       x = reportTables$caption$setup,
#'       startCol = 1,
#'       startRow = row_p
#'     )
#'
#'     row_p <- row_p + 1
#'     writeDataTable(
#'       temp_wb,
#'       "Input_data",
#'       x = reportTables$setup,
#'       startCol = 1,
#'       startRow = row_p
#'     )
#'
#'
#'     row_p <- nrow(reportTables$setup) + row_p + 2
#'     writeData(
#'       temp_wb,
#'       "Input_data",
#'       x = reportTables$caption$panel,
#'       startCol = 1,
#'       startRow = row_p
#'     )
#'
#'     row_p <- row_p + 1
#'     writeDataTable(
#'       temp_wb,
#'       "Input_data",
#'       x = reportTables$panel,
#'       startCol = 1,
#'       startRow = row_p
#'     )
#'
#'     # Tab1 ---------------------------------------------
#'     addWorksheet(temp_wb, "Tab1")
#'
#'     row_p <- 1
#'     writeData(
#'       temp_wb,
#'       "Tab1",
#'       x = reportTables$caption$indi,
#'       startCol = 1,
#'       startRow = row_p,
#'       borders = c("all"), #"surrounding"
#'       headerStyle =
#'         createStyle(
#'           border = c("top","bottom","left","right")
#'         )
#'     )
#'     row_p <- row_p + 1
#'     writeDataTable(
#'       temp_wb,
#'       "Tab1",
#'       x = reportTables$indi,
#'       startCol = 1,
#'       startRow = row_p
#'     )
#'
#'     row_p <- nrow(reportTables$indi) + row_p + 2
#'     writeData(
#'       temp_wb,
#'       "Tab1",
#'       x = reportTables$caption$modal,
#'       startCol = 1,
#'       startRow = row_p
#'     )
#'
#'     row_p <- row_p + 1
#'     writeDataTable(
#'       temp_wb,
#'       "Tab1",
#'       x = reportTables$modal,
#'       startCol = 1,
#'       startRow = row_p
#'     )
#'
#'     row_p <- nrow(reportTables$modal) + row_p + 2
#'     writeData(
#'       temp_wb,
#'       "Tab1",
#'       x = reportTables$caption$med,
#'       startCol = 1,
#'       startRow = row_p
#'     )
#'
#'     row_p <- row_p + 1
#'     writeDataTable(
#'       temp_wb,
#'       "Tab1",
#'       x = reportTables$med,
#'       startCol = 1,
#'       startRow = row_p
#'     )
#'
#'
#'     # Tab2 ---------------------------------------------
#'     for(gca in gca_selected) {
#'       # gca = "M1"
#'       sheet_name <- paste0("Tab2_", gca)
#'       addWorksheet(temp_wb, sheet_name)
#'
#'       out_name <- paste0("detailESS_", gca)
#'       if(!is_empty(reportTables$caption[[out_name]])) {
#'         row_p <- 1
#'         writeData(
#'           temp_wb,
#'           sheet_name,
#'           x = reportTables$caption[[out_name]],
#'           startCol = 1,
#'           startRow = row_p,
#'           borders = c("all"), #"surrounding"
#'           headerStyle =
#'             createStyle(
#'               border = c("top","bottom","left","right")
#'             )
#'         )
#'         row_p <- row_p + 1
#'         writeDataTable(
#'           temp_wb,
#'           sheet_name,
#'           x = reportTables[[out_name]],
#'           startCol = 1,
#'           startRow = row_p
#'         )
#'       }
#'
#'       col_p <- ifelse(is_empty(reportTables$caption[[out_name]]), 1,
#'                 ncol(reportTables[[out_name]]) + 2)
#'
#'       out_name <- paste0("crosst_", gca)
#'       if(!is_empty(reportTables$caption[[out_name]])) {
#'         row_p <- 1
#'         writeData(
#'           temp_wb,
#'           sheet_name,
#'           x = reportTables$caption[[out_name]],
#'           startCol = col_p,
#'           startRow = row_p,
#'           borders = c("all"), #"surrounding"
#'           headerStyle =
#'             createStyle(
#'               border = c("top","bottom","left","right")
#'             )
#'         )
#'         row_p <- row_p + 1
#'         writeDataTable(
#'           temp_wb,
#'           sheet_name,
#'           x = reportTables[[out_name]],
#'           startCol = col_p,
#'           startRow = row_p
#'         )
#'       }
#'
#'       col_p_ec <- ifelse(is_empty(reportTables$caption[[out_name]]), 1,
#'                       ncol(reportTables[[out_name]]) + col_p + 1)
#'
#'       out_name <- paste0("crosst_ec_", gca)
#'       if(!is_empty(reportTables$caption[[out_name]])) {
#'         row_p <- 1
#'         writeData(
#'           temp_wb,
#'           sheet_name,
#'           x = reportTables$caption[[out_name]],
#'           startCol = col_p_ec,
#'           startRow = row_p,
#'           borders = c("all"), #"surrounding"
#'           headerStyle =
#'             createStyle(
#'               border = c("top","bottom","left","right")
#'             )
#'         )
#'         row_p <- row_p + 1
#'         writeDataTable(
#'           temp_wb,
#'           sheet_name,
#'           x = reportTables[[out_name]],
#'           startCol = col_p_ec,
#'           startRow = row_p
#'         )
#'       }
#'
#'       row_p <- nrow(reportTables[[out_name]]) + 4
#'
#'       # out_name <- paste0("detailp_", gca)
#'       # if(!is_empty(reportTables$caption[[out_name]])) {
#'       #   writeData(
#'       #     temp_wb,
#'       #     sheet_name,
#'       #     x = reportTables$caption[[out_name]],
#'       #     startCol = col_p,
#'       #     startRow = row_p
#'       #   )
#'       #
#'       #   row_p <- row_p + 1
#'       #   print(reportTables[[out_name]])
#'       #   insertPlot(
#'       #     temp_wb,
#'       #     sheet = sheet_name,
#'       #     width = 6,
#'       #     height = 4,
#'       #     startRow = row_p,
#'       #     startCol = col_p,
#'       #     fileType = "png",
#'       #     units = "in",
#'       #     dpi = 300
#'       #   )
#'       # }
#'     }
#'
#'
#'     # Tab3 ---------------------------------------------
#'     addWorksheet(temp_wb, "Tab3")
#'
#'     out_name <- paste0("effpage")
#'     if(!is_empty(reportTables$caption[[out_name]])) {
#'
#'       row_p <- 1
#'       writeData(
#'         temp_wb,
#'         "Tab3",
#'         x = reportTables$caption[[out_name]],
#'         startCol = 1,
#'         startRow = row_p
#'       )
#'
#'       row_p <- row_p + 1
#'       writeDataTable(
#'         temp_wb,
#'         "Tab3",
#'         x = reportTables[[out_name]],
#'         startCol = 1,
#'         startRow = row_p
#'       )
#'     }
#'
#'     col_p <- ifelse(is_empty(reportTables$caption[[out_name]]), 1,
#'                              ncol(reportTables[[out_name]]) + 2)
#'
#'     # Tab4 ---------------------------------------------
#'     addWorksheet(temp_wb, "Tab4")
#'
#'     out_name <- paste0("ireview")
#'     if(!is_empty(reportTables$caption[[out_name]])) {
#'
#'       row_p <- 1
#'       writeData(
#'         temp_wb,
#'         "Tab4",
#'         x = reportTables$caption[[out_name]],
#'         startCol = 1,
#'         startRow = row_p
#'       )
#'
#'       row_p <- row_p + 1
#'       writeDataTable(
#'         temp_wb,
#'         "Tab4",
#'         x = reportTables[[out_name]],
#'         startCol = 1,
#'         startRow = row_p
#'       )
#'     }
#'
#'     return(temp_wb)
#'   }
