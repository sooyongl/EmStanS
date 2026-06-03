#' @include 2_update_tab.r
NULL

#' Get an excel file for bootstrapping results ready for downloading
#'
boot_excel <- function(bootreport) {
  # bootreport <- bootResults
  id_ <- str_remove(names(bootreport$table1), "_boot_table")

  reportTables <- list()
  i=1
  # for(i in 1:length(id_)){
    f_tab_gen <- bootreport$table1[[i]]

    reportTables[[paste0("boot_t1_",i)]] <- f_tab_gen
    reportTables$caption[[paste0("boot_t1_",i)]] <- paste0("Table-",id_[i],". Bootstrapping")

    f_tab_gen <- bootreport$table2[[i]]

    reportTables[[paste0("boot_t2_",i)]] <- f_tab_gen
    reportTables$caption[[paste0("boot_t2_",i)]] <- paste0("Table-",id_[i],". Bootstrapping")

  # }

  reportTables
}

#' Download an excel file for bootstrapping results
#'
boot_exceldown <- function(reportTables) {

  filename = "temp.xlsx"
  temp_wb <- createWorkbook("temp_wb")

  sheet_name <- paste0("Bootstrapping")
  addWorksheet(temp_wb, sheet_name)
  row_p <- 1
  col_p <- 1

  table_n <- length(reportTables$caption)/2

  # Tab3 ---------------------------------------------
  for(gca in 1:table_n) {

    out_name <- paste0("boot_t1_", gca)
    writeData(
      temp_wb,
      sheet_name,
      x = reportTables$caption[[out_name]],
      startCol = col_p,
      startRow = row_p,
      borders = c("all"),
      headerStyle =
        createStyle(
          border = c("top","bottom","left","right")
        )
    )
    row_p <- row_p + 1
    writeDataTable(
      temp_wb,
      sheet_name,
      x = reportTables[[out_name]],
      startCol = col_p,
      startRow = row_p
    )

    row_p <- row_p + nrow(reportTables[[out_name]]) + 1

    out_name <- paste0("boot_t2_", gca)
    writeData(
      temp_wb,
      sheet_name,
      x = reportTables$caption[[out_name]],
      startCol = col_p,
      startRow = row_p,
      borders = c("all"),
      headerStyle =
        createStyle(
          border = c("top","bottom","left","right")
        )
    )
    row_p <- row_p + 1
    writeDataTable(
      temp_wb,
      sheet_name,
      x = reportTables[[out_name]],
      startCol = col_p,
      startRow = row_p
    )

    row_p <- 1
    col_p <- col_p + ncol(reportTables[[out_name]]) + 1
  }

  return(temp_wb)

}


