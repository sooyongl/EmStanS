#' Run Embedded Standard Setting Analysis
#'
#' `emstans()` conducts Embedded Standard Setting analysis using either an Excel
#' file or a prepared list of data frames. The function reads test setup
#' information, panelist information, item ratings, item metadata, and optional
#' student data, then computes cut scores and related summary outputs.
#'
#' @param filePath A character string giving the path to an Excel file. The Excel
#'   file must contain five sheets: test setup, panelist information, item
#'   ratings, item metadata, and student data. The student data sheet may be
#'   empty, but the other four sheets must contain the required information.
#'   When `filePath = NULL`, `data` must be provided. See
#'   `EmStanS::genFakeDataSet()` for an example of the required data structure.
#'   Default is `NULL`.
#'
#' @param data A list of data frames containing test setup information, panelist
#'   information, item ratings, item metadata, and student data. When
#'   `data = NULL`, `filePath` must be provided. Default is `NULL`.
#'
#' @param grade Either `NULL`, a numeric vector, or a character vector indicating
#'   which grade clusters should be used in the analysis. If `NULL`, all grade
#'   clusters in the setup data are used. If numeric, values are interpreted as
#'   the positions of the grade clusters to select. If character, values are
#'   matched to the grade-cluster names. Default is `NULL`.
#'
#' @param domain A character string giving the column name in the setup data that
#'   identifies grade clusters. A grade-cluster identifier usually combines the
#'   first letter of the domain with the grade level, such as `"M1"` for Math
#'   Grade 1 or `"R3"` for Reading Grade 3. Default is `"GCA"`.
#'
#' @param targets A character string giving the column name in the item-rating
#'   data that identifies the aligned level description. This column is commonly
#'   named `"ALD"`. Default is `"ALD"`.
#'
#' @param loc A character string giving the column name in the item-metadata data
#'   that contains the item location or difficulty values used to order items.
#'   Default is `"RP67"`.
#'
#' @param select_domain Either `NULL` or a character vector indicating specific
#'   subdomains to analyze, such as vocabulary in reading or algebra in math. If
#'   `NULL`, all subdomains are combined. Default is `NULL`.
#'
#' @param median A character string indicating the method used to summarize cut
#'   scores across panelists and tables. `"modal"` uses the modal cut score, and
#'   `"median"` uses the median cut score. Default is `"modal"`.
#'
#' @param WESS A logical value indicating whether Weighted Embedded Standard
#'   Setting should be used to compute cut scores. If `FALSE`, the count-based
#'   method is used. Default is `TRUE`.
#'
#' @param gamest A logical value indicating whether a generalized additive model
#'   should be used to compute cut scores. Default is `FALSE`.
#'
#' @param font_size A numeric value specifying the font size used in plots.
#'   Default is `14`.
#'
#' @param digits A numeric value specifying the number of decimal places used in
#'   output tables and summaries. Default is `3`.
#'
#' @return An object of class `"ESS"`. The returned object contains Embedded
#'   Standard Setting results, including cut scores, summary tables, plots, and
#'   information used for reporting. This object can be used with S3 methods such
#'   as `print()`, `summary()`, `report()`, and `update()`, if those methods are
#'   defined for the `"ESS"` class.
#'
#' @details
#' Either `filePath` or `data` must be supplied. If `filePath` is provided, the
#' function reads the required Excel sheets and prepares the input data
#' internally. If `data` is provided, it should already follow the structure
#' expected by the EmStanS package.
#'
#' The function can compute cut scores using either count-based Embedded Standard
#' Setting or Weighted Embedded Standard Setting. It can also optionally estimate
#' cut scores using a generalized additive model.
#'
#' @examples
#' \dontrun{
#' fake_data <- EmStanS::genFakeDataSet(
#'   ngca = 3,
#'   cor_val = 0.2,
#'   n = 30,
#'   nlevel = 3,
#'   ntable = 5,
#'   npanelist = 5,
#'   sdinp = 1,
#'   ecinp = 0,
#'   100,
#'   300
#' )
#'
#' res <- emstans(
#'   data = fake_data,
#'   domain = "GCA",
#'   targets = "ALD",
#'   loc = "RP67",
#'   median = "modal",
#'   WESS = TRUE,
#'   gamest = FALSE
#' )
#' }
#'
#' @export
emstans <- function(filePath = NULL,
                    data = NULL,
                    grade = NULL,
                    domain = "GCA",
                    targets = "ALD",
                    loc = "RP67",
                    median = "modal",
                    select_domain = NULL,
                    WESS = T,
                    gamest = F,
                    font_size = 14,
                    digits = 3) {

  if(is.null(filePath) & is.null(data)) {
    message("Either filePath or data must be provided")
  }

  # Containers ---------------
  input <- list()
  output <- list()

  input$targets <- targets #= "ALD"#
  input$WESS <- WESS # = T
  input$gamest <- gamest # = F
  input$median <- median # = "modal"
  input$loc <-  loc # = "RP67" # "RP50" #"Loc_RP50" #

  input$domain <- domain # =  c("GCA") # "GCA" ; "Domain"

  input$select_domain <- select_domain # = NULL
  input$font_size <- font_size # = 14

  # Data imported --------------------------------
  # filePath = fs::dir_ls("prerun/data/example_file")[1]
  input$setups$datapath <- filePath

  # imprt_data <- data
  imprt_data <- if(is.null(filePath)) {
    data
  } else {

    sheets_name <- input$setups$datapath %>% excel_sheets()
    read_data(filePath = input$setups$datapath, sheets_name = sheets_name)
  }

  output$imprt_data <- imprt_data

  # Validate Data file ----------------------------
  validateData(imprt_data)

  # Data ready for analysis
  data_list <- data_ready(imprt_data)

  output$data_list <- data_list

  setup_data    <- data_list$setup_data
  panelist_data <- data_list$panelist_data
  rating_data   <- data_list$rating_data
  item_data     <- data_list$item_data
  examinee_data <- data_list$examinee_data


  # Information ready --------------------------------------

  if(is.null(grade)) {

    input$tests <-  as.character(setup_data$GCA)

  } else if(is.numeric(grade)) {

    input$tests <-  as.character(setup_data$GCA[grade])

  } else if(is.character(grade)) {

    input$tests <-  as.character(setup_data$GCA[setup_data$GCA %in% grade])
  }


  information <-
    get_data_info(
      data_list = data_list,
      gca       = input$tests,
      ald       = "ALD",
      location  = input$loc,
      WESS      = input$WESS,
      gamest    = input$gamest,
      domain    = input$domain,
      select_domain = input$select_domain,
      modal     = F
    )

  output$information <- information


  # Tab 0 --------------------------
  ## This provides all fundamental results
  tab0 <- gen_tab0(information = information)

  output$tab0 <- tab0

  # Tab 1: Compute Cut scores --------------------------
  tab1 <- gen_tab1(tab0, information)

  # Main output:
  # res : individual result
  # median: median_res_com
  # mode: modal_res_com
  # average: average_res_com

  output <- tab1_output(tab1, output)

  # output$individual_cut <- tab1$res
  # output$median_cut <- tab1$median_res_com
  # output$modal_cut <- tab1$modal_res_com
  # output$average_cut <- tab1$average_res_com


  ## Tab 2: Obtain Data for Display (Cut Scores)-------
  tab2 <- gen_tab2(tab1, information)

  # tab2$for_tab2_out$`R0-All`[[1]]$eff_data
  # tab2$for_tab2_out$`R0-All`[[1]]$t_out
  # tab2$for_tab2_out$`R0-All`[[1]]$crosst
  # tab2$for_tab2_out$`R0-All`[[1]]$gam_fitted

  output <- tab2_output(tab2, output)


  ## Tab 3: Cut Score Summary --------------------------------------
  ## Summary of Cut Scores and impact data -----------------
  tab3 <-
    gen_tab3(tab1, information) %>%
    tab3_table_pagetb(.) %>%
    tab3_plots(.)

  output <- tab3_output(tab3, output)


  ## Tab 4: Item Review Ready --------------------------------
  tab4 <- gen_tab4(tab1, tab2, information)

  output <- tab4_output(tab4, output)

  output$input <- input

  output <- structure(
    output,
    class = "ESS"
  )


  invisible(output)

}
