#' @export
emstans <- function(filePath = NULL,
                    data = NULL,
                    tests = NULL,

                    targets = "ALD",
                    WESS = T,
                    gamest = F,
                    median = "modal",
                    loc = "RP67",
                    domain = "GCA",
                    select_domain = NULL,
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

  if(is.null(tests)) {

    input$tests <-  as.character(setup_data$GCA)

  } else if(is.numeric(tests)) {

    input$tests <-  as.character(setup_data$GCA[tests])

  } else if(is.character(tests)) {

    input$tests <-  as.character(setup_data$GCA[setup_data$GCA %in% tests])
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
