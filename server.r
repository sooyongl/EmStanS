shiny_server <- function(input, output, session) {
  #
  # Data part ------------------------------------------
  output$data_import <- renderUI({
    if(input$empirical == "empirical") {
      fluidRow(
        column(4,
               fileInput("setups", "Choose csv file for Setup",
                         multiple = FALSE, accept = c("csv")),
               textInput("Level name", "lvname"),


               actionButton("import", "IMPORT")
        ),

        column(8,
               tabsetPanel(
                 # verbatimTextOutput("gen_data")
                 tabPanel("Imported data",DTOutput("gen_data"))
               ))
      )
    } else {
      fluidRow(
        column(2,
               numericInput(
                 inputId = "nitem",
                 label = "Num of Items (Max:1000)",
                 value = 30, min = 10, max = 1000, step = 1),

               numericInput(
                 inputId = "nlevel",
                 label = "Num of levels (Max:5)",
                 value = 3, min = 2,max = 5, step = 1),

               numericInput(
                 inputId = "cor_value",
                 label = "Correlation bw Loc and ALD",
                 value = 0.6, min = 0, max = 1),

               numericInput(
                 inputId = "sd",
                 label = "SD",
                 value = 1, min = 1, max = 100, step = 1),

               numericInput(
                 inputId = "ec",
                 label = "EC",
                 value = 0, min = 0, max = 100, step = 1),

               actionButton("import", "Import data")),

        column(10,
               downloadButton("sim_data_down", "Download"),
               tabsetPanel(
                 tabPanel("Imported data",DTOutput("gen_data"))
               )
        )
      )
    }
  })


  imprt_data <- eventReactive(input$import, {
    if(input$empirical == "empirical") {

      inpdata <- read.csv(input$setups$datapath)


    } else {
      nitem       <- input$nitem
      nlevel      <- input$nlevel
      cor_value   <- input$cor_value
      sd          <- input$sd
      ec          <- input$ec

      inpdata <-
        genFakeData(
          fun = 'runif',
          cor_value = cor_value,
          nlevel = nlevel,
          n = nitem,
          100,
          300)
    }

    return(inpdata)
  })

  observeEvent(imprt_data(), {

    output$gen_data <- renderDT({
      imprt_data()})
  })




  output$sim_data_down <- downloadHandler(
    filename = function() {
      paste("simulated_data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(imprt_data(), file, row.names = F)
    }
  )


  res_emstans <- eventReactive(input$run1, {

    if(input$empirical == "empirical") {
      lvname <- input$lvname
      lvname <- str_remove_all(lvname, " ")
      lvname <- str_split(lvname, ",", simplify = T)[1,]

    } else {
      lvname <- paste0("Level",1:input$nlevel)
    }

    res <- emstans(data = imprt_data(), lvname = lvname)
    # res <- emstans(data = inpdata, lvname = lvname)
    res[[1]]
    return(res)
  })


  observeEvent(input$run1, {

    output$result0 <- renderDT({
      DT::datatable(res_emstans()[[1]])
    })

    output$result1 <- renderDT({
      res_emstans()[[2]]
    })

  })


  output$ess_weight <- downloadHandler(
    filename = function() {
      paste("ess_weight", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(res_emstans()[[1]], file, row.names = F)
    }
  )

  output$review <- downloadHandler(
    filename = function() {
      paste("review", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(res_emstans()[[2]], file, row.names = F)
    }
  )
}##################  Shiny Server last line   ############################
