for (i in fs::dir_ls("R")) {source(i)}

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(shinyalert)
library(waiter)
library(shinycssloaders)
library(bslib)
library(DT)
library(data.table)
library(tidyverse)
library(foreach)
library(glue)
library(mgcv)
library(tidymv)

fluidPage(
  # useShinydashboard(),
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css",
  #             href = file.path("inst","extdata", "theme.css"))
  # ),
  theme=bs_theme(version = 4,
                 bootswatch = "journal",
                 primary = "#ED79F9",
                 base_font = font_google("Work Sans") #
  ),

  navbarPage(

    # title -------------------------------------------------
    title = "EmStanS (0.0.0)",

    #---------------------------------------------------------
    # Data Import
    #---------------------------------------------------------
    tabPanel("Data Import",
             fluidRow(
               column(2,
                      prettyRadioButtons("empirical",
                                         label = "Empiricial or Simulated",
                                         choices = c("empirical", "simulated"),
                                         selected = "empirical",
                                         status = "danger",
                                         icon = icon("check"),
                                         bigger = TRUE,
                                         animation = 'smooth')
               ),
               column(10,
                      uiOutput("data_import")
               )
             )
    ),

    tabPanel("Analysis",

             fluidRow(
               column(1,
                      actionButton("run1", "Run")
               ),

               column(1,
                      materialSwitch(
                        inputId = "WESS",
                        label = strong("Count"),
                        value = TRUE,
                        status = "danger")
               ),


               column(1,strong("Weight")),

               column(1,
                      switchInput(
                        inputId = "gamest",
                        label = "Smoothing",
                        value = T,
                        onStatus = "danger",
                        offStatus = "sucess",
                        labelWidth = "80px")
               )
             ),

             fluidRow(
               # verbatimTextOutput("results")
               tabsetPanel(
                 tabPanel("Model Fit",
                          downloadButton("ess_weight", "Download"),
                          DTOutput("result0")),

                 tabPanel("Review",
                          downloadButton("review", "Download"),
                          DTOutput("result1")
                          )
               )
             )
    )
  )
)##########################  shiny UI last line  #######################
