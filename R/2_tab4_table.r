#' @include 2_tab3_function.r
NULL

#' Generate DataTable obejct for item review table
#'
tab4_table_review <- function(tab4_split) {
  # tab4_split <- GCA_split[[1]]
  maxRow <- nrow(tab4_split)

  # create a character vector of shiny inputs
  shinyInput = function(FUN, len, id, ...) {
    inputs = character(len)
    for (i in seq_len(len)) {
      inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...))
    }
    inputs
  }

  tab4_out <- tab4_split %>%
    mutate_if(is.numeric, round,4) %>%
    mutate(
      excluded =
        shinyInput(FUN = shinyWidgets::awesomeCheckbox,
                   len = nrow(tab4_split),
                   id = str_replace_all(unique(tab4_split$GCA), "\\.","_"),
                   status = "danger"
        )
    )

  item_review <-
    datatable(tab4_out,
                  escape = FALSE,
                  selection = 'multiple',
                  rownames = F,
                  extensions = 'FixedHeader',
                  options = table_options_3(maxRow),
                  filter = 'top'
    ) %>%
    formatStyle(1:nrow(tab4_out),
                target= 'row',
                lineHeight='70%')

  item_review
}

# Helper functions ----------------------------------------------
table_options_3 <- function(maxRow){
  list(
    compact=TRUE,
    dom = 'Bftrip',
    pageLength = maxRow,
    scrollX = T,
    scroller = TRUE,
    fixedHeader = F,
    autoWidth = F,
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '##DEF7F9', 'color': '#000', 'font-weight': 'bold', 'text-align': 'center'});",
      "}"
    ),
    columnDefs = list(
      list(className = 'dt-center', targets = "_all")
    ),

    preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
    drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
  )
}
