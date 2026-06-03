#' @include 2_tab0_function.r
NULL

#' Generate DataTable for individual ESS results
#'
#' @param tab1_indi_res a data frame

dt_table_out_indi <- function(tab1_indi_res, table_options){
  # tab1_indi_res <- tab1$indi_table; table_options <- table_options_new_1

  eval_length <- length(unique(str_split(tab1_indi_res$GCA, "-", simplify = T)[,2]) == "All") == 1
  if(eval_length) {
    tab1_indi_res <- tab1_indi_res %>%
      separate(GCA, c("GCA","all"), '-') %>%
      select(-all)
  } else {
    tab1_indi_res <- tab1_indi_res %>%
      separate(GCA, c("GCA","Domain"), '-')
  }

  # if(dim(tab1_indi_res)[1] <= 20) {
  # maxRow <- 20
  # } else {
  # maxRow <- tab1_indi_res %>% count(GCA) %>% pull(2) %>% .[1]
  # if(maxRow == 1) {
  maxRow <- 100
  # }
  # }
  # maxRow <- nrow(tab1_indi_res)

  grades <- tab1_indi_res %>% pull(1) %>% unique()

  level_name <- names(tab1_indi_res)[get_which(names(tab1_indi_res), "_p")]
  level_name <-
    str_split(level_name, "_p") %>%
    map(., ~ .x[[1]]) %>%
    unlist() %>%
    str_replace(., "L", "Level")
  num_level <- length(level_name)

  levels <-
    foreach(pi = 1:num_level, .combine = 'c') %do% {
      glue::glue("th(colspan = 2, '{level_name[pi]}'),")

    } %>%
    paste(., collapse = "\n")

  if(eval_length) {
    con_dt <- glue(
      "container_dt = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'GCA'),
      th(rowspan = 2, 'Table'),
      th(rowspan = 2, 'Panelist'),
      th(rowspan = 2, 'Correlation'),

      th(colspan = {num_level}, 'Pages'),

      {levels}

      th(colspan = 2, 'SUM'),

    tr(
      lapply(c(level_name,rep(c('Count','Weight'), (num_level+1))), th)
      )

      )
    )
  )
)"
    )

    row_group_point <- 1

  } else {
    con_dt <- glue(
      "container_dt = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'GCA'),
      th(rowspan = 2, 'Domain'),
      th(rowspan = 2, 'Table'),
      th(rowspan = 2, 'Panelist'),
      th(rowspan = 2, 'Correlation'),

      th(colspan = {num_level}, 'Pages'),

      {levels}

      th(colspan = 2, 'SUM'),

    tr(
      lapply(c(level_name,rep(c('Count','Weight'), (num_level+1))), th)
      )

      )
    )
  )
)"
    )

    row_group_point <- 2
  }

  datatable(tab1_indi_res,
            container = eval(parse(text = con_dt)),
            class = 'table-bordered stripe table-condensed;',
            rownames = F,
            extensions = c('RowGroup'),
            options = match.fun(table_options)(maxRow, row_group_point)
  ) %>%
    formatStyle(1, backgroundColor = styleEqual(grades,colors[1:length(grades)])
    ) %>%
    formatStyle(names(tab1_indi_res),lineHeight='80%')

}

#' Generate DataTable for modal ESS results
#'
#' @param tab1_indi_res a data frame
dt_table_out_mode <- function(tab1_indi_res, table_options){

  eval_length <- length(unique(str_split(tab1_indi_res$GCA, "-", simplify = T)[,2]) == "All") == 1
  if(eval_length) {
    tab1_indi_res <- tab1_indi_res %>%
      separate(GCA, c("GCA","all"), '-') %>%
      select(-all)
  } else {
    tab1_indi_res <- tab1_indi_res %>%
      separate(GCA, c("GCA","Domain"), '-')
  }

  if(dim(tab1_indi_res)[1] <= 5) {
    maxRow <- 5
  } else {
    maxRow <- maxRow <- nrow(tab1_indi_res)
  }

  grades <- tab1_indi_res %>% pull(1) %>% unique()

  tab1_indi_res$Correlation <- round(tab1_indi_res$Correlation, 2)

  level_name <- names(tab1_indi_res)[str_detect(names(tab1_indi_res), "_p")]
  level_name <-
    str_split(level_name, "_p") %>%
    map(., ~ .x[[1]]) %>%
    unlist() %>%
    str_replace(., "L", "Level")
  num_level <- length(level_name)

  levels <-
    foreach(pi = 1:num_level, .combine = 'c') %do% {
      glue::glue("th(colspan = 2, '{level_name[pi]}'),")

    } %>% paste(., collapse = "\n")


  if(eval_length) {
    con_dt <- glue(
      "container_dt = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'GCA'),
      th(rowspan = 2, 'Table'),
      th(rowspan = 2, 'Correlation'),

      th(colspan = {num_level}, 'Pages'),

      {levels}

      th(colspan = 2, 'SUM'),

    tr(
      lapply(c(level_name,rep(c('Count','Weight'), (num_level+1))), th)
      )

      )
    )
  )
)"
    )

  } else {
    con_dt <- glue(
      "container_dt = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'GCA'),
      th(rowspan = 2, 'Domain'),
      th(rowspan = 2, 'Table'),
      th(rowspan = 2, 'Correlation'),

      th(colspan = {num_level}, 'Pages'),

      {levels}

      th(colspan = 2, 'SUM'),

    tr(
      lapply(c(level_name,rep(c('Count','Weight'), (num_level+1))), th)
      )

      )
    )
  )
)"
    )

  }

  dt.out <- DT::datatable(tab1_indi_res,
                          container = eval(parse(text = con_dt)),
                          class = 'table-bordered stripe table-condensed',
                          rownames = F,
                          extensions = c('RowGroup'),
                          options = match.fun(table_options)(maxRow)
  )

  dt.out %>%
    formatStyle(1,
                backgroundColor = styleEqual(grades,
                                             colors[1:length(grades)]
                )
    ) %>%
    formatStyle(1:nrow(tab1_indi_res),
                target= 'row',
                lineHeight='70%')
}

#' Generate DataTable for median ESS results
#'
#' @param tab1_indi_res a data frame
dt_table_out_med <- function(tab1_indi_res, table_options){

  eval_length <- length(unique(str_split(tab1_indi_res$GCA, "-", simplify = T)[,2]) == "All") == 1
  if(eval_length) {
    tab1_indi_res <- tab1_indi_res %>%
      separate(GCA, c("GCA","all"), '-') %>%
      select(-all)
  } else {
    tab1_indi_res <- tab1_indi_res %>%
      separate(GCA, c("GCA","Domain"), '-')
  }


  if(dim(tab1_indi_res)[1] <= 5) {
    maxRow <- 5
  } else {
    maxRow <- maxRow <- nrow(tab1_indi_res)
  }

  grades <- tab1_indi_res %>% pull(1) %>% unique()

  level_name <- names(tab1_indi_res)[str_detect(names(tab1_indi_res), "_p")]
  level_name <-
    str_split(level_name, "_p") %>%
    map(., ~ .x[[1]]) %>%
    unlist() %>%
    str_replace(., "L", "Level")
  num_level <- length(level_name)

  levels <-
    foreach(pi = 1:num_level, .combine = 'c') %do% {
      glue::glue("th(colspan = 2, '{level_name[pi]}'),")

    } %>% paste(., collapse = "\n")


  con_dt <- glue::glue(
    "container_dt = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'GCA'),
      th(rowspan = 2, 'Table'),

      th(colspan = {num_level}, 'Pages'),


    tr(
      lapply(c(level_name), th)
      )

      )
    )
  )
)"
  )

  if(eval_length) {
    con_dt <- glue(
      "container_dt = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'GCA'),
      th(rowspan = 2, 'Table'),

      th(colspan = {num_level}, 'Pages'),


    tr(
      lapply(c(level_name), th)
      )

      )
    )
  )
)"
    )

  } else {
    con_dt <- glue(
      "container_dt = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'GCA'),
      th(rowspan = 2, 'Domain'),
      th(rowspan = 2, 'Table'),

      th(colspan = {num_level}, 'Pages'),


    tr(
      lapply(c(level_name), th)
      )

      )
    )
  )
)"
    )

  }

  datatable(tab1_indi_res,
            container = eval(parse(text = con_dt)),
            class = 'table-bordered stripe table-condensed',
            rownames = F,
            extensions =
              c('RowGroup'),
            options = match.fun(table_options)(maxRow)

  ) %>%
    formatStyle(1,backgroundColor = styleEqual(grades,colors[1:length(grades)])
    ) %>%
    formatStyle(1:nrow(tab1_indi_res),
                target= 'row',
                lineHeight='70%')
}

# Helper functions -------------------------------------------
# table options
table_options_new_1 <- function(maxRow, row_group_point){
  list(
    dom = 'Bftrip',
    pageLength = maxRow,
    scrollX = T,
    scroller = TRUE,
    fixedHeader = TRUE,
    autoWidth = F,
    bFilter = F,

    rowGroup = list(dataSrc = c(row_group_point)),

    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '##DEF7F9', 'color': '#000', 'font-weight': 'bold', 'text-align': 'center'});",
      "}"
    ),
    columnDefs = list(
      list(
        className = 'dt-center', targets = "_all"
      )
    )
  )
}

#
table_options_new_2 <- function(maxRow){
  list(
    dom = 't',
    pageLength = maxRow,
    scrollX = T,
    scroller = TRUE,
    fixedHeader = TRUE,
    autoWidth = F,
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '##DEF7F9', 'color': '#000', 'font-weight': 'bold', 'text-align': 'center'});",
      "}"
    ),
    columnDefs = list(
      list(
        className = 'dt-center', targets = "_all"
      )
    )
  )
}
