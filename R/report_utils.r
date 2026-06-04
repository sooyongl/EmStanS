#' report_setting
#'
#' The function provides the table for setup data when the data is imported.
#' @param imprt_data a list of data from \code{\link{read_data}}
#' @return kable output
#'
report_setting<- function(imprt_data) {

    imprt_data[["setup"]] %>%
      kable(.,"html", escape = F, align = "c",
            table.attr = "style='width:50%;'",
            caption = "Setup") %>%
      kable_styling(bootstrap_options = c("striped"),
                    full_width = F,
                    position = "left",
                    font_size = 18,
                    fixed_thead = T) %>%

      row_spec(1:nrow(imprt_data[["setup"]]), color = "black") %>%
      row_spec(0, angle = 0,
               background = "floralwhite",
               extra_css = "border-bottom: 2px solid; border-bottom-color: black") %>%
      row_spec(., 1:nrow(imprt_data[["setup"]]), extra_css = "line-height: 90%;")

}

#' report_itemtable1
#'
#' The function provides the table for the number of items in the rating data when the data is imported.
#' @param imprt_data a list of data from \code{\link{read_data}}
#' @return kable output
#'
report_itemtable1 <- function(imprt_data) {
    data_name <- names(imprt_data[["rating"]])

    temp_data <-
      imprt_data[["rating"]] %>%
      set_names(nm = c("GCA","Subject","Grade","Round","Table","Panelist","Item_ID","ALD")) %>%
      group_by(GCA, Subject, Grade, Round, Panelist) %>%
      summarise(
        `Number of Item` = n(),
        .groups = "drop"
      ) %>%
      mutate(GCA = factor(GCA, levels = imprt_data[[1]]$GCA)) %>%
      arrange(GCA)

    temp_data %>%
      kable(.,"html", escape = F, align = "c",
            table.attr = "style='width:50%;'",
            caption = "Number of Items on Rating") %>%
      kable_styling(bootstrap_options = c("striped"),
                    full_width = F,
                    position = "left",
                    font_size = 18,
                    fixed_thead = T) %>%

      row_spec(1:nrow(temp_data), color = "black") %>%
      row_spec(0, angle = 0,
               background = "floralwhite",
               extra_css = "border-bottom: 2px solid; border-bottom-color: black") %>%
      row_spec(., 1:nrow(temp_data), extra_css = "line-height: 80%;")
}


#' report_itemtable2
#'
#' The function provides the table for the number of items in the item meta data when the data is imported.
#' @param imprt_data a list of data from \code{\link{read_data}}
#' @return kable output
#'
report_itemtable2 <- function(imprt_data) {

    GCA_item <- imprt_data[["item_data"]] %>%
      group_by(GCA) %>%
      summarise(
        `N.Item in GCA` = n(),
        .groups = "drop"
      ) %>%
      mutate(GCA = factor(GCA, levels = imprt_data[[1]]$GCA)) %>%
      arrange(GCA)
    kable.line <- nrow(GCA_item)

    if("Domain" %in% names(imprt_data[["item_data"]])){
      Domain_item <- imprt_data[["item_data"]] %>%
        group_by(GCA,Domain) %>%
        summarise(
          `N.Item in Domain` = n(),
          .groups = "drop"
        ) %>%
        mutate(GCA = factor(GCA, levels = imprt_data[[1]]$GCA)) %>%
        arrange(GCA)

      GCA_n <-
        imprt_data[["item_data"]] %>%
        count(GCA,Domain) %>%
        count(GCA) %>%
        mutate(GCA = factor(GCA, levels = imprt_data[[1]]$GCA)) %>%
        arrange(GCA) %>%
        pull(n)

      GCA_n <- GCA_n-1

      GCA_number <- unlist(map2(GCA_item[[2]], GCA_n, ~ c(.x, rep("",.y))))

      GCA_item <-
        GCA_item %>%
        left_join(Domain_item, by = "GCA") %>%
        select(GCA, Domain, everything()) %>%
        mutate(`N.Item in GCA` = GCA_number)

      kable.line <- GCA_item %>% count(GCA) %>% pull(n)
      if(length(kable.line) > 1) {
        for(i in 2:length(kable.line)) {
          kable.line[i] <- kable.line[i] + kable.line[i-1]
        }
      }

    }

    GCA_item <-
      GCA_item %>%
      kable(.,"html", escape = F, align = "c",
            table.attr = "style='width:50%;'",
            caption = "Number of Items on Item Meta Data") %>%
      kable_styling(bootstrap_options = c("striped"),
                    full_width = F,
                    position = "left",
                    font_size = 18,
                    fixed_thead = T) %>%

      row_spec(1:nrow(GCA_item), color = "black") %>%
      row_spec(0, angle = 0,
               background = "floralwhite",
               extra_css = "border-bottom: 2px solid; border-bottom-color: black") %>%
      # collapse_rows(columns = 1:2, valign = "top") %>%
      row_spec(., kable.line, extra_css = "border-bottom: 2px solid; border-bottom-color: black;line-height: 60%;") %>%
      row_spec(., 1:nrow(GCA_item), extra_css = "line-height: 70%;")

    return(GCA_item)

}

#' report_setup
#'
#' @param output ESS object from \code{\link{emstans}}
#' @return html output
#'
report_setup <- function(output) {

  page(
    title = "Data Setup",
    theme = bs_theme(version = 5),

    card(
      card_header("Settings"),
      card_body(
        HTML(as.character(output$setting))
      )
    ),

    card(
      card_header("Item Table 1"),
      card_body(
        HTML(as.character(output$itemtable1))
      )
    ),

    card(
      card_header("Item Table 2"),
      card_body(
        HTML(as.character(output$itemtable2))
      )
    )
  )
}

#' report_tab1
#'
#' @param output ESS object from \code{\link{emstans}}
#' @return html output
#'
report_tab1 <- function(output) {

  bslib::page(
    title = "Tab 1 Report",
    theme = bslib::bs_theme(version = 5),

    bslib::layout_column_wrap(
      width = 1,

      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Individual Summary"),
        bslib::card_body(
          output$tab1_indi
        )
      ),

      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Group Summary: Mode"),
        bslib::card_body(
          output$tab1_group_mode
        )
      ),

      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Group Summary: Median"),
        bslib::card_body(
          output$tab1_group_median
        )
      ),

      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Group Summary: Average"),
        bslib::card_body(
          output$tab1_group_average
        )
      )
    )
  )
}

#' report_tab2
#'
#' @param output ESS object from \code{\link{emstans}}
#' @return html output
#'
report_tab2 <- function(output) {

  gca_name <- output$information$data_ready$id_list$GCA

  tabs <- lapply(seq_along(gca_name), function(i) {

    bslib::nav_panel(
      title = paste0(gca_name[i]),

      bslib::navset_tab(
        id = paste0("tab2_inner_", i),

        bslib::nav_panel(
          title = "Table",

          bslib::card(
            full_screen = TRUE,
            fill = TRUE,
            height = "100%",

            bslib::card_body(
              fillable = TRUE,
              fill = TRUE,
              style = "overflow-y: auto;",
              output[[paste0("t1_", i, "_1")]]
            )
          )
        ),

        bslib::nav_panel(
          title = "Plot",

          bslib::card(
            full_screen = TRUE,
            bslib::card_body(
              output[[paste0("p1_", i, "_1")]]
            )
          ),

          bslib::card(
            full_screen = TRUE,
            bslib::card_header("Plot data"),
            bslib::card_body(
              output[[paste0("gam_", i, "_1")]]
            )
          )
        ),

        bslib::nav_panel(
          title = "Crosstab",

          bslib::layout_columns(
            col_widths = c(6, 6),
            height = "100%",

            bslib::card(
              full_screen = TRUE,
              height = "100%",
              bslib::card_header("Cross Tab"),
              bslib::card_body(
                style = "overflow-y: auto;",
                HTML(as.character(output[[paste0("ct_", i, "_1")]]))
              )
            ),

            bslib::card(
              full_screen = TRUE,
              height = "100%",
              bslib::card_header("Cross Tab Effect"),
              bslib::card_body(
                style = "overflow-y: auto;",
                HTML(as.character(output[[paste0("ct_e_", i, "_1")]]))
              )
            )
          )
        ),

        bslib::nav_panel(
          title = "Summary",
          HTML(as.character(output[[paste0("sum_", i, "_1")]]))
        )
      )
    )
  })

  bslib::page_fillable(
    title = "Tab 2 Report",
    theme = bslib::bs_theme(version = 5),
    padding = 0,
    gap = 0,

    htmltools::tags$style(htmltools::HTML("
      html, body {
        height: 100%;
      }

      .bslib-page-fill {
        height: 100vh;
      }

      .tab-content {
        height: calc(100vh - 45px);
      }

      .tab-pane {
        height: 100%;
      }

      .tab-pane > .tabbable {
        height: 100%;
      }

      .tab-pane > .tabbable > .tab-content {
        height: calc(100% - 45px);
      }

      .card {
        height: 100%;
      }

      .card-body {
        overflow-y: auto;
      }
    ")),

    do.call(
      bslib::navset_tab,
      c(tabs, list(id = "tab2_outer"))
    )
  )
}


#' report_tab3
#'
#' @param output ESS object from \code{\link{emstans}}
#' @return html output
#'
report_tab3 <- function(output) {

  gca_id <- output$information$data_ready$id_list$GCA
  tab_id <- unique(str_split(gca_id, "-", simplify = T)[,2])

  tabs <- lapply(1:length(tab_id), function(i) {

    nav_panel(
      paste0(tab_id[i]),

      navset_tab(
        nav_panel("Table",
                  HTML(as.character(output[[paste0("tab3table_", i)]]))
        ),
        nav_panel("Plot",
                  plotly::ggplotly(output[[paste0("tab3p1_", i)]]),
                  plotly::ggplotly(output[[paste0("tab3p2_", i)]]),
                  plotly::ggplotly(output[[paste0("tab3p3_", i)]])
        )

      )
    )
  })

  do.call(navset_tab, tabs)
}

#' report_tab4
#'
#' @param output ESS object from \code{\link{emstans}}
#' @return html output
#'
report_tab4 <- function(output) {

  gca_name <- output$information$data_ready$id_list$GCA

  tabs <- lapply(seq_along(gca_name), function(i) {

    bslib::nav_panel(
      title = paste0(gca_name[i]),

      bslib::card(
        full_screen = TRUE,
        fill = TRUE,

        bslib::card_body(
          style = "height: calc(100vh - 90px); overflow-y: auto;",
          output[[paste0("out4__", i, "_1")]]
        )
      )
    )
  })

  bslib::page_fillable(
    title = "Tab 4 Report",
    theme = bslib::bs_theme(version = 5),
    padding = 0,

    do.call(bslib::navset_tab, tabs)
  )
}

