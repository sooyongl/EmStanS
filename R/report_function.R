
report <- function(x, ...) {
  UseMethod("report")
}

report.ESS <- function(x, what = "all") {

  if(what == "all") {

    page_fillable(
      navset_tab(
        nav_panel(
          "Individual",
          report_tab1(x)
        ),
        nav_panel(
          "Detailed",
          report_tab2(x)
        ),
        nav_panel(
          "CS Summary",
          report_tab3(x)
        ),

        nav_panel(
          "Item Review",
          report_tab4(x)
        )

      )

    )


  } else if(what == "individual") {
    report_tab1(x)

  } else if(what == "detailed") {
    report_tab2(x)

  } else if(what == "summary") {
    report_tab3(x)

  } else if(what == "review") {
    report_tab4(x)

  } else if(what == "setup") {

    o <- list()
    o$setting    <- print_setting(x$imprt_data)
    o$itemtable1 <- print_itemtable1(x$imprt_data)
    o$itemtable2 <- print_itemtable2(x$imprt_data)

    report_setup(o)
  }

  # invisible(x)
}

report_setup <- function(output) {
  # browsable(
  #   tagList(
  #     HTML(as.character(output$setting)),
  #     br(),
  #     HTML(as.character(output$itemtable1)),
  #     br(),
  #     HTML(as.character(output$itemtable2))
  #   )
  # )
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

report_tab1 <- function(output) {
  # htmltools::browsable(
  #   tagList(
  #     div(output$tab1_indi),
  #     br(),
  #     div(output$tab1_group_mode),
  #     br(),
  #     div(output$tab1_group_median),
  #     br(),
  #     div(output$tab1_group_average)
  #   )
  # )

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

# report_tab4 <- function(output) {
#
#   n.of.gca <- output$information$data_ready$id_list$GCA
#   n.of.tb <- rep(1, length(n.of.gca))
#
#
#   tabs <- lapply(1:length(n.of.tb), function(i) {
#
#     nav_panel(
#       paste0(n.of.gca[i]),
#
#       navset_tab(
#         nav_panel("Table",
#                   bslib::card(
#                     full_screen = TRUE,
#                     # bslib::card_header("Item Review"),
#                     bslib::card_body(output[[paste0("out4__", i, "_1")]])
#                   )
#
#
#         )
#
#
#       )
#     )
#   })
#
#   do.call(navset_tab, tabs)
# }

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

