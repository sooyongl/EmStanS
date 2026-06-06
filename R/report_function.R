#' @export
report <- function(x, ...) {
  UseMethod("report")
}

#' @export
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
    o$setting    <- report_setting(x$imprt_data)
    o$itemtable1 <- report_itemtable1(x$imprt_data)
    o$itemtable2 <- report_itemtable2(x$imprt_data)

    report_setup(o)
  }

  # invisible(x)
}

