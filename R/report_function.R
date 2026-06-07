#' @export
report <- function(x, ...) {
  UseMethod("report")
}

#' Extract Components from an ESS Object
#'
#' `extract.ESS()` extracts selected components from an object of class `"ESS"`.
#' It provides a convenient interface for retrieving stored Embedded Standard
#' Setting results without directly indexing the internal object structure.
#'
#' @param x An object of class `"ESS"` returned by `emstans()`.
#'
#' @param what A character string indicating which component to extract. Available
#'   options are `"all"`, `"individual"`, `"detailed"`, `"summary"`, `"review"`,
#'   and `"setup"`. If `what = "all"`, all available extractable components are
#'   returned. Default is `"all"`.
#'
#' @param ... Additional arguments passed to or from other methods. Currently not
#'   used.
#'
#' @return An extracted component from `x`, depending on the value of `what`.
#'   If `what = "all"`, a list of available ESS output components is returned.
#'   If `what` is one of `"individual"`, `"detailed"`, `"summary"`, `"review"`,
#'   or `"setup"`, the corresponding ESS component is returned.
#'
#' @details
#' This function is intended for use after running `emstans()`. It is an S3
#' method for objects of class `"ESS"`.
#'
#' @exportS3Method
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

