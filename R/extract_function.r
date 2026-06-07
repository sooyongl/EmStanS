#' @export
extract <- function(x, ...) {
  UseMethod("extract")
}

#' S3 method
#'
#' @exportS3Method
extract.bootESS <- function(x, what = "all",...) {

  # x = boot_res
  list(
    bootstrapping_cutscore = x$bootResults$table1,
    bootstrapping_summary = x$bootResults$table2,
    bootstrapping_plot = x$bootResults$boot_p
  )
}

#' Extract Components from an ESS Object
#'
#' `extract.ESS()` extracts selected results from an object of class `"ESS"`.
#' The function is an S3 method for ESS objects and allows users to retrieve
#' specific components of the Embedded Standard Setting output, such as
#' individual-level results, detailed results, summary tables, review tables,
#' or cut scores.
#'
#' @param x An object of class `"ESS"` returned by `emstans()`.
#'
#' @param what A character string indicating which component to extract. Available
#'   options are `"all"`, `"individual"`, `"detailed"`, `"summary"`, `"review"`,
#'   and `"cutscore"`. If `"all"`, all available extractable components are
#'   returned. Default is `"all"`.
#'
#' @param ... Additional arguments passed to or from other methods. Currently not
#'   used.
#'
#' @return An object extracted from `x`, depending on the value of `what`.
#'   If `what = "all"`, a list of available ESS output components is returned.
#'   For other values of `what`, the corresponding ESS component is returned.
#'
#' @details
#' This function provides a convenient interface for accessing stored results in
#' an ESS object without directly indexing the internal list structure. It is
#' intended to be used after running `emstans()`.
#'
#' @exportS3Method
extract.ESS <- function(x, what = "all",...) {


  if(what == "all") {

    list(
      Individual = list(individual_cut = x$individual_cut,
           median_cut = x$median_cut,
           modal_cut = x$modal_cut,
           average_cut = x$average_cut),
      Detailed = list(
        summary_table = x$summary_table,
        cutscore_table = x$cutscore_table,
        crosstab_table = x$crosstab_table,
        gam_fit = x$gam_fit),

      `CS Summary` = list(effective_table = x$effective_table),

      `Item Review`= list(item_review = x$distance_output)
    )


  } else if(what == "individual") {
    list(individual_cut = x$individual_cut,
         median_cut = x$median_cut,
         modal_cut = x$modal_cut,
         average_cut = x$average_cut)

  } else if(what == "detailed") {

    list(
      summary_table = x$summary_table,
      cutscore_table = x$cutscore_table,
      crosstab_table = x$crosstab_table,
      gam_fit = x$gam_fit)

  } else if(what == "summary") {

    list(effective_table = x$effective_table)

  } else if(what == "review") {

    list(item_review = x$distance_output)

  } else if(what == "cutscore") {


    information <- x$information
    tab0 <- x$tab0
    tab1 <- x$tab1

    n.of.gca <- information$data_ready$id_list$GCA
    modal_selected_cs_all <- x$tab1$modal_selected_cs_all
    names(modal_selected_cs_all) <-
      # map(tab1$modal_est_cs_all, ~ unique(.x[[1]])) %>% unlist()
      n.of.gca

    modal_selected_cs_all
  }
}


