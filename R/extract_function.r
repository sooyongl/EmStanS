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

#' S3 method
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


