#' @export
extract <- function(x, ...) {
  UseMethod("extract")
}

#' S3 method
#'
#' @export
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

  }
}


