#' Update Manual Cut Points in an ESS Object
#'
#' Updates an existing `ESS` object by replacing or adding manually specified
#' cut points. Manual cut points are provided through `...` as named numeric
#' vectors, where each name identifies a model, domain, scale, or other
#' component used in the `ESS` object.
#'
#' @param output An object of class `ESS`.
#' @param ... Named numeric vectors of manual cut scores Each argument should
#'   be supplied in the form `name = c(lower, upper)` or
#'   `name = c(cp1, cp2, ...)`. For example, `M1 = c(100, 200)` specifies manual
#'   cut points for `M1`.
#'
#' @return An updated object of class `ESS` containing the supplied manual cut
#'   points.
#'
#' @details
#' The values supplied through `...` are captured internally as a named list
#' using `list(...)`. Therefore, all arguments in `...` should be named.
#' These names are used to match the provided manual cut points to the
#' corresponding elements of the `ESS` object.
#'
#' @examples
#' \dontrun{
#' ess2 <- update.ESS(
#'   ess,
#'   M1 = c(100, 200),
#'   M2 = c(56, 170)
#' )
#' }
#'
#' @export
update.ESS <- function(output, ...) {
  # ... : Manual cuts corresponding to the panel
  # extract(res, "cutscore")


  # output = res; updates = list(`M1-All` = c(7, 21))
  updates <- list(...)

  if (length(updates) == 0) {
    stop("No manual cut point values were provided.")
  }

  if (is.null(names(updates)) || any(names(updates) == "")) {
    stop("All manual cut point values must be named, for example `M1-All` = c(100, 200).")
  }

  information <- output$information
  tab0 <- output$tab0
  tab1 <- output$tab1

  n.of.gca <- information$data_ready$id_list$GCA
  modal_selected_cs_all <- output$tab1$modal_selected_cs_all
  names(modal_selected_cs_all) <-
    # map(tab1$modal_est_cs_all, ~ unique(.x[[1]])) %>% unlist()
    n.of.gca

  # manual_cp <-
    # modal_selected_cs_all[which(names(modal_selected_cs_all) %in% names(manual_cutpoint))] <- updates

  updated_which <- which(names(modal_selected_cs_all) %in% names(updates))
  for(ii in seq_along(updated_which)) {
    modal_selected_cs_all[[updated_which[[ii]]]] = updates[[ii]]
  }
  manual_cs <- modal_selected_cs_all

  # Tab1 Update ----------------------------------------
  tab1 <-  update_tab1(tab0, tab1, information, manual_cs)
  output <- tab1_output(tab1, output)

  # Tab2 Update ----------------------------------------
  tab2 <- update_tab2(tab1, information)
  output <- tab2_output(tab2, output)

  ## Tab 3: Cut Score Summary --------------------------------------
  ## Summary of Cut Scores and impact data -----------------
  tab3 <-
    gen_tab3(tab1, information) %>%
    tab3_table_pagetb(.) %>%
    tab3_plots(.)

  output <- tab3_output(tab3, output)


  ## Tab 4: Item Review Ready --------------------------------
  tab4 <- gen_tab4(tab1, tab2, information)

  output <- tab4_output(tab4, output)

  output <- structure(
    output,
    class = "ESS"
  )


  invisible(output)

}
