#' @include EmStanS-package.r
NULL

#' Check input data for validation
#'
#' @param imprt_data a list containing imported data such as setup, panelists, rating, item meta, and examinee data, which comes from \code{\link{read_data}}.
#' @return a list containing messages related to validation.
#' @export
validateData <- function(imprt_data) {

  val_obj <- list()

  setup <- imprt_data[["setup"]];
  panelist <- imprt_data[["panelist"]]
  rating <- imprt_data[["rating"]]
  item_data <- imprt_data[["item_data"]]
  examinee_data <- imprt_data[["examinee_data"]]

  # Overall validatino
  val_obj$overall_validated <-
    if(length(imprt_data) < 5) {
      "The number of sheets is less than 5."
    } else if (length(imprt_data) > 5) {
      "The number of sheets is more than 5."
    }

  # for setup ------------------------------------------
  if(!"SD" %in% names(setup) ) {
    val_obj$setup_sd <- "- SD is not provided; SD is assumed to be 1"
  }

  if(sum(str_detect(names(setup), "^EC")) == 0) {
    val_obj$setup_ec <- "- EC is not provided; EC is assumed to be 0"
  }

  # for panelist ------------------------------------------
  rating_panel <- rating[str_detect(names(rating), "User|Panel")] %>% pull() %>% unique()
  panel_panel <- panelist[str_detect(names(panelist), "User|Panel")]%>% pull() %>% unique()

  if(length(rating_panel) != length(panel_panel)) {

    val_obj$panel_ec <- "Panelist IDs are not matched in the rating and panelist data"
  }


  # for rating ------------------------------------------
  data_name <- names(imprt_data[["rating"]])

  rating <- imprt_data[["rating"]] %>%
    select(1:8) %>%
    set_names(nm = c("GCA","Subject","Grade","Round","Table","Panelist","Item_ID","ALD")) %>%
    group_by(GCA, Subject, Grade, Round, Panelist) %>%
    summarise(
      `Number of Item_rating` = n()
    )

  item_data <- imprt_data[["item_data"]] %>%
    group_by(GCA) %>%
    summarise(
      `Number of Item_itemdata` = n()
    )

  a0 <- left_join(rating, item_data, by = "GCA")

  a1 <- a0$`Number of Item_rating`
  a2 <- a0$`Number of Item_itemdata`
  a3 <- which((a1 - a2) != 0)
  a4 <- paste0(a0$GCA[a3], collapse = ", ")

  if(length(a3) != 0) {
    val_obj$ratiing_validated <- glue::glue("WARNING: Inconsistent length of items in {a4}")
  }

  # for item data ---------------------------------------
  if(sum(duplicated(imprt_data[["item_data"]]$Item_ID)) != 0) {

    val_obj$item_data_validated <- "Item IDs are not unique; check duplicated IDs."
  }

  # for examinee data -----------------------------------


  # Validation --------------------------------------
  if(length(val_obj) == 0) {
    val_obj$overall_validated <- "Data input is validated"
  }
  # validation text-------------------------
  val_text <-""
  for(i in names(val_obj)) {
    # val_text <- glue("{val_text}\n{val_obj[[i]]}")
    val_text <- paste(val_text, val_obj[[i]], sep = "\n")
  }


  # return-----------------------------------
  return(val_text)
}
