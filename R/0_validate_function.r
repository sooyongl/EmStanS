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
      `Number of Item_rating` = n(),
      .groups = "drop"
    )

  item_data <- imprt_data[["item_data"]] %>%
    group_by(GCA) %>%
    summarise(
      `Number of Item_itemdata` = n(),
      .groups = "drop"
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

#' Stop with a clear message when prepared input cannot be analysed
#'
#' Runs on the output of [data_ready()], before any estimation. Checks the
#' conditions the estimation relies on, so problems surface as one readable
#' error instead of a failure deep inside the pipeline.
#'
#' @param data_list output of [data_ready()].
#' @param loc name of the item location column.
#' @return `data_list`, invisibly, if all checks pass.
#' @keywords internal
check_inputs <- function(data_list, loc) {
  items   <- data_list$item_data
  ratings <- data_list$rating_data
  setup   <- data_list$setup_data

  show <- function(x, n = 10) {
    x <- unique(as.character(x))
    paste0(paste(utils::head(x, n), collapse = ", "),
           if (length(x) > n) paste0(", ... (", length(x), " in total)") else "")
  }

  # 1. Location column exists, is numeric, and is complete.
  if (!loc %in% names(items)) {
    num_cols <- names(items)[vapply(items, is.numeric, logical(1))]
    stop("Location column `", loc, "` is not in the item data. ",
         "Numeric columns available: ", show(num_cols, 20), ".", call. = FALSE)
  }
  if (!is.numeric(items[[loc]])) {
    stop("Location column `", loc, "` must be numeric.", call. = FALSE)
  }
  if (anyNA(items[[loc]])) {
    stop("Location column `", loc, "` is missing for item(s): ",
         show(items$Item_ID[is.na(items[[loc]])]), ".", call. = FALSE)
  }

  # 2. Item IDs are unique (ratings are joined to items by Item_ID alone).
  dup <- items$Item_ID[duplicated(items$Item_ID)]
  if (length(dup) > 0) {
    stop("Item IDs must be unique in the item data. Duplicated: ",
         show(dup), ".", call. = FALSE)
  }

  # 3. SD and EC, when given, are numbers.
  for (col in intersect(c("SD", "EC"), names(setup))) {
    bad <- !is.na(setup[[col]]) & is.na(suppressWarnings(as.numeric(setup[[col]])))
    if (any(bad)) {
      stop("`", col, "` in the setup data is not a number for grade(s): ",
           show(setup$GCA[bad]), ".", call. = FALSE)
    }
  }

  # 4. Within each grade, every panelist rated exactly the grade's items,
  #    once each. Several steps pair rows across panelists by position.
  panel_col <- names(ratings)[grepl("User|Panel", names(ratings))][1]
  for (g in unique(as.character(ratings$GCA))) {
    grade_items <- as.character(items$Item_ID[as.character(items$GCA) == g])
    r <- ratings[as.character(ratings$GCA) == g, , drop = FALSE]
    by_panelist <- split(as.character(r$Item_ID), as.character(r[[panel_col]]))
    for (pn in names(by_panelist)) {
      ids <- by_panelist[[pn]]
      missing <- setdiff(grade_items, ids)
      extra   <- setdiff(ids, grade_items)
      twice   <- ids[duplicated(ids)]
      if (length(missing) + length(extra) + length(twice) > 0) {
        stop("Ratings for panelist `", pn, "` in grade `", g, "` do not match the item data",
             if (length(missing)) paste0("; not rated: ", show(missing)),
             if (length(extra))   paste0("; not in item data: ", show(extra)),
             if (length(twice))   paste0("; rated more than once: ", show(twice)),
             ". Every panelist must rate every item in the grade exactly once.",
             call. = FALSE)
      }
    }
  }

  invisible(data_list)
}
