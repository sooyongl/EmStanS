#' print_setting
#'
#' The function provides the table for setup data when the data is imported.
#' @param imprt_data a list of data from \code{\link{read_data}}
#' @return kable output
#'
print_setting<- function(imprt_data) {

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

#' print_itemtable1
#'
#' The function provides the table for the number of items in the rating data when the data is imported.
#' @param imprt_data a list of data from \code{\link{read_data}}
#' @return kable output
#'
print_itemtable1 <- function(imprt_data) {
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


#' print_itemtable2
#'
#' The function provides the table for the number of items in the item meta data when the data is imported.
#' @param imprt_data a list of data from \code{\link{read_data}}
#' @return kable output
#'
print_itemtable2 <- function(imprt_data) {

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
