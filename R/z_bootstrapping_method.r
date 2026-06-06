#' @export
boostrapping <- function(output, ...) {
  UseMethod("boostrapping")
}

boostrapping.ESS <- function(output, gca_name, boot_num = 10) {
  # output <- res; gca_name = "M1-All";boot_num = 10
  if(length(gca_name) > 1) {
    stop("only one `gca_name` must be selected.")
  }

  selected_id <- gca_name
  information <- output$information

  ald <- information$base_data$target_nm
  loc_nm <- information$base_data$loc_nm
  WESS <- information$base_data$WESS
  levels <- if(WESS) { "_W$" }else { "_C$" }

  bootResults <- list()

  gca <- unique(information$base_data$filtered_data[["GCA"]]) %>%
    sort() %>% as.character()
  level_nm <- information$data_ready$level_nm

  gca_p <- which(gca == selected_id)

  n_level <- length(level_nm[,gca_p])
  selected_cp <- output$tab1$modal_selected_cp_all[[gca_p]]
  used_data <- output$tab2$for_tab2_out[[gca_p]][[1]]$t_out

  new_data <-
    used_data %>%
    select(all_of(loc_nm), all_of(ald)) %>%
    mutate(!!ald := as.numeric(str_remove(!!as.name(ald), "Level")))

  names(new_data)[1] <- "location"

  boot_res <-
    boot_ESS(
      new_data = new_data,
      WESS = WESS,
      n_rep = boot_num,
      b_prop = 1,
      replace = T,
      keep.sample = F,
      empirical = F,
      n_level = n_level)

  bootResults$boot_p <-
    boot_plot(boot_res = boot_res,
              est_data = used_data,
              selected_cp = selected_cp,
              information = information,
              font_size = 18)

  bootResults$table1 <-
    boot_res %>%
    select(-y) %>%
    spread("lv_nm","x") %>%
    select(-boot_rep)

  bootResults$table2 <-
    boot_res %>%
    select(-y) %>%
    spread("lv_nm","x") %>%
    select(-boot_rep) %>%
    psych::describe() %>%
    select(-vars, -trimmed, -mad) %>%
    mutate_if(is.numeric, round, 2)


  bootResults$selected_id <- selected_id

  output$bootResults <- bootResults

  output <- structure(
    output,
    class = "bootESS"
  )


  invisible(output)

}
