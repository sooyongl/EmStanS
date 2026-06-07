#' @include 2_tab3_function.r
NULL

#' Item review table
#'
#' @param tab1 a list of data coming from \code{\link{gen_tab1}}
#' @param tab2 a list of data coming from \code{\link{gen_tab2}}
#' @param information a list of data coming from \code{\link{get_data_info}}
#' @return a list containing item review tables
gen_tab4 <- function(tab1, tab2, information){

  tab4 <- list()
  domain <- information$base_data$domain
  eval_domain_len <- length(domain) == 2

  n.of.gca <- information$data_ready$id_list$GCA
  loc_nm <- information$base_data$loc_nm

  for_tab2_out <- tab2$for_tab2_out

  level_names <- information$data_ready$level_nm[[1]]
  level_names <-  level_names[2:length(level_names)]

  SD <- information$data_ready$SD_data
  EC <- information$data_ready$EC_data

  cs_inf <-
    tab1$modal_res_all %>%
    select(GCA, ends_with("_loc")) %>%
    set_names(., nm = c("GCA", level_names)) %>%
    gather(., "ALD", "Cut_Score", -GCA)

  item_review_table <- foreach(vi = 1:length(n.of.gca), .combine = 'rbind') %do% {
    # vi = 1
    cutpoint_inp <- tab1$modal_selected_cp_all[[vi]]
    dataUse_1 <- for_tab2_out[[vi]][[1]][["t_out"]]

    dataUse_Weight <-
      dataUse_1 %>%
      mutate(
        weightSum = dplyr::select(., ends_with("_W")) %>% rowSums()
      ) %>% pull(weightSum)

    target_names <- dataUse_1$ALD

    lv_name <- target_names %>% unique() %>% sort()
    #
    given_n <- c(1:length(lv_name))
    length_n <- ifelse(length(given_n) == 1, 2, length(given_n))
    targets_n <- match(target_names, lv_name)

    er <- length(target_names)
    review_table <-
      foreach(n = 2:length_n, .combine = 'rbind') %do% {
        # n = 2
        cr <- cutpoint_inp[(n-1)]
        # cr = 3
        cr_abo <- cr
        cr_bel <- cr

        c_bel <- which(targets_n[0:(cr - 1)] >= n)
        c_abo <- (which(targets_n[cr_abo:er] < n) - 1) + cr_abo

        a1 <- dataUse_1[c(c_bel, c_abo), ] %>% mutate(item_status = "inc")
        a2 <- dataUse_1[-c(c_bel, c_abo), ] %>% mutate(item_status = "cons")

        a3 <- bind_rows(a2, a1)
        a3
      }

    all_names <- names(review_table)
    review_table <- review_table %>% distinct(!!!syms(all_names))
    review_table <-
      review_table %>%
      select(-Round) %>%
      mutate(.,
             Weight = dplyr::select(., ends_with("_W")) %>% rowSums()
      ) %>%
      mutate_if(is.numeric, round, 4)
  }

  cs_inf_upper <-
    cs_inf %>%
    mutate(
      ALD = extract_num(ALD),
      ALD = paste0("Level", (ALD - 1))
    ) %>%
    rename(
      "Cut_Score_upper" = "Cut_Score"
    )

  item_review_table <-
    item_review_table %>%
    left_join(., cs_inf, by = c("GCA" = "GCA",
                                "ALD" = "ALD")) %>%
    left_join(., cs_inf_upper, by = c("GCA" = "GCA",
                                      "ALD" = "ALD")) %>%
    left_join(., SD, by = c("GCA" = "GCAid")) %>%
    left_join(., EC, by = c("GCA" = "GCAid")) %>%

    mutate(AN = extract_num(ALD),
           ON = extract_num(Operational_Lv),
           Diff_LV = AN - ON,
           Distance = case_when(
             AN > ON ~ !!as.name(loc_nm) - Cut_Score,
             AN < ON ~ !!as.name(loc_nm) - (Cut_Score_upper - 1),
             AN == ON ~ 0
           ),
           `Std. Abs. Distance` = abs(round(Distance / SD, 3)),
           item_status = case_when(
             abs(Distance) <= EC & abs(Distance) > 0 ~ "Essentially Consistent",
             abs(Distance) > EC ~ "Inconsistent",
             abs(Distance) == 0 ~ "Consistent"
           ),
           item_status = factor(item_status)

    ) %>%

    select(-matches("L[[:digit:]]+"), -AN, -ON) %>%
    select(
      GCA, Item_ID, OOD,
      ALD, Operational_Lv, Diff_LV,
      starts_with("Loc"),
      Distance,`Std. Abs. Distance`,item_status) %>%
    rename(
      "Aligned_Lvl" = "ALD",
      "Operational_Lvl" = "Operational_Lv",
      "Lvl_Diff" = "Diff_LV"
    ) %>%
    arrange(
      GCA, Item_ID
    ) %>%
    mutate_if(is.numeric, round,4)

  item_review_table <-
    item_review_table %>%
    mutate(GCA = factor(GCA, levels = n.of.gca)) %>%
    mutate_if(is.numeric, round,4) %>%
    arrange(GCA, OOD)


  item_review_table <- item_review_table %>%
    group_by(GCA, Item_ID) %>%
    filter(row_number(OOD) == 1) %>%
    ungroup() %>%
    mutate_if(is.numeric, round,4)
  tab4$for_tab4_out <- item_review_table

  return(tab4)
}

# Helper functions --------------------------------------
#' extract_num
extract_num <- function(vectorInp){
  as.numeric(str_extract(vectorInp, "[[:digit:]]"))
}
