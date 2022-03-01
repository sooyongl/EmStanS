#' Conduct Embeded Standard Setting
#'
#' @examples
#' a1 <- emstans(data, lvname = c("Level1", "Level2", "Level3"))
#' @export
emstans <- function(data, lvname = NULL, WESS = T, GAM = T,
                    SD = 1, EC = 0, digits= 3) {

  message("Note. Input data must be ordered by OOD, location, and ALD")

  names(data) <- c("OOD","location","ALD")

  if(is.null(lvname)) {
    if(!is.numeric(data$ALD)){
      stop("Levels must be specified! ex. Level1, Level2, Level3\nor, must be a numeric vector. ex. 1,2,3")
    } else {
      data$ALD <- paste0("Level", data$ALD)
    }
  }

  cut_scores <- calCountWeight(data)

  cut_scores <-
    cut_scores %>%
    mutate_at(vars(matches("_W$")), ~ .x /SD) %>%
    mutate_all(round, digits)

  cut_point  <- cal_minp(cut_scores)

  gam_est <- splineFit(cut_scores %>% mutate(location = data$location))
  gam_res <- extractgam(gam_est)

  cut_point <- append(cut_point, gam_res[["scp"]])

  selected_CP <- select_cp(cut_point, cut_scores, WESS,GAM)
  selected_CS <- select_cs(selected_CP, gam_res, new_data$location, WESS, GAM)

  selected_weights <- select_weight(selected_CP, gam_res, cut_scores, GAM)

  data_2 <- data %>% bind_cols(., cut_scores)

  cor_inc <- getCor(data_2, selected_CP, lvname)

  data2 <-
    data_2 %>%
    mutate(Operational_Lv = cor_inc[[2]])

  cutpoint_inp <- selected_CP
  dataUse_1 <- data2

  ess_table <- dataUse_1 %>%
    select(OOD, location, ALD, Operational_Lv,
           ends_with("_C"),ends_with("_W")) %>%
    rename("Aligned_Lvl" = "ALD")

  # dataUse_Weight <-
  #   dataUse_1 %>%
  #   mutate(
  #     weightSum = dplyr::select(., ends_with("_W")) %>% rowSums()
  #   ) %>% pull(weightSum)

  target_names <- dataUse_1$ALD
  lv_name <- target_names %>% unique() %>% sort()
  #
  given_n   <- c(1:length(lv_name))
  length_n  <- ifelse(length(given_n) == 1, 2, length(given_n))
  targets_n <- match(target_names, lv_name)

  er <- length(target_names)
  review_table <-
    foreach(n = 2:length_n, .combine = 'rbind') %do% {
      # n = 2
      cr <- cutpoint_inp[(n-1)]

      cr_abo <- cr
      cr_bel <- cr

      c_bel <- which(targets_n[0:(cr - 1)] >= n)
      c_abo <- (which(targets_n[cr_abo:er] < n) - 1) + cr_abo

      a1 <- dataUse_1[c(c_bel, c_abo), ] %>%
        mutate(item_status = "inc")
      a2 <- dataUse_1[-c(c_bel, c_abo), ] %>%
        mutate(item_status = "cons")

      a3 <- bind_rows(a2, a1) %>% arrange(OOD)
      a3
    }

  # review_table <-
  #   review_table  %>%
  #   mutate(.,
  #          Weight = dplyr::select(., ends_with("_W")) %>% rowSums()
  #   )


  cs_inf <- data2 %>%
    slice(selected_CP) %>%
    select(OOD, ALD, location) %>%
    mutate(ALD = lvname[-1]) %>%
    rename(
      "Cut_Score" = "location"
    ) %>% select(-OOD)

  cs_inf_upper <-
    cs_inf %>%
    mutate(
      ALD = extract_num(ALD),
      ALD = paste0("Level", (ALD - 1))
    ) %>%
    rename(
      "Cut_Score_upper" = "Cut_Score"
    )


  review_table <- review_table %>%
    left_join(., cs_inf, by = c("ALD" = "ALD")) %>%
    left_join(., cs_inf_upper, by = c("ALD" = "ALD")) %>%
    mutate(AN = extract_num(ALD),
           ON = extract_num(Operational_Lv),
           Diff_LV = AN - ON,
           Distance = case_when(
             AN > ON ~ location - Cut_Score,
             AN < ON ~ location - (Cut_Score_upper - 1),
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
    rename(
      "Aligned_Lvl" = "ALD",
      "Operational_Lvl" = "Operational_Lv",
      "Lvl_Diff" = "Diff_LV"
    )


  o <- list(ess_table = ess_table, review_table = review_table)
  class(o) <- append("ess", class(o))
  # o <- structure(o, class = "ess")

  o
}
