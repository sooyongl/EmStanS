#' @include 2_tab4_function.r
NULL

#' Manually update individual ESS results
#'
update_tab1 <- function(tab0, tab1, information, manual_cs) {

  est_cutscore <- tab0$est_cs

  tab1$modal_est_cutscore_all <-
    estCutScore_mode_manual(est_cutscore, information, manual_cs)

  tab1$modal_est_cs_all <- map(tab1$modal_est_cutscore_all, ~ .x$est_cs)
  tab1$modal_est_cp_all <- map(tab1$modal_est_cutscore_all, ~ .x$est_cp)
  tab1$modal_selected_cp_all <- map(tab1$modal_est_cutscore_all, ~ .x$selected_CP)
  tab1$modal_selected_cs_all <- map(tab1$modal_est_cutscore_all, ~ .x$selected_CS)
  tab1$modal_selected_weights_all <- map(tab1$modal_est_cutscore_all, ~ .x$selected_weights)

  # modal----------------------------------------------
  tab1$modal_res_all <-
    pmap(list(tab1$modal_est_cs_all, tab1$modal_selected_cp_all,tab1$modal_selected_cs_all, tab1$modal_selected_weights_all),
         ~ tab1_group_out_all(..1, ..2, ..3, ..4, information$base_data$WESS, modal = T)) %>%
    bind_rows() %>%
    select(-OOD) %>%
    mutate(Table = "All", .after = GCA)

  tab1 <- gen_modal_table(tab1)

  return(tab1)
}

# Helper functions ---------------------------------------

#' Estimate modal ESS cut scores based on user-given cut scores.
#'
estCutScore_mode_manual <- function(data, information, manual_cutscore) {
  # data = est_cutscore; manual_cutscore = manual_cs

  gcaid <- information$data_ready$id_list$GCA
  cond <- crossing(gcaid)
  inploc <- information$base_data$loc_nm

  lvnm <- information$data_ready$level_nm
  locReady <- information$data_ready$location_ready
  WESS <- information$base_data$WESS

  mod_data_1 <- data %>% bind_rows()

  modal_ALD <-
    foreach(i = 1:nrow(cond)) %do% {
      # i <- 1
      ext_ALD <-
        mod_data_1 %>%
        filter(GCA == cond[i,1] %>% pull()) %>%
        # group_split(User) %>%
        # group_split(vars(matches("Panelist|User"))) %>%
        group_by_at(vars(matches("Panelist|User"))) %>%
        group_split() %>%
        map(., ~ .x$ALD) %>%
        set_names(., nm = 1:length(.)) %>%
        bind_cols()

      ext_cors <-
        mod_data_1 %>%
        filter(GCA == cond[i,1] %>% pull()) %>%
        distinct(vars(matches("Panelist|User")), Correlation) %>% pull(Correlation)

      tabl_ALD <- apply(ext_ALD, 1, get_mode, ext_cors)
    }

  mod_data_2 <-
    mod_data_1 %>%
    distinct(GCA, Round, Item_ID, OOD, !!as.name(inploc)) %>%
    mutate(ALD = unlist(modal_ALD))

  # final result ----------------------------------------
  split_filter <-
    mod_data_2 %>%
    group_split(GCA) %>%
    map(., ~ .x %>% select(-OOD, -all_of(inploc)))

  mode_cs <-
    map2(split_filter, manual_cutscore, estCutScore_manual,
         information
    )
  return(mode_cs)
}

#' Estimate ESS cut scores based on user-given cut scores
#'
estCutScore_manual <- function(inp_data, manual_cutscore, information) {
  # inp_data = split_filter[[1]];
  # manual_cutscore = c(300, 310)
  GCA_data <- inp_data

  need_data <- data_prep(GCA_data, information)

  data_1   = need_data$data_1
  level_nm = need_data$level_nm
  test_id  = need_data$test_id
  location = need_data$location
  bind_loc = need_data$bind_loc

  locnm <- names(location)[3]
  ald_vector <- remove_blank_vector(data_1 %>% pull(ALD) )
  lv_vector <- remove_blank_vector(level_nm)

  SD_inp <-
    information$data_ready$SD_data %>%
    filter(GCAid == test_id) %>%
    pull(SD)

  cut_scores <- runESS(inp_data, location, length(lv_vector))

  cut_scores <-
    cut_scores %>%
    mutate_at(vars(matches("_W$")), ~ .x /SD_inp) %>%
    mutate_all(round, 2)

  cut_point    <- cal_minp(cut_scores)
  gam_est <- splineFit(cut_scores %>% mutate(!!locnm := location[[locnm]]))

  gam_res <- extractgam(gam_est)
  cut_point   <- append(cut_point, gam_res$scp)

  selected_CS <- manual_cutscore
  selected_CP <- sapply(selected_CS, function(x) { # x = selected_CS[1]
    a1 <- x - bind_loc[[2]]
    a2 <- which(a1 == max(a1[a1 <= 0]))[1]
  })

  selected_weights <- select_weight(selected_CP, gam_res,cut_scores, information$base_data$gamest)

  data_2 <-
    left_join(data_1, bind_loc, by = "Item_ID") %>%
    bind_cols(., cut_scores) %>%
    relocate(., OOD, !!as.name(locnm), .after = Item_ID)

  cor_inc <- getCor(inp_data, data_2, selected_CP, lv_vector, locnm, ald_vector)

  data_3 <-
    data_2 %>%
    mutate(
      Operational_Lv = cor_inc[[2]],
      Correlation = cor_inc[[1]]
    )

  o <- list(est_cs = data_3,
            est_cp = cut_point,
            selected_CP = selected_CP,
            selected_CS = selected_CS,
            selected_weights = selected_weights)


  return(o)
}


#' update_tab2
update_tab2 <- function(tab1, information) {
  gen_tab2(tab1, information)
}
