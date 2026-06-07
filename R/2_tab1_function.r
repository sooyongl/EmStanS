#' @include 2_tab0_function.r
NULL

#' ESS analysis for overall GCA
#'
#' @param tab0 a list of data coming from \code{\link{gen_tab0}}
#' @param information a list of data coming from \code{\link{get_data_info}}
#' @return a list of data for each GCA aggregating all the panelists and the tables about ESS results.
gen_tab1 <- function(tab0, information){
  #
  tab1 <- list()
  # tab1 results ------------------------------------------
  tab1 <- tab1_res(tab0, tab1, information)

  # individuals -------------------------------------------
  tab1 <- gen_indi_table(tab1 = tab1, tab1_out = tab1$res, list_name = "indi_table")

  # median -----------------------------------------------
  tab1 <- gen_median_table(tab1)

  # average -----------------------------------------------
  tab1 <- gen_average_table(tab1)

  # modal ------------------------------------------------
  tab1 <- tab1_modal_res(tab0, tab1, information)

  tab1 <- gen_modal_table(tab1)

  # res : individual result
  # median: median_res_com
  # mode: modal_res_com
  # average: average_res_com

  return(tab1)
}

# Helper functions -------------------------------------
#' tab1_res output
tab1_res <- function(tab0, tab1, information) {

  tab1$res <-
    pmap(list(tab0$est_cs, tab0$selected_CP, tab0$selected_CS, tab0$selected_weights),
         ~ tab1_group_out(..1, ..2, ..3, ..4, information$base_data$WESS)) %>%
    bind_rows() %>%
    select(-OOD) %>%
    mutate_if(is.numeric, round, 4) %>%
    factor_arrange(information)

  tab1
}

#' tab1_group_out
tab1_group_out <- function(inp_data, selectedCp, selectedCs, selectedWeights,WESS, modal = F){
  # inp_data = tab0$est_cs[[1]]; selectedCp = tab0$selected_CP[[1]]; selectedCs = tab0$selected_CS[[1]]; WESS = information$base_data$WESS; modal = F; selectedWeights = tab0$selected_weights[[1]]

  inp_names <- names(inp_data)

  num_item <- nrow(inp_data)
  total_item <- num_item*length(selectedCp)

  ln_0 <- inp_names[get_which(inp_names,"ALD",1):get_which(inp_names, "Operational_Lv",-1)]

  ln_1 <- ln_0[!str_detect(ln_0, "_W")]
  ln_2 <- ln_0[str_detect(ln_0, "_W")]
  target_loc <-  inp_names[get_which(inp_names, "ALD",-1)]

  cor_inc <- inp_data %>% pull(Correlation) %>% unique()
  if (modal == F){
    inp_data <-
      inp_data %>%
      slice(selectedCp) %>%
      select(GCA, Table, matches("Panelist|User"), OOD, Item_ID,
             all_of(target_loc), all_of(ln_0))
  } else {
    inp_data <-
      inp_data %>%
      slice(selectedCp) %>%
      select(GCA, Table, OOD, Item_ID,
             all_of(target_loc), all_of(ln_0))
  }

  start_level <- get_which(names(inp_data), target_loc, 1)

  d1 <- inp_data %>% mutate(!!target_loc := selectedCs)
  res <-
    foreach(ti = 1:nrow(d1), .combine = 'cbind') %do% {
      # ti <- 1
      ll <- start_level + 1*(ti-1)
      ww <- (start_level+nrow(d1)) + 1*(ti-1)

      lv_nm <- str_remove(ln_1[ti], "_C")

      d2 <-
        d1[ti, ] %>%
        select(OOD, all_of(target_loc), all_of(ll), all_of(ww))
      names(d2)[1] <- paste0(lv_nm,"_p")
      names(d2)[2] <- paste0(lv_nm,"_loc")
      return(d2)
    } %>%
    mutate(!!paste0("L","_sum") := eval(parse(text = paste(ln_1, collapse = "+"))),
           !!paste0("LW","_sum") := eval(parse(text = paste(ln_2, collapse = "+")))
    ) %>%
    select(ends_with("_p"), ends_with("_loc"), everything()) %>%
    bind_cols(d1[1,c(1:(4-modal))], .) %>%
    mutate(num_item = num_item, total_item = total_item) %>%
    mutate(Correlation = round(cor_inc,3), .before = OOD)


  w_pos <- which(str_detect(names(res), "_W"))

  for(i in 1:length(selectedWeights)) {
    res[[w_pos[i]]] <- selectedWeights[i]
  }

  res[["LW_sum"]] <- sum(selectedWeights)

  return(res)
}

#' tab1_group_out_all
tab1_group_out_all <-
  function(inp_data, selectedCp, selectedCs, selectedWeights, WESS, modal = F){
    # inp_data = tab1$modal_est_cs_all[[1]]; selectedCp = tab1$modal_selected_cp_all[[1]]; selectedCs = tab1$modal_selected_cs_all[[1]];WESS = information$base_data$WESS; modal = T

    inp_data <- inp_data %>% mutate_if(is.numeric, round, 2)

    num_item <- nrow(inp_data)
    total_item <- num_item*length(selectedCp)

    ln_0 <-
      names(inp_data)[get_which(names(inp_data), "ALD", 1):get_which(names(inp_data), "Operational_Lv", -1)]
    ln_1 <- ln_0[!str_detect(ln_0, "_W")]
    ln_2 <- ln_0[str_detect(ln_0, "_W")]
    target_loc <-  names(inp_data)[get_which(names(inp_data), "ALD", -1)]

    cor_inc <- inp_data %>% pull(Correlation) %>% unique()

    inp_data <-
      inp_data %>%
      slice(selectedCp) %>%
      select(GCA, -Round, OOD, Item_ID,
             all_of(target_loc), all_of(ln_0))

    start_level <- get_which(names(inp_data), target_loc, 1)

    d1 <- inp_data %>% mutate(!!target_loc := selectedCs)
    res <-
      foreach(ti = 1:nrow(d1), .combine = 'cbind') %do% {
        # ti <- 1
        ll <- start_level + 1*(ti-1)
        ww <- (start_level+nrow(d1)) + 1*(ti-1)

        d2 <-
          d1[ti, ] %>%
          select(OOD, all_of(target_loc), all_of(ll), all_of(ww))

        lv_nm <- str_remove(ln_1[ti], "_C")

        names(d2)[1] <- paste0(lv_nm,"_p")
        names(d2)[2] <- paste0(lv_nm,"_loc")
        return(d2)
      } %>%
      mutate(!!paste0("L","_sum") := eval(parse(text = paste(ln_1, collapse = "+"))),
             !!paste0("LW","_sum") := eval(parse(text = paste(ln_2, collapse = "+")))
      ) %>%
      select(ends_with("_p"), ends_with("_loc"), everything()) %>%
      bind_cols(d1[1,c(1:2)], .) %>%
      mutate(num_item = num_item, total_item = total_item) %>%
      mutate(Correlation = cor_inc, .before = OOD)


    w_pos <- which(str_detect(names(res), "_W"))

    for(i in 1:length(selectedWeights)) {
      res[[w_pos[i]]] <- selectedWeights[i]
    }

    res[["LW_sum"]] <- sum(selectedWeights)

    return(res)
  }

#' gen_indi_table
gen_indi_table <- function(tab1, tab1_out, list_name = "indi_table") {
  # tab1_out <- tab1$res; list_name = "modal_table"
  tab1_name <- names(tab1_out)

  page_name <- tab1_name[get_which(tab1_name, "_p")]
  weight_name <- tab1_name[get_which(tab1_name, "_W")]

  default_name <- tab1_name[get_which(tab1_name, "_C")]

  cors_p <- get_which(tab1_name, "Correlation")
  panel.key <- tab1_out[, 1:(cors_p)]
  table.inf <- tab1_out[, -c(seq_len(ncol(panel.key)))]
  table.inf <- table.inf %>% mutate_if(is.numeric, round, 4)

  table.keep <- table.inf
  level_names <- names(table.inf)

  for(mi in 1:length(page_name)) {
    # mi <- 1
    mii <- mi + length(page_name)

    mut.inp <- glue::glue(
      'paste0({level_names[{mi}]},"\n(",{level_names[{mii}]},")")'
    )

    table.inf <- table.inf %>% mutate(!!page_name[mi] := eval(parse(text = mut.inp)))
  }

  table.inf <- table.inf %>% select(-ends_with("_loc"))

  for(mi in 1:length(default_name)) {
    # mi <- 2
    mii <- mi + length(default_name)

    mut.inp <- glue::glue(
      'paste0({default_name[{mi}]}," / ",num_item)'
    )

    table.inf <- table.inf %>% mutate(!!default_name[mi] := eval(parse(text = mut.inp)))
  }
  table.inf <-
    table.inf %>%
    mutate(L_sum = paste0(L_sum, " / ", total_item)) %>%
    select(-num_item, -total_item)

  tbl_res <- bind_cols(panel.key, table.inf) %>%
    mutate(Table = as.character(Table),
           Table = if_else(Table == "0", "All", Table))


  tbl_res <- list(tbl_res)
  names(tbl_res) <- list_name

  if(any(names(tab1) == list_name)) {
    tab1[[list_name]] <- tbl_res[[list_name]]
  } else {
    tab1 <- append(tab1, tbl_res)
  }

  return(tab1)
}

#' gen_median_table
gen_median_table <- function(tab1) {

  # For Median Table ready
  tab1$median_res <- ready_median_table(tab1$res, type = "table")
  tab1$median_res_all <- ready_median_table(tab1$res, type = "all")
  tab1$median_res_com <-
    bind_rows(tab1$median_res,tab1$median_res_all) %>% arrange(GCA)

  # For Median Table output ready
  tab1$median_table <- ready_median_output(tab1$median_res_com)

  tab1
}

#' gen_average_table
gen_average_table <- function(tab1) {

  # For average Table ready
  tab1$average_res <- ready_average_table(tab1$res, type = "table")
  tab1$average_res_all <- ready_average_table(tab1$res, type = "all")
  tab1$average_res_com <-
    bind_rows(tab1$average_res,tab1$average_res_all) %>% arrange(GCA)

  # For average Table output ready
  tab1$average_table <- ready_median_output(tab1$average_res_com)

  tab1

}


#' ready_median_table
ready_median_table <- function(tab1_res, type = "all"){
  # tab1_res <- tab1$res; type = 'table'
  if(type == "all") {
    tt1 <- tab1_res %>% group_split(GCA)
  } else {
    tt1 <- tab1_res %>% group_split(GCA, Table)
  }

  tab1_res_name <- names(tab1_res)
  page_name <- tab1_res_name[get_which(tab1_res_name, "_p")]

  get_med <- function(given){ # given = for_given

    given <- sort(given)

    length_given <- length(given)
    middle <- length_given/2
    middle_point <- ceiling(middle)

    if(length_given %% 2 != 0){
      given_med <- given[middle_point]
    } else {
      given_med <- given[c(middle_point,(middle_point+1))]
    }
    given_med <- unique(given_med)
    return(given_med)
  }

  med_cutscore <-
    foreach(mei = 1:length(tt1)) %do% {
      # mei = 1; p_i = 1
      dataUse <- tt1[[mei]]
      num_level <- length(page_name)
      loc_nm <- names(dataUse)[get_which(names(dataUse), "_loc")]


      foreach(p_i = 1:num_level, .combine = 'cbind') %do% {
        pg_start <- page_name[p_i]
        loc_start <- loc_nm[p_i]



        # cols_use <- c(pg_start, loc_start)
        # message("p_i = ", p_i)
        # message("cols_use = ", paste(cols_use, collapse = ", "))
        # message("is.na(cols_use) = ", paste(is.na(cols_use), collapse = ", "))
        # message("cols in dataUse = ", paste(cols_use %in% names(dataUse), collapse = ", "))
        #
        # if (anyNA(cols_use)) {
        #   stop("Missing column name in cols_use: ", paste(cols_use, collapse = ", "))
        # }
        #
        # if (!all(cols_use %in% names(dataUse))) {
        #   stop(
        #     "Some selected columns are not in dataUse.\n",
        #     "cols_use: ", paste(cols_use, collapse = ", "), "\n",
        #     "names(dataUse): ", paste(names(dataUse), collapse = ", ")
        #   )
        # }


        for_given <- tt1[[mei]][[page_name[p_i]]]

        medp <- get_med(for_given)

        mp <- c()
        for(temi in 1:length(medp)) {
          # temi = 2
          med_point <- which(tt1[[mei]][[page_name[p_i]]] %in% medp[temi] )
          mp[temi] <- med_point[1]
        }

        dataUse %>%
          select(all_of(pg_start), all_of(loc_start)) %>%
          slice(mp) %>%
          summarise_all( mean)
      }
    }

  odd_num <- seq(1, length(c(page_name,loc_nm)), 2)
  even_num <-seq(2, length(c(page_name,loc_nm)), 2)

  temp_name <- 1:length(c(page_name,loc_nm))
  temp_name[odd_num] <- page_name
  temp_name[even_num] <- loc_nm

  med_cutscore <-
    lapply(med_cutscore, unname) %>%
    lapply(., function(x) {names(x) <- temp_name; x}) %>%
    bind_rows(.)

  if(type == "all") {
    median_table <-
      tab1_res %>%
      distinct(GCA) %>%
      mutate(Table = "All") %>%
      bind_cols(.,med_cutscore) %>%
      mutate(Table = as.character(Table))
  } else {
    median_table <-
      tab1_res %>%
      distinct(GCA, Table) %>%
      bind_cols(.,med_cutscore) %>%
      mutate(Table = as.character(Table))
  }

  median_table <- median_table %>%
    # mutate_at(vars(page_name), ceiling)
    mutate(across(all_of(page_name), ceiling))

  return(median_table)
}

#' ready_average_table
ready_average_table <- function(tab1_res, type = "all"){
  # tab1_res <- tab1$res; type = 'table'
  if(type == "all") {
    tt1 <- tab1_res %>% group_split(GCA)
  } else {
    tt1 <- tab1_res %>% group_split(GCA, Table)
  }

  tab1_res_name <- names(tab1_res)
  page_name <- tab1_res_name[get_which(tab1_res_name, "_p")]

  avg_cutscore <-
    foreach(mei = 1:length(tt1)) %do% {
      # mei = 1; p_i = 1
      dataUse <- tt1[[mei]]
      num_level <- length(page_name)
      loc_nm <- names(dataUse)[get_which(names(dataUse), "_loc")]
      foreach(p_i = 1:num_level, .combine = 'cbind') %do% {
        pg_start <- page_name[p_i]
        loc_start <- loc_nm[p_i]

        for_given <- tt1[[mei]][[page_name[p_i]]]

        medp <- for_given
        mp <- c()
        for(temi in 1:length(medp)) {
          # temi = 2
          med_point <- which(tt1[[mei]][[page_name[p_i]]] %in% medp[temi] )
          mp[temi] <- med_point[1]
        }

        dataUse %>%
          select(all_of(pg_start), all_of(loc_start)) %>%
          slice(mp) %>%
          summarise_all( mean ,
                         .groups = "drop")
      }
    }

  odd_num <- seq(1, length(c(page_name,loc_nm)), 2)
  even_num <-seq(2, length(c(page_name,loc_nm)), 2)

  temp_name <- 1:length(c(page_name,loc_nm))
  temp_name[odd_num] <- page_name
  temp_name[even_num] <- loc_nm

  avg_cutscore <-
    lapply(avg_cutscore, unname) %>%
    lapply(., function(x) {names(x) <- temp_name; x})# %>%
    # bind_rows(.) %>%
    # mutate_all(round, 2)

  loc_nm <- tab1_res_name[str_detect(tab1_res_name, "_loc")]
  tab1_temp <- map(tt1, ~ .x %>% select(all_of(page_name), all_of(loc_nm)))

  close_page <- vector("list", length(page_name))
  for(nn in 1:length(loc_nm)) { # nn = 1
    i = loc_nm[nn]
    j <- str_replace(i, "_loc", "_p")

    temp1 <- map(tab1_temp, ~ .x %>% pull(all_of(i)))
    temp2 <- map(avg_cutscore, ~ .x %>% pull(all_of(i)))

    close_pos <- map2(temp1, temp2, ~ abs(.x - .y)) %>%
      map(.,
          ~ which(.x == min(.x))[1]) %>%
      unlist()

    # temp3 <- abs(temp1 - temp2)
    # close_pos <- which(temp3 == min(temp3))[1]
    # close_page[j] <- tab1_temp %>% slice(close_pos) %>% pull(all_of(j))

    close_page[[nn]] <- map2(close_pos, tab1_temp,
        ~ .y %>% slice(.x) %>% pull(all_of(j))) %>%
      unlist()
  }

  avg_cutscore <- bind_rows(avg_cutscore)
  avg_cutscore[page_name] <- close_page

  avg_cutscore <- avg_cutscore %>% mutate_if(is.numeric, round, 2)

  if(type == "all") {
    avg_table <-
      tab1_res %>%
      distinct(GCA) %>%
      mutate(Table = "All") %>%
      bind_cols(.,avg_cutscore) %>%
      mutate(Table = as.character(Table))
  } else {
    avg_table <-
      tab1_res %>%
      distinct(GCA, Table) %>%
      bind_cols(.,avg_cutscore) %>%
      mutate(Table = as.character(Table))
  }

  return(avg_table)
}
#' gen_modal_table
tab1_modal_res <- function(tab0, tab1, information) {

  est_cutscore <- tab0$est_cs

  # modal ------------------------------------------------
  # modal Cut Score Estimate
  tab1$modal_est_cutscore <- estCutScore_mode(est_cutscore,information,"table")
  tab1$modal_est_cutscore_all <- estCutScore_mode(est_cutscore, information, "all")

  tab1$modal_est_cs <- map(tab1$modal_est_cutscore, ~ .x$est_cs)
  tab1$modal_est_cp <- map(tab1$modal_est_cutscore, ~ .x$est_cp)
  tab1$modal_selected_cp <- map(tab1$modal_est_cutscore, ~ .x$selected_CP)
  tab1$modal_selected_cs <- map(tab1$modal_est_cutscore, ~ .x$selected_CS)
  tab1$modal_selected_weights <- map(tab1$modal_est_cutscore, ~ .x$selected_weights)

  tab1$modal_est_cs_all <- map(tab1$modal_est_cutscore_all, ~ .x$est_cs)
  tab1$modal_est_cp_all <- map(tab1$modal_est_cutscore_all, ~ .x$est_cp)
  tab1$modal_selected_cp_all <-
    map(tab1$modal_est_cutscore_all, ~ .x$selected_CP)
  tab1$modal_selected_cs_all <-
    map(tab1$modal_est_cutscore_all, ~ .x$selected_CS)
  tab1$modal_selected_weights_all <-
    map(tab1$modal_est_cutscore_all, ~ .x$selected_weights)

  tab1$modal_res <-
    pmap(list(tab1$modal_est_cs, tab1$modal_selected_cp, tab1$modal_selected_cs, tab1$modal_selected_weights),
         ~ tab1_group_out(..1, ..2, ..3, ..4, information$base_data$WESS, modal = T)
    ) %>%
    bind_rows() %>%
    select(-OOD) %>%
    mutate_if(is.numeric, round, 4) %>%
    mutate(Table = as.character(Table)) %>%
    factor_arrange(., information)

  tab1$modal_res_all <-
    pmap(list(tab1$modal_est_cs_all, tab1$modal_selected_cp_all, tab1$modal_selected_cs_all, tab1$modal_selected_weights_all),
         ~ tab1_group_out_all(..1, ..2, ..3, ..4, information$base_data$WESS, modal = T)) %>%
    bind_rows() %>%
    select(-OOD) %>%
    mutate_if(is.numeric, round, 4) %>%
    mutate(Table = "All", .after = GCA) %>%
    factor_arrange(information)

  tab1$modal_res_com <-
    bind_rows(tab1$modal_res, tab1$modal_res_all) %>%
    arrange(GCA) %>%
    mutate_if(is.numeric, round, 4) %>%
    factor_arrange(information)

  tab1
}

gen_modal_table <- function(tab1) {
  # modal cut score ouput ready
  tab1 <- gen_indi_table(tab1, tab1$modal_res_com, "modal_table")
  tab1 <- gen_indi_table(tab1, tab1$modal_res_all, "modal_table_all")

  tab1
}


#' modal est cut score
estCutScore_mode <- function(data, information, type = "all") {
  # data <- est_cutscore; type = "all"

  gcaid <- information$data_ready$id_list$GCA
  gcaid <- factor(gcaid, levels = gcaid)

  cond <- information$data_ready$id_list$Table
  cond$GCA <- factor(cond$GCA, levels = gcaid)

  if(type == "all"){
    cond <- crossing(gcaid)
  }

  inploc <- information$base_data$loc_nm

  lvnm <- information$data_ready$level_nm
  locReady <- information$data_ready$location_ready
  WESS <- information$base_data$WESS

  mod_data_1 <- data %>% bind_rows()

  if(type == "all") {
    text_filter <- "filter(mod_data_1, GCA == cond[i,1] %>% pull())"
    table <- "Round"
    text_groupsplit <- "group_split(mod_data_2,GCA)"
  } else {
    text_filter <- "filter(mod_data_1, GCA == cond[i,1] %>% pull(), Table == cond[i,2] %>% pull())"
    table <- "Table"
    text_groupsplit <- "group_split(mod_data_2,GCA,Table)"
  }

  modal_ALD <-
    foreach(i = 1:nrow(cond)) %do% {
      # i <- 1
      ext_ALD <-
        eval(parse(text=text_filter)) %>%
        group_by_at(vars(matches("Panelist|User"))) %>%
        group_split() %>%
        map(., ~ .x$ALD) %>%
        set_names(., nm = 1:length(.)) %>%
        bind_cols()

      ext_cors <-
        eval(parse(text=text_filter)) %>%
        distinct_at(vars(matches("Panelist|User|Correlation"))) %>%
        pull(Correlation)

      tabl_ALD <- apply(ext_ALD, 1, get_mode, ext_cors)

    }

  mod_data_2 <-
    mod_data_1 %>%
    distinct(GCA, Round, !!as.name(table), Item_ID, OOD, !!as.name(inploc)) %>%
    mutate(ALD = unlist(modal_ALD))

  # final result -----------------------------------
  split_filter <-
    eval(parse(text=text_groupsplit)) %>%
    map(., ~ .x %>% select(-OOD, -all_of(inploc)))

  mode_cs <- map(split_filter, estCutScore, information)

  return(mode_cs)
}


#' gen_median_output
ready_median_output <- function(medTable) {

  medTable <-
    medTable %>%
    mutate_if(is.numeric, round, 2)

  page_name <- names(medTable)[str_detect(names(medTable), "_p")]
  panel.key <- medTable[, 1:2]
  table.inf <- medTable[, -c(1:2)]
  table.keep <- table.inf
  level_names <- names(table.inf)

  for(mi in 1:length(page_name)) {
    # mi <- 2
    mii <- 2 + 2*(mi-1)
    miii <- 1 + 2*(mi-1)
    mut.inp <- glue::glue(
      'paste0({level_names[{miii}]}," (",{level_names[{mii}]},")")'
    )
    table.inf <- table.inf %>% mutate(!!page_name[mi] := eval(parse(text = mut.inp)))
  }
  median_out <-
    table.inf %>% select(-ends_with("_loc")) %>%
    bind_cols(panel.key, .) %>%
    mutate(Table = as.character(Table),
           Table = if_else(Table == "0", "All", Table))
  return(median_out)
}

#' get_mode
get_mode <- function(x, corInf){
  # x <- ext_ALD[4,]
  # corInf <- ext_cors
  x <- x %>% unlist()

  cor_order <-
    data.frame(x, corInf) %>%
    arrange(., desc(corInf))

  a1 <- x
  a2 <- sort(table(a1), decreasing = T)

  if(sum(a2 == max(a2)) > 1){

    max_name <- names(which(a2 == max(a2)))

    a2 <-
      cor_order %>%
      filter(x %in% max_name) %>%
      .[1,1]
  } else {
    modal_ALD <- names(a2)[1]
  }
}

