#' @include 2_tab2_function.r
NULL

#' Cut score summary
#'
#' @param tab1 a list of data coming from \code{\link{gen_tab1}}
#' @param information a list of data coming from \code{\link{get_data_info}}
#' @return a list containing cut score summaries for each domain
gen_tab3 <- function(tab1, information) {

  tab3 <- list()

  tryCatch({

    dataUse_0 <- tab1$modal_res_all

    target_filter <- information$base_data$target_nm
    target_loc <- information$base_data$loc_nm

    gca_nm <- information$data_ready$id_list$GCA
    new_gca_nm <- str_split(gca_nm, "-", simplify = T)
    gca_level <- unique(new_gca_nm[,1])
    domain_level <- unique(new_gca_nm[,2])

    gca_nm <-
      data.frame(new_gca_nm) %>%
      set_names(c("GCA","Domain")) %>%
      mutate(GCA = factor(GCA, levels = gca_level),
             Domain = factor(Domain, levels = domain_level)) %>%
      arrange(Domain, GCA) %>%
      mutate(GCA = paste(GCA, Domain, sep = "-")) %>%
      pull(GCA)

    domain <- information$base_data$domain
    level_nm <- information$data_ready$level_nm[, gca_nm]

    setup_data <- information$imported_data$setup_data
    examinee_data <- information$imported_data$examinee_data

    if("GCA" %in% domain & length(domain) == 1) {

      item_data <- information$imported_data$item_data %>%
        mutate(GCA = paste(GCA, "All", sep = "-"))

    } else if("Domain" %in% domain & length(domain) == 2) {

      item_data_all <- information$imported_data$item_data %>%
        mutate(GCA = paste(GCA, "All", sep = "-"))

      item_data <- information$imported_data$item_data %>%
        mutate(GCA = paste(GCA, Domain, sep = "-")) %>%
        bind_rows(item_data_all, .)

    } else if("Domain" %in% domain & length(domain) == 1) {
      item_data <- information$imported_data$item_data %>%
        mutate(GCA = paste(GCA, Domain, sep = "-"))
    }

    item_data <- item_data %>%
      mutate(GCA = factor(GCA, levels = gca_nm)) %>%
      arrange(GCA)

    gca_p <-
      which(remove_blank(names(examinee_data)) ==
              remove_blank("grade")|
              remove_blank(names(examinee_data)) ==
              remove_blank("gca"))

    examinee_data <-
      examinee_data %>%
      filter(GCA %in% gca_nm) %>%
      mutate(GCA = factor(GCA, levels = gca_nm)) %>%
      group_split(!!as.name(names(examinee_data)[gca_p])) %>%
      set_names(., nm = gca_nm)

    item_data <-
      item_data %>%
      filter(GCA %in% gca_nm) %>%
      group_split(GCA) %>%
      set_names(., nm = gca_nm)

    num_item <- map(item_data, nrow)
    loc_num <- map(item_data, ~ .x %>% dplyr::select(all_of(target_loc)))

    dataUse_0 <-
      dataUse_0 %>%
      mutate(GCA = factor(GCA, levels = gca_nm)) %>%
      arrange(GCA)

    data_name <- names(dataUse_0)
    page_name <- data_name[str_detect(data_name, "_p")]
    loc_name <- data_name[str_detect(data_name, "_loc")]
    level_names <- paste0("Level", 1:(length(page_name)+1))
    cut_data <- dataUse_0 %>% select(matches("_p|_loc"))

    dataUse_0 <-
      # dataUse_0 %>% mutate_at(loc_name,  ceiling)
       dataUse_0 %>% mutate_at(page_name,  ceiling)

    tab3$page_data <-
      map(1:nrow(dataUse_0), gen_page_data,
          dataUse_0, item_data, examinee_data, page_name,loc_name,
          target_loc, level_names)

    tab3$scale_scores <-
      map(tab3$page_data, ~ .x$scale_scores) %>%
      bind_rows() #%>%
      #mutate_at("scaleScore",  ceiling)

    tab3$perc_ins <-
      map(tab3$page_data, ~ .x$perc_ins) %>%
      bind_rows()
    tab3$perc_atabos <-
      map(tab3$page_data, ~ .x$perc_atabos) %>%
      bind_rows()
    tab3$perc_bel <-
      map(tab3$page_data, ~ .x$perc_bel) %>%
      bind_rows()

    tab3$perc_ins <-
      tab3$perc_ins %>%
      mutate(perIn_c = paste0(percIn, "%"))
    tab3$perc_atabos <-
      tab3$perc_atabos %>%
      mutate(percAtabo_c = paste0(percAtabo, "%"))
    tab3$perc_bel <-
      tab3$perc_bel %>%
      mutate(percBel_c = paste0(percBel, "%"))

    blank_page <-
      map2(gca_nm, data.frame(level_nm), gen_blank_page)

    perIn <- tab3$perc_ins %>% group_split(GCA)
    percAtabv <- tab3$perc_atabos %>% group_split(GCA)
    percBel <- tab3$perc_bel %>% group_split(GCA)

    min_data <- cut_data %>% select(ends_with("_p"))
    cut_score_data <- cut_data  %>% select(ends_with("_loc"))

    tab3$eff_page <-
      map(1:length(blank_page), summarize_page,
          blank_page, perIn, percAtabv, percBel, min_data, level_names, num_item, cut_score_data) %>%
      bind_rows(.)

    eff_page <-
      tab3$eff_page %>%
      mutate(GCA = factor(GCA, levels = gca_nm)) %>%
      group_split(GCA)

    for(i in 1:length(eff_page)) {
      if(str_detect(pull(eff_page[[i]][3,3]), "NA")) {
        eff_page[[i]] <- eff_page[[i]] %>%
          mutate_at(
            vars(matches("^Level")),
            ~ if_else(`.` %in%  c("Percent in Level", "Percent at or Above Level","Percent Below Level"), "", .x)
          )
      } else {
        eff_page[[i]] <- eff_page[[i]]
      }
    }
    tab3$eff_page <- bind_rows(eff_page)
  },

  error = function(e) {
    tab3$error <- T
  })
  return(tab3)
}


# Helper functions -------------------------------------
#' gen_page_data
gen_page_data <- function(ep, datainp, fourthData, fifthData, pageName, locName, targetLoc, lvnm){
  # ep = 1; datainp = dataUse_0; fourthData = item_data; fifthData = examinee_data; pageName =  page_name; targetLoc=target_loc; lvnm = level_names; locName = loc_name
  dataUse_1 <- datainp[ep, ]
  gca_id <- dataUse_1 %>% pull(1)
  cutpoint_inp <- dataUse_1 %>% select(all_of(pageName))
  cutscore_inp <- dataUse_1 %>% select(all_of(locName))
  loc_num <- fourthData[[ep]] %>% pull(targetLoc)

  sclScore <-
    if(sum(str_detect(toupper(names(fifthData[[ep]])), toupper("freq"))) == 0) {
      fifthData[[ep]] %>% pull(2)
    } else {

      rep(fifthData[[ep]]$score, fifthData[[ep]]$freq)
    }

  min_point <- cutpoint_inp %>% unlist()
  scale_score <- unlist(cutscore_inp) # map(1:length(min_point), ~ loc_num[min_point[.x]]) %>% unlist()

  lv_data <- lv_por(sclScore, scale_score)

  perc_in <- lv_data$pors
  perc_atabo <- lv_data$atabv
  perc_bel <- lv_data$bel

  scale_scores <-
    scale_score %>%
    tibble( scaleScore = ., Level = lvnm[-1], GCA = gca_id)

  perc_ins <-
    perc_in %>%
    tibble( percIn = ., Level = lvnm, GCA = gca_id)

  perc_atabos <-
    perc_atabo %>%
    tibble( percAtabo = ., Level = lvnm[-1], GCA = gca_id)

  perc_bel <-
    perc_bel %>%
    tibble( percBel = ., Level = lvnm[-1], GCA = gca_id)

  return(
    list(scale_scores = scale_scores,perc_ins = perc_ins,
         perc_atabos = perc_atabos, perc_bel = perc_bel))
}
#' lv_por
lv_por <- function(scores, scale_score) {
  # scores = sclScore;

  if(any(scores == -999)) {
    lv_length <- length(scale_score) + 1

    lv_first_por = NA
    lv_md_por = rep(NA, lv_length-2)
    lv_last_por = NA
    atabv = NA
    bel = NA

  } else {

    lv_first_por <- round(sum(scores < scale_score[1]) / length(scores), 3)*100


    lv_md_por <-if(length(scale_score) == 1) {

      # myi <- 0
      # round(sum( scores < scale_score[myi+1] & scores >= scale_score[myi]) / length(scores), 3)*100
      NULL

    } else {

      foreach(i = 1:(length(scale_score)-1), .combine = 'c') %do% {
        myi <- i
        round(sum( scores < scale_score[myi+1] & scores >= scale_score[myi]) / length(scores), 3)*100
      }
    }

    lv_last_por <- round(sum( scores >= scale_score[length(scale_score)] ) / length(scores), 3)*100

    atabv <- foreach(i = 1:(length(scale_score)), .combine = 'c') %do% {
      round(sum( scores >= scale_score[i]) / length(scores), 3)*100
    }

    bel <- foreach(i = 1:(length(scale_score)), .combine = 'c') %do% {
      round(sum( scores < scale_score[i]) / length(scores), 3)*100
    }
  }

  list(pors = c(lv_first_por, lv_md_por, lv_last_por),
       atabv = atabv, bel = bel)
}
#' gen_blank_page
gen_blank_page <-
  function(GCA, lvnm){
    # GCA = gca_nm; lvnm = information$data_ready$level_nm[[1]]
    `.` <- c("OIB Page","Cut Scores","Percent in Level",
             "Percent at or Above Level","Percent Below Level")
    page_name <- paste0("Level", 1:length(lvnm))
    sum_table_1 <- tibble(GCA, `.`)
    for(pn in page_name) {
      sum_table_1 <- sum_table_1 %>%
        mutate(!!pn := 0)
    }
    return(sum_table_1)
  }
#' summarize_page
summarize_page <- function(ep, blankPage, perIns, percAtabove, perBelow, minData, lvnm,numItem, cut_score_data){

  # ep = 1
  # blankPage = blank_page
  # perIns = perIn
  # percAtabove = percAtabv
  # perBelow = percBel
  # minData = min_data
  # lvnm =level_names
  # numItem = num_item
  # locNum = loc_num

  sum_page <- blankPage %>% .[[ep]]
  perIn_c <- perIns %>% .[[ep]] %>% pull(perIn_c)
  percAtabo_c <- percAtabove %>% .[[ep]] %>% pull(percAtabo_c)
  percBel_c <- perBelow %>% .[[ep]] %>% pull(percBel_c)
  min_point <- minData[ep, ] %>% unlist()
  cut_score <- cut_score_data[ep, ] %>% unlist()
  numItem_1 <- numItem %>% .[[ep]]

  levelData <- vector("list", length(lvnm))
  levelData[[1]] <-
    if(min_point[1] == 1){
    c(glue("1-1"), "", perIn_c[1], "100%", "" )
    } else {
      c(glue("1-{min_point[1]-1}"), "", perIn_c[1], "100%", "" )

    }

  for(i in 1:length(min_point)) { # i = 1
    ii <- i + 1
    point_inp <- ifelse(ii <= length(min_point), min_point[ii] - 1, numItem_1)
    levelData[[ii]] <- c(glue("{min_point[i]}-{point_inp}"), cut_score[[i]],perIn_c[ii], percAtabo_c[i], percBel_c[i])
  }

  for(li in 1:length(levelData)) { # li = 1
    lii <- li + which(names(sum_page)==".")
    sum_page[,lii] <- levelData[[li]]
  }
  return(sum_page)
}
