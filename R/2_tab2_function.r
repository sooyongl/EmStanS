#' @include 2_tab1_function.r
NULL

#' Detailed ESS results
#'
#' @param tab1 a list of data coming from \code{\link{gen_tab1}}
#' @param information a list of data coming from \code{\link{get_data_info}}
#' @return a list containing detailed ESS results
gen_tab2 <- function(tab1, information) {

  tab2 <- list()

  modal_est_cs <- tab1$modal_est_cs_all
  modal_est_cp <- tab1$modal_est_cp_all
  modal_selected_cp <- tab1$modal_selected_cp_all
  modal_selected_cs <- tab1$modal_selected_cs_all

  n.of.gca <- information$data_ready$id_list$GCA
  n.of.tb <- rep(1, length(n.of.gca))

  est_cutscore <- modal_est_cs

  target_filter <- information$base_data$target_nm
  WESS <- information$base_data$WESS
  gamest <- information$base_data$gamest
  EC_data <- information$data_ready$EC_data

  tab2$for_tab2_out <-
    lapply(1:length(n.of.gca), function(vi) {
      # vi = 1 ; vvi = 1
      lv_nm <- information$data_ready$level_nm[,vi]

      in_num <- n.of.tb[vi]
      lapply(1:in_num, function(vvi) {

        EC <- EC_data[vi, "EC"]
        selected_cp <- modal_selected_cp[[vi]]
        selected_cs <- modal_selected_cs[[vi]]

        tbl_data <- est_cutscore[vi][[vvi]] %>% select(-Correlation)

        eff_data <- tab2_effdata(tab1$modal_table_all[vi,])

        ct_data <- tab2_crosstab(dataUse = tbl_data,
                                 EC = 0,
                                 distance_res = NULL,
                                 lv_nm = lv_nm)
        distance_inp <-
          suppressWarnings(crosstab_ready(tab1$modal_res_all[vi,],
                              tab1$modal_selected_cp_all[[vi]],
                              tbl_data,
                              information))
        ct_data_ec <-
          tryCatch(
            tab2_crosstab(tbl_data, EC = EC, distance_res = distance_inp,
                          lv_nm),

            error = function(e) "Error"
          )


        gam_fitted <- splineFit(tbl_data, information)

        plot_data <- tab2_plot_data(tbl_data, information, vi)
        p1 <- tab2_plot(tbl_data, plot_data, information, vi, selected_cp, selected_cs, gam_fitted)

        list(eff_data = eff_data,
             t_out    = tbl_data,
             crosst   = ct_data,
             crosst_ec   = ct_data_ec,
             p1       = p1,
             plot_data = plot_data,
             selected_cp = selected_cp,
             selected_cs = selected_cs,
             gam_fitted = gam_fitted)
      })

    })

  names(tab2$for_tab2_out) <- n.of.gca

  return(tab2)
}

# Helper functions ------------------------------------
#'
tab2_effdata <- function(datainp){
  datainp %>%
    data.frame() %>%
    mutate(Correlation = round(Correlation, 3))
}
#'
#' dataUse = tbl_data
#' EC = EC; distance_res = distance_inp
tab2_crosstab <- function(dataUse, EC = 0, distance_res = NULL, lv_nm = NULL) {
  # dataUse <- tbl_data

  lv_nm <- unique(lv_nm)
  lv_len <- length(lv_nm)

  ct_0 <- matrix(0, lv_len, lv_len)
  rownames(ct_0) <- colnames(ct_0) <- lv_nm


  if(EC > 0) {

    dataUse <-
      dataUse %>%
      left_join(., distance_res[, c("Item_ID","Distance")], by = "Item_ID")

    dataUse <-
      dataUse %>%
      select(ALD, Operational_Lv, Distance) %>%
      mutate(
        ALD = case_when(
          Distance < EC & !is.na(Distance) ~ Operational_Lv,
          TRUE ~ ALD
        )
      )
  }

  ct_data <- dataUse %>% select(ALD, Operational_Lv)

  ct_1 <- xtabs(as.formula(paste0("~","Operational_Lv","+","ALD")), ct_data)

  for(cname in colnames(ct_1)) {
    for(rname in rownames(ct_1)) {
      ct_0[rname, cname] <- ct_1[rname, cname]
    }
  }

  ct_1 <- as.data.frame.matrix(ct_0)

  # if(length(rownames(ct_1)) == lv_len & length(colnames(ct_1)) == lv_len ){
  #
  #
  #   ct_1 <- as.data.frame.matrix(ct_1)
  #
  # } else if(length(rownames(ct_1)) == lv_len | length(colnames(ct_1)) == lv_len ){
  #
  #   if(length(rownames(ct_1)) != length(colnames(ct_1))) {
  #     if(length(rownames(ct_1)) > length(colnames(ct_1))) {
  #       total.lv <- rownames(ct_1)
  #       fill_0 <- rep(0, length(total.lv))
  #
  #       new_ct <- as.data.frame.matrix(diag(length(total.lv)))
  #       colnames(new_ct) <- total.lv
  #       rownames(new_ct) <- total.lv
  #
  #       cons.num <- match(rownames(ct_1), colnames(ct_1))
  #       cons.num <- cons.num[which(!is.na(cons.num))]
  #       omit.num <- seq_len(length(total.lv))[-cons.num]
  #
  #       cons.lv <- total.lv[cons.num]
  #       omit.lv <- total.lv[omit.num]
  #
  #       ct_1 <- as.data.frame.matrix(ct_1)
  #
  #       row_ <- rownames(ct_1)
  #       col_ <- colnames(ct_1)
  #
  #       for(ri in 1:length(row_)) {
  #         for(ci in 1:length(col_)){
  #           row_which <- as.numeric(str_extract(row_[ri], pattern = "[[:digit:]]"))
  #           col_which <- as.numeric(str_extract(col_[ci], pattern = "[[:digit:]]"))
  #
  #           new_ct[row_which, col_which] <- ct_1[ri, ci]
  #         }
  #       }
  #       new_ct[, omit.num] <- fill_0
  #
  #       ct_1 <- new_ct
  #
  #     } else {
  #       total.lv <- colnames(ct_1)
  #       fill_0 <- rep(0, length(total.lv))
  #
  #       new_ct <- as.data.frame.matrix(diag(length(total.lv)))
  #       colnames(new_ct) <- total.lv
  #       rownames(new_ct) <- total.lv
  #
  #       cons.num <- match(rownames(ct_1), colnames(ct_1))
  #       omit.num <- seq_len(length(total.lv))[-cons.num]
  #
  #       cons.lv <- total.lv[cons.num]
  #       omit.lv <- total.lv[omit.num]
  #
  #       ct_1 <- as.data.frame.matrix(ct_1)
  #
  #       row_ <- rownames(ct_1)
  #       col_ <- colnames(ct_1)
  #
  #       for(ri in 1:length(row_)) {
  #         for(ci in 1:length(col_)){
  #           row_which <- as.numeric(str_extract(row_[ri], pattern = "[[:digit:]]"))
  #           col_which <- as.numeric(str_extract(col_[ci], pattern = "[[:digit:]]"))
  #
  #           new_ct[row_which, col_which] <- ct_1[ri, ci]
  #         }
  #       }
  #       new_ct[omit.num, ] <- fill_0
  #
  #       ct_1 <- new_ct
  #     }
  #   } else {
  #     ct_1 <- as.data.frame.matrix(ct_1)
  #   }
  #
  # } else {
  #
  #   if(length(rownames(ct_1)) != length(colnames(ct_1))) {
  #
  #     total.lv <- sort(lv_nm)
  #     fill_0 <- rep(0, length(total.lv))
  #
  #     new_ct <- as.data.frame.matrix(diag(0,length(total.lv)))
  #     colnames(new_ct) <- total.lv
  #     rownames(new_ct) <- total.lv
  #
  #     for(i in rownames(ct_1)) {
  #       for(j in colnames(ct_1)) {
  #         new_ct[i, j] <- ct_1[i, j]
  #       }
  #     }
  #
  #     ct_1 <- as.data.frame.matrix(new_ct)
  #   }
  #
  # }



  # summary table
  ct_mat <- as.matrix(ct_1)

  total <- sum(ct_mat)
  agree <- sum(diag(ct_mat))
  disagree <- sum(ct_mat[lower.tri(ct_mat)]) + sum(ct_mat[upper.tri(ct_mat)])

  num_r <- nrow(ct_mat)
  diag.pos <- seq(from = 1, to = num_r^2, num_r+1)
  adj.pos <- sort(c(diag.pos + 1, diag.pos - 1))
  adj.pos <- adj.pos[-c(1, length(adj.pos))]

  adj_dis <- sum(ct_mat[adj.pos])
  disc_dis <- sum(ct_mat[-c(diag.pos, adj.pos)])
  wkappa <- suppressWarnings(psych::cohen.kappa(ct_1)$weighted.kappa)
  agree_num <- c(agree, disagree, adj_dis, disc_dis)
  agree_per <- paste0(round(agree_num/total, 2) * 100, "%")

  table_sum <- data.frame(
    name = c("Agree","Disagree","Adj_dis","Disc_dis", "Wkappa"),
    values = c(agree_num, round(wkappa, 2)),
    perc = c(agree_per, "")
  )

  # marginal
  ct_num <- ct_1
  ct_prop<- round(ct_1 / sum(ct_1) * 100, 1)

  right_num <- rowSums(ct_1)
  right_prop <- round(right_num / sum(ct_1) * 100, 1)

  bot_num <- c(colSums(ct_1), sum(ct_1))
  bot_prop <- round(bot_num / sum(ct_1) * 100, 1)

  num_t <- data.frame(ct_num, Total = right_num)
  num_t <- rbind(num_t, bot_num)

  num_p <- data.frame(ct_prop, Total = right_prop)
  num_p <- rbind(num_p, bot_prop)

  table_cross <- num_t
  for(i in 1:nrow(num_t)) {
    for(j in 1:ncol(num_t)){
      table_cross[i, j] <- paste0(num_t[i, j],"<br>",num_p[i, j],"%")
    }
  }

  rownames(table_cross)[length(rownames(table_cross))] <- "Total"

  list(table_cross, table_sum)
}

#'
crosstab_ready <- function(tab1_filtered, tab1_cut, tab2_tbl, information) {

  # tab1_filtered = tab1$modal_res_all[vi,]
  # tab1_cut = tab1$modal_selected_cp_all[[vi]]
  # tab2_tbl = tbl_data

  n.of.gca <- information$data_ready$id_list$GCA
  n.of.tb <- rep(1, length(n.of.gca))

  loc_nm <- information$base_data$loc_nm

  level_names <- information$data_ready$level_nm[[1]]
  level_names <-  level_names[2:length(level_names)]

  SD <- information$data_ready$SD_data

  cs_inf <-
    tab1_filtered %>%
    select(GCA, ends_with("_loc")) %>%
    set_names(., nm = c("GCA", level_names)) %>%
    # mutate(GCA = as.character(GCA)) %>%
    gather(., "ALD", "Cut_Score", -GCA)

  dataUse_Weight <-
    tab2_tbl %>%
    mutate(
      weightSum = dplyr::select(., ends_with("_W")) %>%
        rowSums()
    ) %>% pull(weightSum)

  target_names <- tab2_tbl$ALD

  lv_name <- target_names %>% unique() %>% sort()

  given_n <- c(1:length(lv_name))
  length_n <- ifelse(length(given_n) == 1, 2, length(given_n))
  targets_n <- match(target_names, lv_name)

  er <- length(target_names)
  review_table <-
    foreach(n = 2:length_n, .combine = 'rbind') %do% {
      # n = 2
      cr <- tab1_cut[(n-1)]
      # cr = 3
      cr_abo <- cr
      cr_bel <- cr

      c_bel <- which(targets_n[0:(cr - 1)] >= n)
      c_abo <- (which(targets_n[cr_abo:er] < n) - 1) + cr_abo

      tab2_tbl[c(c_bel, c_abo), ]
    }
  all_names <- names(review_table)
  review_table <- review_table %>% distinct(!!!syms(all_names))
  review_table <-
    review_table %>%
    select(-Round) %>%
    mutate(.,
           Weight = dplyr::select(., ends_with("_W")) %>%
             rowSums()
    )

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
    review_table %>%
    left_join(., cs_inf, by = c("GCA" = "GCA",
                                "ALD" = "ALD")) %>%
    left_join(., cs_inf_upper, by = c("GCA" = "GCA",
                                      "ALD" = "ALD")) %>%
    left_join(., SD, by = c("GCA" = "GCAid")) %>%

    mutate(AN = extract_num(ALD),
           ON = extract_num(Operational_Lv),
           Diff_LV = AN - ON,
           Distance = case_when(
             AN > ON ~ !!as.name(loc_nm) - Cut_Score,
             AN < ON ~ !!as.name(loc_nm) - (Cut_Score_upper - 1)
           ),
           `Std. Distance` = round(Distance / SD, 3)
    ) %>%

    select(-matches("L[[:digit:]]+"), -AN, -ON) %>%
    select(
      GCA, Item_ID, OOD,
      ALD, Operational_Lv, Diff_LV,
      starts_with("Loc"),
      Distance,`Std. Distance`) %>%
    rename(
      "Aligned_Lvl" = "ALD",
      "Operational_Lvl" = "Operational_Lv",
      "Lvl_Diff" = "Diff_LV"
    ) %>%
    arrange(
      GCA, Item_ID
    ) %>%
    mutate(
      Distance = abs(Distance)
    )
  item_review_table
}

#' tab2_plot_data
tab2_plot_data <- function(dataInp, information, vvv){
  # dataInp = data_inp; information; vvv
  dataUse_1 <- dataInp

  target_filter <- information$base_data$target_nm
  targ_p <- which(names(dataUse_1) == target_filter)
  lv_names <- names(dataUse_1)[(targ_p + 1):(ncol(dataUse_1)-1)]
  loc_nm <- information$base_data$loc_nm

  dataUse_1 <-
    dataUse_1 %>%
    mutate(
      OOD_1 = paste0(OOD,"\n", !!as.name(loc_nm)),
      OOD_1 = factor(OOD_1, levels = paste0(OOD,"\n", !!as.name(loc_nm))
      )
    ) %>%
    select(OOD_1, !!as.name(loc_nm), (targ_p + 1):(ncol(dataUse_1)-1)) %>%
    gather(., "Cut Level", "Inconsistency", -c(OOD_1), -!!as.name(loc_nm))

  if(information$base_data$WESS) {
    dataUse_1 <-
      dataUse_1 %>%
      filter( str_detect(`Cut Level`, "_W" ))

  } else {
    dataUse_1 <-
      dataUse_1 %>%
      filter( !str_detect(`Cut Level`, "_W" ))
  }
  return(dataUse_1)
}

#' tab2_plot
tab2_plot <- function(data_inp, plot_data, information, vvv, selected_cp, selected_cs, gam_fitted, font_size = 14){
  # data_inp = tbl_data; vvv = vi; WESS = T
  WESS <- information$base_data$WESS
  smoothing <- information$base_data$gamest
  loc_name <- information$base_data$loc_nm
  dataUse <- plot_data

  gam_value <- gam_fitted$gam_value
  gam_fit <- gam_fitted$gam_fit

  cut_dt <- dataUse %>%
    group_split(`Cut Level`) %>%
    map2(., selected_cp, slice) %>%
    bind_rows() %>%
    mutate(OOD = OOD_1) %>%
    separate(OOD, c("cutp","cutcs"), "\n") %>%
    mutate(
      !!loc_name := selected_cs,
      cutcs = selected_cs
    )

  if(smoothing) {
    fitted_val <- gam_value

    temp_incon <- map2_dbl(gam_fit, selected_cs, ~ calGamOutcome(.x, .y) %>% round(2))

    cut_dt[["Inconsistency"]] <- temp_incon
  }

  breaks = function(x, xby) x[seq(1, length(x), by=xby)]

  cutLevel <- dataUse %>% pull(`Cut Level`) %>% unique()

  incon_value <-
    foreach(i = 1:length(cutLevel), .combine = 'cbind') %do% {
      dataUse %>%
        filter(`Cut Level` == cutLevel[i]) %>%
        pull(Inconsistency)
    }

  # cross_value <-
  #   foreach(i = 1:(dim(incon_value)[2] - 1), .combine = 'c') %do% {
  #     ii = i + 1
  #     incon_value[which( incon_value[,i] == incon_value[,ii] )[1], 1]
  #   }

  bk_x <- ceiling(length(unique(dataUse$OOD_1))/20)
  x_disc <- breaks(unique(dataUse$OOD_1), bk_x)

  unique_incons <- unique(dataUse$Inconsistency)
  bk_y = ceiling(length(unique_incons)/20)
  y_disc <- breaks(unique(dataUse$Inconsistency), bk_y)

  x_range <- dataUse %>% pull(loc_name)
  x_range <- c(min(x_range), max(x_range))

  y_range <- dataUse %>% pull(Inconsistency)
  y_range <- c(min(y_range), max(y_range))

  # ggplot -----------------------------------
  geom.text.size = font_size * (5/14)
  theme_set(theme_bw(base_size = font_size) +
              theme(plot.margin = unit(c(.1,.1,.1,.1), "cm")) +
              theme(
                legend.position = "bottom",
                legend.justification = c(0, 1)))

  arrow_length <- max(dataUse[["Inconsistency"]])/6

  pp1 <-
    dataUse %>%
    ggplot(aes(x = !!as.name(loc_name), y = Inconsistency,
               yend = 0, xend = x_range[1])) +
    geom_point(aes(colour = factor(`Cut Level`)),
               fill = "white", size = 2, stroke = .5, shape = 21)

  if(smoothing) {
    pp1 <- pp1 +
      geom_line(
        data = fitted_val,
        aes(group = factor(`Cut Level`),
            linetype = factor(`Cut Level`),
            colour = factor(`Cut Level`)),
        linewidth = 1.2,
        alpha = .5
      )
  }

  pp1 <- pp1 +
    geom_point(
      data = cut_dt,
      aes(x = !!as.name(loc_name),
          y = Inconsistency),
      fill = "white",
      size = rel(2.5),
      stroke = rel(1.5),
      shape = 21,
      alpha = .8
    ) +
    geom_segment(
      data = cut_dt,
      aes(yend = Inconsistency),
      lty = "dashed",
      colour = "grey30"
    ) +
    geom_segment(
      data = cut_dt,
      aes(xend = !!as.name(loc_name)),
      lty = "dashed",
      colour = "grey30"
    ) +
    geom_text(
    # geom_label_repel(
      data = cut_dt,
      aes(x = !!as.name(loc_name),
          y = Inconsistency,
          label = cutcs),
      fontface = 'bold',
      size = geom.text.size,
      # nudge_y = arrow_length,
      # arrow = arrow(length = unit(0.02, "npc")),
      # box.padding = 1,
      alpha = 0.8
    ) +
    labs(x = "Location",
         y = "Number of Inconsistent Items",
         colour = "Cut Level",
         title = "",
         caption = "") +
    guides(linetype = "none") +
    scale_colour_brewer(palette = "Set1")

  if(WESS){
    pp1 <-
      pp1 +
      scale_x_continuous(
        limits = x_range,
        breaks = round(seq(from = x_range[1],
                           to = x_range[2],
                           length.out = 10),0)
      ) +
      labs(y = "Total Weights",
           title = "") +
      scale_y_continuous(
        breaks = seq(from = round(min(dataUse$Inconsistency),0),
                     to = round(max(dataUse$Inconsistency),0),
                     length.out = 5)
      )
  } else {
    pp1 <- pp1 + scale_y_continuous(breaks = y_disc)
  }

  pp1

}

#'
ggplotly_render <- function(gg_obj, plot_data, selected_cp, selected_cs, information, font_size = 12) {
  # gg_obj <- p1;
  loc_name <- information$base_data$loc_nm
  cut_dt <- plot_data %>%
    group_split(`Cut Level`) %>%
    map2(., selected_cp, slice) %>%
    bind_rows() %>%
    mutate(OOD = OOD_1) %>%
    separate(OOD, c("cutp","cutcs"), "\n") %>%
    mutate(
      !!loc_name := selected_cs,
      cutcs = selected_cs
    )

  annotation <- list(yref = 'paper',
                     xref = "paper",
                     y = 1,
                     x = .95,
                     showarrow=FALSE,
                     text = paste0("Cut score: ",paste(cut_dt[["cutcs"]], collapse = ", ")))

  a1 <- c(3, 6)
  if(length(gg_obj$layers) == 5) {
    a1 <- a1 - 1
  }
  gg_obj$layers[a1] <- NULL

  gg_obj <- gg_obj + theme_bw(base_size = font_size)

  ggplotly(gg_obj) %>%
    plotly::layout(
      legend = list(orientation = "h", x = 0, y = 1.15),
      annotations= list(annotation)
      )
}




