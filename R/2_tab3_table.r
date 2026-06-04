#' @include 2_tab2_function.r
NULL

#' Generate cut score summary table
#'
tab3_table_pagetb <- function(tab3) {

  if(is.null(names(tab3))) {

    # tab3$effpage <- "Null"

  } else {

    page_data <- tab3$eff_page %>% data.frame()

    gca_level <- unique(str_split(page_data$GCA, "-", simplify = T)[,2])

    page_data_list <-
      page_data %>%
      mutate(GCA_temp = GCA) %>%
      separate(GCA_temp, c("a1","a2"),"-") %>%
      mutate(a2 = factor(a2, levels = gca_level)) %>%
      group_split(a2) %>%
      map(~ .x %>% dplyr::select(-a1, -a2))


    effpage <- vector("list", length(page_data_list))
    for(i in 1:length(page_data_list)) {

      page_data <- page_data_list[[i]]
      forline <- page_data %>% pull(1) %>% unique() %>% length()
      kable.line <- 1:forline
      for(fl in 1:forline){
        kable.line[fl] <- 5*fl + 0
      }

      effpage[[i]] <-
        page_data %>%
        kable(.,"html", escape = F, align = "c",
              table.attr = "style='width:50%;'") %>%
        kable_styling(bootstrap_options = c("striped"),
                      full_width = F,
                      position = "left",
                      font_size = 18,
                      fixed_thead = T) %>%
        row_spec(1:nrow(page_data), color = "black") %>%
        row_spec(0, angle = 0,
                 background = "floralwhite",
                 extra_css = "border-bottom: 2px solid; border-bottom-color: black") %>%
        collapse_rows(columns = 1:2, valign = "top") %>%
        row_spec(., kable.line, extra_css = "border-bottom: 2px solid; border-bottom-color: black;line-height: 60%;") %>%
        row_spec(., 1:nrow(page_data), extra_css = "line-height: 60%;")
    }
    tab3$effpage <- effpage
  }


  return(tab3)
}

#' Generate VA summary plots
#'
tab3_plots <- function(tab3, font_size = 16, pallete = "set2") {

  if(is.null(names(tab3))) {

    # tab3_plots <- list(p_page1 = "Null", p_page2 = "Null", p_page3 = "Null")

  } else {

    color_tab3 <- getColors(pallete = pallete, rep_n = 2)
    num_lv <- length(unique(tab3$scale_scores$Level))+1

    geom.text.size = font_size * (5/14)
    theme_set(theme_bw(base_size = font_size) + theme(plot.margin = unit(c(.1,.1,.1,.1), "cm")))

    scale_scores <- tab3$scale_scores
    gca_level <- unique(str_split(scale_scores$GCA, "-", simplify = T)[,2])

    scale_scores_list <-
      scale_scores %>%
      mutate(GCA_temp = GCA) %>%
      separate(GCA_temp, c("a1","a2"),"-") %>%
      mutate(a2 = factor(a2, levels = gca_level)) %>%
      group_split(a2) %>%
      map(~ .x %>% dplyr::select(-a1, -a2))


    perc_ins <- tab3$perc_ins
    perc_ins_list <-
      perc_ins %>%
      mutate(GCA_temp = GCA) %>%
      separate(GCA_temp, c("a1","a2"),"-") %>%
      mutate(a2 = factor(a2, levels = gca_level)) %>%
      group_split(a2) %>%
      map(~ .x %>% dplyr::select(-a1, -a2))

    perc_atabos <- tab3$perc_atabos
    perc_atabos_list <-
      perc_atabos %>%
      mutate(GCA_temp = GCA) %>%
      separate(GCA_temp, c("a1","a2"),"-") %>%
      mutate(a2 = factor(a2, levels = gca_level)) %>%
      group_split(a2) %>%
      map(~ .x %>% dplyr::select(-a1, -a2))


    tab3_plots <- vector("list", length(scale_scores_list))
    for(i in 1:length(scale_scores_list)) {

      gca_nm <- str_split(scale_scores_list[[i]]$GCA, "-", simplify = T)
      gca_lv <- unique(gca_nm[,1])
      domain_lv <- unique(gca_nm[,2])

      scale_scores <- scale_scores_list[[i]] %>% separate(GCA, c("GCA", "none"), "-") %>% mutate(GCA = factor(GCA, levels = gca_lv))
      perc_ins <- perc_ins_list[[i]]  %>% separate(GCA, c("GCA", "none"), "-") %>% mutate(GCA = factor(GCA, levels = gca_lv))
      perc_atabos <- perc_atabos_list[[i]] %>% separate(GCA, c("GCA", "none"), "-") %>% mutate(GCA = factor(GCA, levels = gca_lv))

      p_page1 <-
        scale_scores %>%
        mutate(Level = factor(Level),
               Level = factor(Level, levels = rev(levels(Level)))
        ) %>%
        ggplot(aes(x = GCA, y = scaleScore, group = Level)) +
        geom_point(aes(colour = Level),
                   fill = "white",
                   size = 3,
                   stroke = 2,
                   shape = 21,
                   alpha = .8) +
        geom_line(aes(colour = Level, linetype = Level),
                  linewidth = 1.2) +
        # geom_label_repel(
        geom_text(
          aes(label = scaleScore),
                         # label.padding=.1,
                         alpha = 0.8,
                         size = geom.text.size) +
        labs(title = glue("Scale Score Cut Scores ({domain_lv})"),
             y = "Scale Score Cut Scores") +
        scale_color_manual(values = color_tab3[num_lv:2])

      p_page2 <-
        if(all(is.na(perc_ins[[1]]))) {

          tibble(x = 1:10, y = 1:10) %>%
            ggplot(aes(x = x, y = y)) +
            annotate("text", x = 1, y = 1, label = "Data not available") +
            theme_void()


        } else {
          perc_ins %>%
            mutate(Level = factor(Level),
                   Level = factor(Level, levels = rev(levels(Level)))
            ) %>%
            ggplot(aes(x = GCA, y = percIn)) +
            geom_col(aes(fill = Level)) +
            geom_text(aes(label = percIn, group = Level),
                      size = geom.text.size,
                      position = position_stack(vjust = .5)) +
            labs(title = glue("Percentage in Level ({domain_lv})"),
                 y =  "Percentage in Level") +
            scale_fill_manual(values = color_tab3[num_lv:1])
        }

      p_page3 <-
        if(all(is.na(perc_atabos[[1]]))) {
          tibble(x = 1:10, y = 1:10) %>%
            ggplot(aes(x = x, y = y)) +
            annotate("text", x = 1, y = 1, label = "Data not available") +
            theme_void()
        } else {

          perc_atabos %>%
            mutate(Level = factor(Level),
                   Level = factor(Level, levels = rev(levels(Level)))
            ) %>%
            ggplot() +
            geom_line(
              aes(x = GCA, y = percAtabo,
                  colour = Level, group = Level,
                  linetype = Level),
              linewidth = 1.5) +
            # geom_label_repel(
            geom_text(
              aes(label = percAtabo,
                                 x = GCA, y = percAtabo, group = Level),
                             size = geom.text.size,
                             vjust = 1) +
            labs(title = glue("Percentage At or Above Cut Score ({domain_lv})"),
                 y = "Percentage At or Above Cut Score") +
            scale_color_manual(values = color_tab3[num_lv:2])
        }

      tab3_plots[[i]] <- list(p_page1 = p_page1, p_page2 = p_page2, p_page3 = p_page3)

    }

  }
  tab3$tab3_plots <- tab3_plots

  return(tab3)
}
