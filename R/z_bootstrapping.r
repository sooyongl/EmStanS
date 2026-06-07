
#' Estimate bootstrapping ESS
#'
boot_ESS <- function(new_data, WESS, n_rep, b_prop, replace = T,
                     keep.sample = F, empirical = F, n_level) {
  # n_rep =  5; b_prop = 1 ; replace = T; keep.sample = F; empirical = F

  # n_level <- length(unique(new_data[[2]]))

  # res_boot <-
  #   rerun(n_rep,
  #         boot_single(new_data, WESS, n_level, b_prop, replace, empirical)) %>%
  #   bind_rows(., .id = "boot_rep")

  res_boot <- vector("list", n_rep)
  for(rrr in 1:n_rep) { # rrr = 1
    # res_boot[[rrr]] <- boot_single(new_data, WESS, n_level, b_prop, replace, empirical)

    temp0 <- tryCatch(
      boot_single(new_data, WESS, n_level, b_prop, replace, empirical),

      error = function(e) { "error" })

    if(is.character(temp0)) { next } else {res_boot[[rrr]] <- temp0}
  }
  res_boot <- res_boot[which(!unlist(map(res_boot, ~ is.null(.x))))]
  res_boot <- bind_rows(res_boot, .id = "boot_rep")

  return(res_boot)
}

#' Plots for bootstrapping results
boot_plot <- function(boot_res, est_data, selected_cp, information, font_size = 14) {
  geom.text.size = font_size * (5/14)

  a1 <- 200 # nrow(est_data)
  WESS <- information$base_data$WESS
  loc_nm <- information$base_data$loc_nm
  ald <- information$base_data$target_nm
  n_level <- nrow(information$data_ready$level_nm)

  # n_level <- sort(as.numeric(unique(str_extract(est_data[[ald]], "[0-9]"))))
  # if(WESS) {
  #   level_vars <-
  #     sapply(2:length(n_level), function(ii) paste0("L",n_level[ii], "_W"))
  # } else {
  #   level_vars <-
  #     sapply(2:length(n_level), function(ii) paste0("L",n_level[ii], "_C"))
  # }

  # n_level <- 2:length(unique(est_data[[ald]]))
  n_level <- 2:n_level
  if(WESS) {
    level_vars <- paste0("L",n_level, "_W")
  } else {
    level_vars <- paste0("L",n_level, "_C")
  }

  original_info <-
    est_data %>%
    select(all_of(loc_nm), all_of(level_vars)) %>%
    gather("lv_nm", "y", -all_of(loc_nm)) %>%
    set_names(c("x", "lv_nm", "y"))

  knots <- ifelse(nrow(original_info) > 20, 10,
                  round(nrow(original_info)/2, 0))
  inp_vars <- names(original_info)
  inp_formula <- glue("y ~ s(x, k = {knots})")

  original_pred <-
    original_info %>%
    group_split(lv_nm) %>%
    # map(., ~ gam(y ~ s(x),
    #              data = .x, family = "gaussian")
    # ) %>%
    map(., ~ gam(formula(inp_formula), data = .x, family = "gaussian")) %>%

    map(., ~ predict_gam(.x, length_out = a1)) %>%
    bind_rows(.id = "id") %>%
    left_join(
      tibble(lv_nm = level_vars) %>%
        mutate(id = as.character(row_number())),
      by = "id"
    )

  OLV <- paste0("Level",str_extract(level_vars, "[0-9]"))

  original_cut <-
    est_data %>%
    slice(selected_cp) %>%
    filter(Operational_Lv %in% OLV) %>%
    select(all_of(loc_nm), all_of(level_vars)) %>%
    gather("lv_nm","y", -all_of(loc_nm)) %>%
    set_names(c("x", "lv_nm", "y"))
  original_cut <- original_cut %>% group_split(lv_nm)
  original_cut <-
    map(1:length(original_cut), ~ original_cut[[.x]][.x, ]) %>%
    bind_rows()

  boot_scaling <-
    boot_res %>%
    group_by(lv_nm) %>%
    summarise(sd_y = sd(y),
              mean_y = mean(y)) %>%
    left_join(original_cut %>% set_names(c("xx","lv_nm","yy")), by = "lv_nm")

  boot_scaled <-
    boot_res %>%
    left_join(boot_scaling, by = "lv_nm") %>%
    mutate(
      y = ((y - mean_y) / sd_y) + yy
    )

  boot_cut <- boot_scaled %>%
    group_by(lv_nm) %>%
    summarise(x_avg = mean(x),
              y_avg = mean(y))


  p1 <- original_info %>%
    ggplot(aes(x = !!as.name("x"), y = !!as.name("y"),
                      colour = !!as.name("lv_nm"))) +
    geom_point(alpha = 0.2) +

    geom_line(data = original_pred,
              aes(x = x, y = fit, colour = lv_nm)) +

    geom_point(data = original_cut,
               aes(x = x, colour = lv_nm, fill = lv_nm),
               y = -Inf, shape = 24, size = 5) +
    annotate(geom = "text",
             x = original_cut[["x"]],
             y = -Inf,
             colour = "grey50",
             label = glue::glue("bold({round(original_cut[['x']], 2)})"),
             parse = T,
             size = geom.text.size,
             vjust = -.5) +
    scale_color_brewer(palette = "Set1")

  p3 <- p1 +
    geom_point(data = boot_scaled,
               aes(x = x, y = y, colour = lv_nm),
               alpha = .8, size = 3,
               stroke = 2,
               shape = 21)

  p3 +
    annotate(
      geom = "point",
      x = boot_cut[[2]], y = boot_cut[[3]],
      colour = "orange", size = 5) +

    geom_text(
      data = boot_cut,
      aes(
        x = x_avg,
        y = y_avg,
        label = round(x_avg, 2),
        group = lv_nm
      ),

      colour = "grey20",
      size = geom.text.size,
      vjust = -1
    ) +
    labs(x = "location", y = "fit", color = "", fill = "") +
    theme_bw(base_size = font_size)

}

# Helper functions -----------------------------
#'
boot_single <- function(new_data, WESS, n_level, b_prop, replace, empirical) {
  # new_data; WESS; n_level; b_prop; replace; empirical
  inp_data <-
    new_data %>%
    sample_frac(., size = b_prop, replace = replace) %>%
    arrange(location)

  cut_data <- boot_cutScore(inp_data, WESS, n_level, empirical)

  return(cut_data)
}

#'
boot_cutScore <- function(inp_data, WESS, n_level, empirical) { #
  # cut_info <- new_runESS(new_data, d_alpha = 1, empirical =  empirical)
  #
  # a1 <- length(seq(min(inp_data[["location"]]), max(inp_data[["location"]])))
  a1 <- nrow(inp_data)

  cut_info <- boot_runESS(inp_data, n_level, d_alpha = 1, empirical =  empirical)

  # n_level <- sort(as.numeric(unique(str_extract(inp_data$ALD, "[0-9]"))))
  # if(WESS) {
  #   cut_res <- cut_info %>% select(matches("_W$"), location)
  #
  #   lv_nm <-
  #     sapply(2:length(n_level), function(ii) paste0("L",n_level[ii], "_W"))
  # } else {
  #   cut_res <- cut_info %>% select(matches("_C$"), location)
  #   lv_nm <-
  #     sapply(2:length(n_level), function(ii) paste0("L",n_level[ii], "_C"))
  # }

  if(WESS){
    cut_res <- cut_info %>% select(matches("_W$"), location)
    lv_nm <- paste0("L",(2):n_level, "_W")
  } else {
    cut_res <- cut_info %>% select(matches("_C$"), location)
    lv_nm <- paste0("L",(2):n_level, "_C")
  }

  lv <- names(cut_res)[which(names(cut_res) != "location")]
  # lv <- names(cut_res)[1:(n_level-1)]
  knots <- ifelse(nrow(cut_res) > 20, 10, round(nrow(cut_res)/2, 0))
  model_fit <- lapply(lv, function(x) {
    gam(formula(glue("{x} ~ s(location, k = {knots})")),
        data = cut_res, family = "gaussian")
  })

  pred_fit <- map(model_fit, ~ .x %>%
                    predict_gam(length_out = a1) %>%
                    select(1:3) %>%
                    set_names(c("location", "fit", "fit.se"))
                    )

  minimizer <- function(model_pred) {
    min_point <- which.min(model_pred$fit)
    y_point <- model_pred$fit[min_point]
    x_point <- model_pred$location[min_point]

    c(x_point, y_point)
  }

  res <-
    map(pred_fit, minimizer) %>%
    do.call('rbind', .) %>%
    data.frame() %>%
    set_names(c("x", "y")) %>%
    mutate(lv_nm = lv_nm) %>%
    mutate_if(is.numeric, round, 3)

  return(res)
}

#'
boot_runESS <- function(inp_data, n_level = 3, d_alpha = 1, SD = 1, empirical = F) {
  # n_level = 3; d_alpha = 1; SD = 1
  # inp_data, n_level, d_alpha = 1, empirical =  empirical

  # n_level0 <- sort(as.numeric(unique(str_extract(inp_data$ALD, "[0-9]"))))
  # temp <- max(n_level0) - n_level
  # n_level0 <- n_level0 -1
  # n_level0 <- n_level0[which(n_level0 > temp)]
  # n_level <- n_level0 + 1
  #
  # temp1 <- map(n_level,
  #              ~ essCW(inp_data, d_alpha, cut_level = .x, SD, empirical = empirical))

  temp1 <- map(2:n_level,
               ~ essCW(inp_data, d_alpha, cut_level = .x, SD, empirical = empirical))

  incon_default <- map(temp1, ~ .x[["incon_default"]]) %>%
    map2(.,
         2:n_level,
         # n_level,
         ~ .x %>% set_names("cut_score", paste0("L",.y,"_C"),paste0("L",.y,"_W"))) %>%
    bind_cols(.name_repair = "minimal")

  incon_default <-
    incon_default %>%
    select(-cut_score) %>%
    mutate(location = incon_default$cut_score)  %>%
    # select(paste0("L",n_level,"_C"), everything()) %>%
    select(all_of(paste0("L",2:n_level,"_C")), everything()) %>%
    data.frame()

  return(incon_default)
}
