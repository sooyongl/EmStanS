
#' Estimate bootstrapping ESS
#'
#' @noRd
boot_ESS <- function(new_data, WESS, n_rep, b_prop, replace = T,
                     keep.sample = F, empirical = F, n_level) {
  # n_rep =  5; b_prop = 1 ; replace = T; keep.sample = F; empirical = F

  # n_level <- length(unique(new_data[[2]]))

  # res_boot <-
  #   rerun(n_rep,
  #         boot_single(new_data, WESS, n_level, b_prop, replace, empirical)) %>%
  #   bind_rows(., .id = "boot_rep")

  res_boot <- vector("list", n_rep)
  failures <- character(0)
  for(rrr in 1:n_rep) { # rrr = 1
    # res_boot[[rrr]] <- boot_single(new_data, WESS, n_level, b_prop, replace, empirical)

    temp0 <- tryCatch(
      boot_single(new_data, WESS, n_level, b_prop, replace, empirical),
      error = function(e) conditionMessage(e))

    if(is.character(temp0)) { failures <- c(failures, temp0); next } else {res_boot[[rrr]] <- temp0}
  }
  res_boot <- res_boot[which(!unlist(map(res_boot, ~ is.null(.x))))]
  if (!length(res_boot)) {
    stop("All bootstrap samples failed. First error: ", failures[1], call. = FALSE)
  }
  if (length(failures)) {
    warning(length(failures), " of ", n_rep, " bootstrap samples failed and were dropped.",
            call. = FALSE)
  }
  res_boot <- bind_rows(res_boot, .id = "boot_rep")

  return(res_boot)
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
    mgcv::gam(formula(glue("{x} ~ s(location, k = {knots})")),
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
