#' Calculate counts and weights across all levels
#'
calCountWeight <- function(new_data, n_cut = 3, d_alpha = 1, SD = 1, empirical = T, EC = 0) {

  new_data <- new_data %>%
    mutate(ALD = as.numeric(str_remove(ALD, "Level")))

  temp1 <- map(2:n_cut,
               ~ .calCW(new_data, d_alpha, cut_level = .x, SD, EC = EC, empirical = empirical))

  incon_default <- map(temp1, ~ .x[["incon_default"]]) %>%
    map2(., 2:n_cut,~ .x %>% set_names("cut_score", paste0("L",.y, "_C"),paste0("L",.y,"_W"))) %>%
    bind_cols(.name_repair = "minimal") %>%
    select(-cut_score) %>%
    select(paste0("L",2:n_cut,"_C"), everything()) %>%
    data.frame()

  return(incon_default)
}

#' Calculate counts and weights for a sinlge level
#'
.calCW <- function(new_data, d_alpha = 1, cut_level = 2, SD = 1, empirical = T, EC = 0) {

  new_data$OOD <- 1:dim(new_data)[1]

  if(empirical) {
    cut_candi <- new_data$location
  } else {
    cut_candi <- seq(min(new_data$location), max(new_data$location), by = 1)
  }

  cut_candi_id <- rep(1:length(cut_candi), each = dim(new_data)[1])
  cut_candi_list <- rep(cut_candi, each = dim(new_data)[1])

  dt <- data.table(new_data)
  dt.expanded <- dt[ ,list(freq=1:length(cut_candi)), by=names(dt)][order(freq, OOD)][ ,freq := NULL]

  dt.expanded[, cut_score := cut_candi_list]
  dt.expanded[, cut_score_id := cut_candi_id]

  incon_default <- data.frame(dt.expanded)

  incon_default <- incon_default %>% data.table() %>% .[, above_ecd := abs(location - cut_score)^d_alpha >= 0]
  incon_default[, below_ecd := abs(location - cut_score)^d_alpha >= 0]

  incon_default[, above_c := location < cut_score & ALD >= cut_level]
  incon_default[, below_c := location >= cut_score & ALD < cut_level]

  incon_default[above_c == T, above_d := abs(location - cut_score)^d_alpha]
  incon_default[below_c == T, below_d := abs(location - cut_score)^d_alpha]

  incon_default <- incon_default[, .(
    above_c = sum(above_c),
    below_c = sum(below_c),
    above_w = sum(above_d, na.rm = T),
    below_w = sum(below_d, na.rm = T)), by=.(cut_score_id)]

  incon_default <- incon_default[, .(
    cut_score = cut_candi,
    counts = above_c + below_c,
    weights = above_w + below_w)
  ]

  return(list(incon_default = incon_default))
}
