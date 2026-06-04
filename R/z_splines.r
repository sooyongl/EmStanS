#' @include EmStanS-package.r
NULL


#' Estimate a generalized additive model (GAM) for ESS cut scores
#'
splineFit <- function(data_inp, information = NULL) {
  # data_inp <- cut_scores %>% mutate(!!locnm := location[[locnm]])
  # data_inp <- tbl_data

  if(is.null(information)) {
    loc_name <- names(data_inp)[ncol(data_inp)]

    cs_candi <- data_inp[[loc_name]]

    # if(cs_candi[length(cs_candi)] - cs_candi[1] < 10) {
    #   a1 <- seq(cs_candi[1], cs_candi[length(cs_candi)], by = 0.01)
    #   a1 <- length(a1)
    # } else {
    #   a1 <- length(cs_candi[1]:cs_candi[length(cs_candi)])
    # }
    a1 <- 200

    knots <- ifelse(nrow(data_inp) > 20, 10, round(nrow(data_inp)/2, 0))
    inp_vars <- names(data_inp)
    container <- list()
    for(i in 1:(length(inp_vars)-1)){ # i = 1

      inp_formula <- glue("{inp_vars[i]} ~ s({loc_name}, k = {knots})")

      fit_w <- gam(formula(inp_formula), data = data_inp, family = "gaussian")

      pred_w <- predict_gam(fit_w, length_out = a1) %>% select(1:3)
      names(pred_w)[2:3] <- c("fit", "se.fit")
      pred_w$`Cut Level` <- inp_vars[i]

      min_c <- pred_w %>% filter(fit==min(fit)) # empirical_min

      gam_min <- tibble(x = min_c[[1]], y = min_c[[2]], `Cut Level` = inp_vars[i], row.names = NULL)

      container[[(i)]] <- list(pred_w = pred_w, gam_min = gam_min)
    }

    res_pred <-
      map(container, ~ .x$pred_w) %>%
      bind_rows() %>%
      set_names(c(loc_name, "Inconsistency", "se", "Cut Level")) %>%
      mutate_at(vars(matches(loc_name)), round, 0)

    res_min <-
      map(container, ~ .x$gam_min) %>%
      bind_rows()

    res_cut_score <-
      data_inp %>%
      gather("Cut Level", "fit", -!!as.name(loc_name)) %>%
      left_join(res_min, by = "Cut Level") %>%
      mutate(a1 = x - !!as.name(loc_name)) %>%
      filter(a1 <=0 ) %>%
      group_split(`Cut Level`) %>%
      map(., ~ .x %>% head(1)) %>%
      bind_rows() %>%
      rename("selected_cs" = all_of(loc_name)) %>%
      select(selected_cs, `Cut Level`)

    res_selected <-
      data_inp %>%
      gather("Cut Level", "fit", -!!as.name(loc_name)) %>%
      left_join(res_cut_score, by = "Cut Level") %>%
      group_by(`Cut Level`) %>%
      summarise(
        selected_cp = which(abs(!!as.name(loc_name) - selected_cs)  == 0)[1],
        .groups = "drop"
      )

    res <- list(gam_pred = res_pred,
                gam_min = res_min,
                gam_selected_cs = res_cut_score,
                gam_selected_cp = res_selected)

  } else {
    loc_name <- information$base_data$loc_nm
    WESS <- information$base_data$WESS

    a1 <- data_inp %>% pull(loc_name)
    a1 <- length(a1)

    if(WESS) {
      data_inp <- data_inp %>% select(all_of(loc_name), matches("^L[1-9]_W"))

    } else {
      data_inp <- data_inp %>% select(loc_name, matches("^L[1-9]_C$"))
    }

    knots <- ifelse(nrow(data_inp) > 20, 10, round(nrow(data_inp)/2, 0))
    inp_vars <- names(data_inp)
    container <- list()
    container0 <- list()
    for(i in 2:length(inp_vars)){ # i = 2
      inp_formula <- glue("{inp_vars[i]} ~ s({loc_name}, k = {knots})")

      fit_w <- gam(formula(inp_formula),
                   data = data_inp, family = "gaussian")

      pred_w <- predict_gam(fit_w, length_out = a1) %>% select(1:3)
      names(pred_w)[2:3] <- c("fit", "se.fit")

      pred_w$`Cut Level` <- inp_vars[i]

      container[[(i-1)]] <- pred_w
      container0[[(i-1)]] <- fit_w
    }
    res <- do.call('rbind', container)
    res0 <- container0
    names(res) <- c(loc_name, "Inconsistency", "se", "Cut Level")

    res <- list(gam_value = res, gam_fit = res0)
  }

  res
}

# Helper functions -----------------------------------------
#' Get predictions from a GAM model.
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' This function is from the superseded package tidymv. Please, use the tidygam
#' package instead.
#'
#' It returns a tibble with the predictions from all the terms in a [gam][mgcv::gam] or [bam][mgcv::bam] model.
#'
#' If you simply want to return a tibble with the predicted values of the
#' response/outcome variable based on all terms (minus excluded smooth terms),
#' set `type = "link"` (the default). Note that if `type = "link"`,
#' parametric terms cannot be excluded from the prediction, due to limitations
#' of `mgcv`. If you want to return a tibble with the predicted values of
#' the response/outcome variable for each term in the model separately, set
#' `type = "terms"`. This type can be helpful if you want more flexibility
#' in plotting.
#'
#' @param model A `gam` or `bam` model object.
#' @param exclude_terms Terms to be excluded from the prediction. Term names should be given as they appear in the model summary (for example, `"s(x0,x1)"`).
#' @param length_out An integer indicating how many values along the numeric predictors to use for predicting the outcome term (the default is `50`).
#' @param values User supplied values for specific terms as a named list. If the value is `NULL`, the first value of the term is selected (useful when excluding terms).
#' @param type Either `"link"` or `"terms"`. See Details below.
#'
#' @return A tibble with predictions from a [gam][mgcv::gam] or [bam][mgcv::bam] model.
#'
#' @examples
#' \dontrun{
#' library(mgcv)
#' set.seed(10)
#' data <- gamSim(4)
#' model <- gam(y ~ fac + s(x2) + s(x2, by = fac) + s(x0), data = data)
#'
#' # get predictions
#' p <- predict_gam(model)
#'
#' # get predictions excluding x0 (the coefficient of x0 is set to 0);
#' # setting the value for the excluded term to NULL with the argument 'values'
#' # reduces computation time
#' p_2 <- predict_gam(model, exclude_terms = "s(x0)", values = list(x0 = NULL))
#'
#' # get predictions with chosen values of x0
#'
#' p_3 <- predict_gam(model, values = list(x0 = c(0.250599, 0.503313, 0.756028)))
#'}
predict_gam <- function(model, exclude_terms = NULL, length_out = 50, values = NULL, type = "link") {
  n_terms <- length(model[["var.summary"]])

  term_list <- list()

  for (term in 1:n_terms) {
    term_summary <- model[["var.summary"]][[term]]
    term_name <- names(model[["var.summary"]])[term]

    if (term_name %in% names(values)) {
      new_term <- values[[which(names(values) == term_name)]]

      if (is.null(new_term)) {
        new_term <- model[["var.summary"]][[term]][[1]]
      }

    } else {
      if (is.numeric(term_summary)) {

        min_value <- min(term_summary)
        max_value <- max(term_summary)

        new_term <- seq(min_value, max_value, length.out = length_out)

      } else if (is.factor(term_summary)) {

        new_term <- levels(term_summary)

      } else {
        stop("The terms are not numeric or factor.\n")
      }
    }

    term_list <- append(term_list, list(new_term))

    names(term_list)[term] <- term_name
  }

  new_data <- expand.grid(term_list)

  if (type == "link") {
    predicted <- as.data.frame(mgcv::predict.gam(model, new_data, exclude = exclude_terms, se.fit = TRUE))

    predictions <- cbind(new_data, predicted)

    predictions <- tibble::as_tibble(predictions)
  } else if (type == "terms") {
    if (!is.null(exclude_terms)) {
      message("Type is 'terms': exclude_terms is ignored.")
    }
    predicted <- mgcv::predict.gam(model, new_data, type = "terms", se.fit = TRUE)

    predicted_fit <- tibble::as_tibble(predicted$fit)
    predicted_se <- tibble::as_tibble(predicted$se.fit)
    colnames(predicted_se) <- paste0(colnames(predicted_se), "_se")

    predictions <- dplyr::bind_cols(predicted_fit, predicted_se)
  } else {
    stop("The argument `type` has to be 'link' or 'terms'.")
  }

  return(predictions)
}

#' Calculate the outcome of GAM function given x
#' @param data a gam class
#' @param par a numeric value for x-axis value
calGamOutcome <- function(data, par = 1) {
  # data = gam_fit

  smooth <- data$smooth[[1]]

  x_name <- smooth$term
  inp_data <- data.frame(v1 = par, V2 = par)
  names(inp_data)[1] <- c(x_name)

  X <- mgcv::PredictMat(smooth, inp_data, 1)
  X <- append(1, X)

  beta <- data$coefficients
  y_value <- X %*% beta

  return(c(y_value))
}

#' Calculate the minimun value of GAM function
#' @param ess_fit A data frame of fitted ESS-Count or ESS-Weight results.
#' @param gam_fit A fitted GAM object, usually from `mgcv::gam()`.
#' @param loc_name A character string giving the column name of the cut-score location variable.
#'
#' @return A named numeric vector with the estimated minimum location `x` and value `y`.
#'
calGamMinimun <- function(ess_fit, gam_fit, loc_name) {
  # ess_fit = data_inp; gam_fit = fit_w
  cut_score <- ess_fit[[loc_name]]

  start_value0 <- mean(cut_score)

  range <- c(min(cut_score), max(cut_score))

  res_optim <- optim(
    par = start_value0,
    fn = calGamOutcome,
    method = "L-BFGS-B",
    data = gam_fit,
    lower = range[1], upper = range[2])

  c(x = res_optim$par, y  = res_optim$value)
}

extractgam <- function(gam_est) {

  a1 <-
    gam_est$gam_min %>%
    select(-x) %>%
    spread("Cut Level", "y") %>%
    mutate_all(round, 2)

  gam.def.cs <- a1 %>% select(!matches("_W"))
  gam.wei.cs <- a1 %>% select(matches("_W"))

  gam.cs <- list(default_gam_cs = gam.def.cs, weight_gam_cs = gam.wei.cs)

  a1 <-
    gam_est$gam_min %>%
    select(-y) %>%
    spread("Cut Level", "x") %>%
    mutate_all(round, 2)

  gam.def.cp <- a1 %>% select(!matches("_W"))
  gam.wei.cp <- a1 %>% select(matches("_W"))

  cp <- list(default_gam = gam.def.cp, weight_gam = gam.wei.cp)

  a2 <- gam_est$gam_selected_cp %>% spread('Cut Level', "selected_cp")

  gam.def.scp <- a2 %>% select(!matches("_W"))
  gam.wei.scp <- a2 %>% select(matches("_W"))

  scp <- list(default_gam = gam.def.scp, weight_gam = gam.wei.scp)

  list(cp = cp, scp = scp, gam.cs = gam.cs)
}
