#' @include 2_tab1_function.r
NULL

#' Generate DataTable for detailed ESS results
#'
#' @param dataInp a data frame
tab2_table <- function(dataInp, information) {# dataInp <- dataUse_1

  WESS <- information$base_data$WESS
  gamest <- information$base_data$gamest
  loc_name <- information$base_data$loc_nm

  dataUse <- dataInp
  data_nm <- names(dataUse)
  ln_0 <- data_nm[get_which(data_nm,"ALD",1):(ncol(dataUse)-1)]
  ln <- sort(ln_0)
  level_name <- ln[!str_detect(ln, "_W")]
  olv <- c("Level1", str_replace(level_name, "L", "Level"))
  olv <- str_remove(olv, "_C")
  w_name <- ln[str_detect(ln, "_W")]
  sd_name <- str_replace(w_name, "_W", "_SW")
  SD <- information$data_ready$SD_data[unique(dataUse$GCA), "SD"]

  lv_order <- c(sapply(1:length(sd_name), function(x) { c(level_name[x], w_name[x], sd_name[x])}))

  new_order_name <- c( data_nm[1:get_which(data_nm,"ALD")], ln,data_nm[ncol(dataUse)])

  # Table setting
  dataUse <- dataUse %>% select(all_of(new_order_name))
  for(i in 1:length(sd_name)) {
    dataUse <-
      dataUse %>%
      mutate(!!w_name[i] := !!as.name(w_name[i]) * SD) %>%
      mutate(!!sd_name[i] := round(!!as.name(w_name[i]) / SD, 2) , .after = w_name[i])
  }

  levels <-
    foreach(pi = 1:length(level_name), .combine = 'c') %do% {
      glue::glue("th(colspan = 3, '{level_name[pi]}'),")

    } %>%
    paste(., collapse = "\n")
  orders <- paste0("'",paste(lv_order, collapse = "','"),"'")
  con_dt <- glue::glue(
    "container_dt = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'GCA'),
        th(rowspan = 2, 'Item_ID'),
        th(rowspan = 2, 'OOD'),
        th(rowspan = 2, '{loc_name}'),
        th(rowspan = 2, 'ALD'),
        {levels}
        th(rowspan = 2, 'Operational_Lv')
       ),
        tr(
          lapply(c({orders}), th)
        )
      )
    ))")

  data_nm <- names(dataUse)
  unselect <- get_which(data_nm,"GCA",1):get_which(data_nm,"Item_ID",-1)
  selected <- data_nm[-unselect]

  dataUse <- dataUse %>% select(all_of(selected))
  data_nm <- names(dataUse)

  colour_vals <- which(data_nm %in% ln)
  shading_vals <- if(WESS) {which(data_nm %in% w_name)} else { which(data_nm %in% level_name)}

  min_vals <- dataUse %>%
    select(all_of(ln)) %>%
    lapply(., min)

  oplv <- dataUse %>% pull(Operational_Lv)
  oplv_names <- olv

  kable.line <- c()
  for(oi in 1:length(oplv_names)){
    o_p <- which(oplv == oplv_names[oi])
    tryCatch({
      kable.line[oi] <- o_p[length(o_p)]
    }, error=function(e){}
    )
  }

  coloring <- function(x) {
    cell_spec(x, "html",
              background = ifelse(x == min(x),"#FFFE35", "transparent"))
  }
  dataUse$hiddenColumn <- 0
  dataUse$hiddenColumn[kable.line] <- 1

  dataUse %>%
    mutate_at(ln, list(coloring)) %>%
    mutate(Operational_Lv =
             cell_spec(Operational_Lv,
                       background =
                         eval(parse(text = gen_ifelse("Operational_Lv",oplv_names))))) %>%
    DT::datatable(.,
                  escape = F,
                  rownames= FALSE,
                  container = eval(parse(text = con_dt)),
                  options = list(
                    searching = FALSE,
                    pageLength = nrow(dataUse),
                    columnDefs = list(
                      list(visible=FALSE, targets=ncol(dataUse)-1),
                      list(width = '50px', targets = "_all")
                    ),
                    compact=TRUE
                  )
    ) %>%
    formatStyle(0:ncol(dataUse), valueColumns = "hiddenColumn",
                `border-bottom` = styleEqual(1, "dashed 1.5px")) %>%
    formatStyle(
      shading_vals,
      backgroundColor = "#e8eded",
      fontWeight = 'bold'
    ) %>%
    formatStyle(c(6, ncol(dataUse)-1), `border-left` = "solid 1px")

}

#' Generate DataTable for summary table
#'
#' @param dataInp a data frame
tab2_table_effpage <- function(dataInp) {
  # dataInp = eff_data
  dataInp <- dataInp %>%
    select(-ends_with("_p")) %>%
    mutate(
      Correlation = round(Correlation, 3)
    )

  dataUse_1 <- dataInp

  ln <- names(dataUse_1)[str_detect(names(dataUse_1), "_W")]
  ln <-
    str_split(ln, "_W") %>%
    map(., ~ .x[[1]]) %>%
    unlist() %>%
    str_replace(., "L", "Level")

  level_break <-
    foreach(lll = 1:length(ln), .combine = 'c') %do% {
      a1 <- ln[lll]
      glue::glue("'{a1}' = 2")
    } %>% paste(., collapse = ", ")


  level_break <- glue::glue('c(" " = 3, {level_break},"SUM" = 2)')

  dataUse_1 %>%
    knitr::kable(format = "html", escape = F,
                 align = 'c',
                 table.attr = "style='width:100%;'") %>%
    kable_styling(
      c("striped","condensed"),
      full_width = F,
      font_size = 16,
      stripe_color='#F7F7F7'
    ) %>%
    add_header_above(
      header = eval(parse(text =level_break)),
      extra_css = "border-top: 2px solid; border-top-color: black"
    ) %>%
    row_spec(1, bold = T, color = "black",
             extra_css = "border-bottom: 2px solid; border-bottom-color: black")

}

#' Generate DataTable for crosstab
#'
#' @param crosstabs a data frame
tab2_table_crosst <- function(crosstabs){
  # crosstabs <- ct_data

  # Crosstabs ------------------------------------
  ct_1 <- crosstabs[[1]]
  num_level <- ncol(ct_1)

  for(i in 1:(nrow(ct_1)-1)) {
    ct_1[i,i] <- cell_spec(ct_1[i, i], "html",
                           escape = F,
                           extra_css =
                             paste(paste('background-color', "#B0D0F9", sep = ': '), 'display: inline-block', 'text-align: left', 'padding: 5px', 'margin: 0px', sep = "; ")
    )
  }

  ct_1 <- ct_1 %>%
    mutate(".." := rownames(.),
           .before = 1) %>%
    mutate(
      "." := c("Operational Level", rep("", num_level-1)),
      .before = 1)

  rownames(ct_1) <- NULL

  cross_tab <-
    ct_1 %>%
    kable( format = "html",escape = FALSE
    ) %>%
    kable_styling(
      full_width = F,
      font_size = 14
    ) %>%
    collapse_rows(., columns = 1, valign = "middle") %>%
    column_spec(
      1,
      bold = T,
      width="10em"
    ) %>%
    column_spec(
      2,
      bold = T
    ) %>%
    row_spec(., 1:nrow(ct_1), extra_css = "line-height: 120%;") %>%
    add_header_above( c(" " = 2, "Aligned ALD" = num_level ))

  # Summary table ------------------------------------
  ct_2 <- crosstabs[[2]]
  ct_2$name <- c("Agree","Disagree","Adjacent Disagreement","Discrepant Disagreement", "Weighted Kappa")

  ct_2 <- ct_2 %>%
    mutate(
      name = cell_spec(x=name, format = "html",
                       extra_css = paste('display: inline-block','padding: 0px','margin: 0px','width: 200px',sep = "; ")
      ),
      values = cell_spec(x=values, format = "html",
                         extra_css = paste('display: inline-block','padding: 0px','margin: 0px','width: 100px',sep = "; ")
      ),
      perc = cell_spec(x=perc, format = "html",
                       extra_css = paste('display: inline-block','padding: 0px','margin: 0px','width: 100px',sep = "; ")
      )
    )

  names(ct_2) <- c(" ", "Freq", "Perc")

  summary_tab <-
    ct_2 %>%
    kable( format = "html",escape = FALSE, align=c('l',rep('c', 2))) %>%
    kable_styling(
      # c("striped"),
      full_width = F,
      font_size = 14
    ) %>%
    row_spec(0, align = 'c') %>%
    column_spec(1, width = "200px", border_right = T,
                background = "white", bold = T) %>%
    column_spec(2:3, width = "100px", bold = T) %>%
    row_spec(0:nrow(ct_2), extra_css = "border-bottom: .5px solid; border-bottom-color: black;")

  list(cross_tab, summary_tab)
}

gam_fitted_table <- function(gam_fitted, selected_cs) {

  gam_value <- gam_fitted$gam_value
  gam_fit <- gam_fitted$gam_fit

  temp_incon <- map2_dbl(gam_fit, selected_cs, ~ calGamOutcome(.x, .y) %>% round(2))
  temp_table <- data.frame(cbind(selected_cs, diag(temp_incon)))

  table_out <- gam_value %>%
    select(-se) %>%
    spread(`Cut Level`, Inconsistency) %>%
    mutate_all(round, 2)

  names(temp_table) <- names(table_out)
  table_out <- bind_rows(table_out, temp_table) %>% distinct(.keep_all = T)

  table_out %>%
    datatable(rownames = F)
}

# Helper functions ------------------------------------
#'
gen_ifelse <- function(x, lvNames) {
    colors <- getColors(pallete = "pastel1", rep_n = 2)
    colors <- colors[1:length(lvNames)]

    for(i in 1:length(lvNames)) {
      lvNames[i] <- glue::glue({ "'{lvNames[i]}'"  })
    }

    for(i in 1:length(colors)) {
      colors[i] <- glue::glue({ "'{colors[i]}'"  })
    }

    if_list <- list()
    for(i in 1:length(lvNames)){
      # i <- 1
      lvNames_1 <- lvNames[i]
      colors_1 <- colors[i]

      if(i < length(lvNames)){
        if_list[[i]] <-
          glue({
            "ifelse({x} == {lvNames_1}, {colors_1},"
          })
      } else {
        p1 <- paste( rep(")", (length(lvNames)-1)), collapse = " ")
        if_list[[i]] <- glue({ "{colors_1} {p1}" })
      }
    }
    return(paste(unlist(if_list), collapse = " "))
  }
