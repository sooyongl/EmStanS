library(htmltools)
library(bslib)

source_files <- fs::dir_ls("R")
for(i in 1:length(source_files)) { source(source_files[i])}


data_path <- fs::dir_ls("prerun/data/example_file")[1]


# Containers ---------------
input <- list()
output <- list()

# --------------------------
input$setups$datapath <- data_path


# Import data --------------
imprt_data <- if(is.null(data_path)) {
  genFakeDataSet(ngca      = input$ngca,
                 ntable    = input$ntable,
                 npanelist = input$npanelist,
                 cor_val   = input$cor_value,
                 nlevel    = input$nlevel,
                 sd        = input$sd,
                 ec        = input$ec,
                 n         = input$nitem,
                 input$dist_info1,
                 input$dist_info2
  )
} else {

  sheets_name <- input$setups$datapath %>% excel_sheets()

  read_data(filePath = input$setups$datapath, sheets_name = sheets_name)
}


# Validate Data file

validateData(imprt_data)


# Data ready for analysis

data_list <-
  # Full Version
  data_ready(imprt_data)


setup_data    <- data_list$setup_data
panelist_data <- data_list$panelist_data
rating_data   <- data_list$rating_data
item_data     <- data_list$item_data
examinee_data <- data_list$examinee_data

output$setting    <- print_setting(imprt_data)
output$itemtable1 <- print_itemtable1(imprt_data)
output$itemtable2 <- print_itemtable2(imprt_data)

class(output$setting)

print_setup <- function(output) {
  browsable(
    tagList(
      HTML(as.character(output$setting)),
      br(),
      HTML(as.character(output$itemtable1)),
      br(),
      HTML(as.character(output$itemtable2))
    )
  )
}



# Set Parameters -----------------------------------------------------

input$tests <-  as.character(setup_data$GCA)
input$targets <- "ALD"#
input$WESS = T
input$gamest = F
input$median = "modal"
input$loc <-  "RP67" # "RP50" #"Loc_RP50" #
# input$loc <-  "Loc_RP50"
input$domain <- c("GCA") # "GCA" ; "Domain"
# input$select_domain <- c("04")
# input$select_domain <- NA # "A" #as.character(sort(unique(item_data$Domain)))
input$select_domain <- NULL
input$font_size = 14

# Information ready ---------------------------------------------------

information <-
  get_data_info(
    data_list = data_list,
    gca       = input$tests,
    ald       = "ALD",
    location  = input$loc,
    WESS      = input$WESS,
    gamest    = input$gamest,
    domain    = input$domain,
    select_domain = input$select_domain,
    modal     = F
  )

output$information <- information
# Tab 1 --------------------------


tab0 <- #eventReactive(input$run_tab1, {
  # Estimate Cut Score and Cut Point
  # waitress$start()
  # information <- information
  gen_tab0(information = information)
#})

tab1 <- #eventReactive(input$run_tab1, {
  # information <- information()
  # tab0 <- tab0()
  gen_tab1(tab0, information)
# })

# Main output:
# individual res : tab1$res
# median: tab1$median_res_com
# mode: tab1$modal_res_com
# average: tab1$average_res_com


### item plot --------------------------------------------
dist_items_plots <- #eventReactive(input$font_size,{
  plot_dist_items(information, font_size = input$font_size)
# })

## Tab1 print
output$tab1_indi <- dt_table_out_indi(tab1$indi_table, table_options_new_1)

# modal or median cut score output
output$tab1_group_mode <- dt_table_out_mode(tab1$modal_table, table_options_new_2)

# modal or median cut score output
output$tab1_group_median <- dt_table_out_med(tab1$median_table, table_options_new_2)

# average cut score output
output$tab1_group_average <- dt_table_out_med(tab1$average_table, table_options_new_2)



print_tab1 <- function(output) {
  browsable(
    tagList(
      div(output$tab1_indi),
      br(),
      div(output$tab1_group_mode),
      br(),
      div(output$tab1_group_median),
      br(),
      div(output$tab1_group_average)
    )
  )
}

print_tab1(output)


## Tab 2: Obtain Data for Display (Cut Scores)-------------------------------
tab2 <- #eventReactive(input$run_tab1, {
  gen_tab2(tab1, information)
# })

# tab2$for_tab2_out$`R0-All`[[1]]$eff_data
# tab2$for_tab2_out$`R0-All`[[1]]$t_out
# tab2$for_tab2_out$`R0-All`[[1]]$crosst
# tab2$for_tab2_out$`R0-All`[[1]]$gam_fitted

## Tab2 Print -----
information <- information
tab2 <- tab2

loc_nm <- information$base_data$loc_nm
WESS_nm <- information$base_data$WESS

n.of.gca <- information$data_ready$id_list$GCA
n.of.tb <- rep(1, length(n.of.gca))

for_tab2_out <- tab2$for_tab2_out

# put the results into each output
# lapply(1:length(n.of.gca), function(vi) {
for(vi in 1:length(n.of.gca)){
  # vi = 1; vvi = 1
  in_num <- n.of.tb[vi]
  # lapply(1:in_num, function(vvi) {
  for(vvi in 1:in_num) {
    t_outname <- paste("t1", vi, vvi, sep = "_")
    p_outname <- paste("p1", vi, vvi, sep = "_")
    ct_outname <- paste("ct", vi, vvi, sep = "_")
    ct_outname_e <- paste("ct_e", vi, vvi, sep = "_")

    sum_outname <- paste("sum", vi, vvi, sep = "_")
    sum_outname_e <- paste("sum_e", vi, vvi, sep = "_")

    gam_outname <-  paste("gam", vi, vvi, sep = "_")

    dataUse_1 <- for_tab2_out[[vi]][[vvi]][["t_out"]]
    crosstabs <- for_tab2_out[[vi]][[vvi]][["crosst"]]
    crosstabs_ec <- for_tab2_out[[vi]][[vvi]][["crosst_ec"]]
    p1 <- for_tab2_out[[vi]][[vvi]][["p1"]]
    gam_fitted <- for_tab2_out[[vi]][[vvi]][["gam_fitted"]]

    plot_data <- for_tab2_out[[vi]][[vvi]][["plot_data"]]
    selected_cp <- for_tab2_out[[vi]][[vvi]][["selected_cp"]]
    selected_cs <- for_tab2_out[[vi]][[vvi]][["selected_cs"]]

    output[[t_outname]] <- tab2_table(dataUse_1,information)
    output[[p_outname]] <-
      p1 %>% ggplotly_render(plot_data, selected_cp, selected_cs, information)


    output[[ct_outname]] <- tab2_table_crosst(crosstabs)[[1]]
    output[[ct_outname_e]] <- tab2_table_crosst(crosstabs_ec)[[1]]
    output[[sum_outname]] <- tab2_table_crosst(crosstabs)[[2]]
    output[[sum_outname_e]] <- tab2_table_crosst(crosstabs_ec)[[2]]

    output[[gam_outname]] <- gam_fitted_table(gam_fitted, selected_cs)
  }
}




print_tab2 <- function(output) {

  gca_name <- output$information$data_ready$id_list$GCA

  tabs <- lapply(1:length(gca_name), function(i) {

    nav_panel(
      paste0(gca_name[i]),

      navset_tab(
        nav_panel("Table", output[[paste0("t1_", i, "_1")]]),
        nav_panel("Plot", output[[paste0("p1_", i, "_1")]]),
        nav_panel("Plot data", output[[paste0("gam_", i, "_1")]]),
        nav_panel(
          "Crosstab",
          HTML(as.character(output[[paste0("ct_", i, "_1")]]))
        ),
        nav_panel(
          "Crosstab eff",
          HTML(as.character(output[[paste0("ct_e_", i, "_1")]]))
        ),
        nav_panel(
          "Summary",
          HTML(as.character(output[[paste0("sum_", i, "_1")]]))
        )
      )
    )
  })

  do.call(navset_tab, tabs)

}

print_tab2(output)



## Tab 3: Cut Score Summary --------------------------------------
## Summary of Cut Scores and impact data -----------------
tab3 <- #eventReactive(input$run_tab1,{
  gen_tab3(tab1, information) %>%
  tab3_table_pagetb(.) %>%
  tab3_plots(.)

# tab3$eff_page

# })

## Tab3 print ------------
information <- information
tab3 <- tab3
effpage <- tab3$effpage
tab3_plot <- tab3$tab3_plot

gca_id <- information$data_ready$id_list$GCA
tab_id <- unique(str_split(gca_id, "-", simplify = T)[,2])

# put the results into each output
# lapply(1:length(tab_id), function(vi) {
for(vi in 1:length(tab_id)) {
  # vi = 1; vvi = 1
  table_outname <- paste("tab3table", vi, sep = "_")

  p1_outname <- paste("tab3p1", vi, sep = "_")
  p2_outname <- paste("tab3p2", vi, sep = "_")
  p3_outname <- paste("tab3p3", vi, sep = "_")

  output[[table_outname]] <- effpage[[vi]]

  output[[p1_outname]] <- tab3_plot[[vi]][[1]]
  output[[p2_outname]] <- tab3_plot[[vi]][[2]]
  output[[p3_outname]] <- tab3_plot[[vi]][[3]]
}


print_tab3 <- function(output) {

  gca_id <- information$data_ready$id_list$GCA
  tab_id <- unique(str_split(gca_id, "-", simplify = T)[,2])

  tabs <- lapply(1:length(tab_id), function(i) {

    nav_panel(
      paste0(tab_id[i]),

      navset_tab(
        nav_panel("Table",
                  HTML(as.character(output[[paste0("tab3table_", i)]]))
        ),
        nav_panel("Plot",
                  plotly::ggplotly(output[[paste0("tab3p1_", i)]]),
                  plotly::ggplotly(output[[paste0("tab3p2_", i)]]),
                  plotly::ggplotly(output[[paste0("tab3p3_", i)]])
        )

      )
    )
  })

  do.call(navset_tab, tabs)
}


print_tab3(output)



## Tab 4: Item Review Ready --------------------------------
tab4 <- #eventReactive(input$run_tab1,{
  gen_tab4(tab1, tab2, information)
# })


# tab4$for_tab4_out


## Tab4 print----------------
information <- information
tab4 <- tab4

GCA_split <-
  tab4$for_tab4_out %>%
  mutate(GCA = factor(GCA, levels = information$data_ready$id_list$GCA)) %>%
  group_split(GCA)

n.of.gca <- information$data_ready$id_list$GCA
n.of.tb <- rep(1, length(n.of.gca))

# put the results into each output
# lapply(1:length(n.of.gca), function(vi) {
for(vi in 1:length(n.of.gca)){
  # vi = 1; vvi = 1
  in_num <- n.of.tb[vi]
  # lapply(1:in_num, function(vvi) {
  for(vvi in 1:in_num) {

    review_outname <- paste("out4_", vi, vvi, sep = "_")
    tab4_split <- GCA_split[[vi]]
    maxRow <- nrow(tab4_split)
    tab4_out <- tab4_split %>%
      mutate_if(is.numeric, round,4)
    item_review <-
      datatable(tab4_out,
                escape = FALSE,
                selection = 'multiple',
                rownames = F,
                extensions = 'FixedHeader',
                # options = table_options_3(maxRow),
                filter = 'top'
      ) %>%
      formatStyle(1:nrow(tab4_out),
                  target= 'row',
                  lineHeight='70%')

    output[[review_outname]] <- item_review # tab4_table_review(GCA_split[[vi]])
  }
}


print_tab4 <- function(output) {

  n.of.gca <- information$data_ready$id_list$GCA
  n.of.tb <- rep(1, length(n.of.gca))


  tabs <- lapply(1:length(n.of.tb), function(i) {

    nav_panel(
      paste0(n.of.gca[i]),

      navset_tab(
        nav_panel("Table",
                  output[[paste0("out4__", i, "_1")]]))
    )
  })

  do.call(navset_tab, tabs)
}


print_tab4(output)





###############################################################################
###############################################################################
###############################################################################
## Refresh after Excluding items ----------------------------------------------
# Excluded items------------------------------------
# ex_name <- #reactive({
information <- information
tab4 <- tab4

if(input$update) {
  tab4 <- tab44()
}

for_tab4_out <- tab4$for_tab4_out

GCA_split <-
  for_tab4_out %>%
  mutate(GCA = factor(GCA, levels = information$data_ready$id_list$GCA)) %>%
  group_split(GCA)

n.of.gca <- information$data_ready$id_list$GCA
n.of.tb <- rep(1, length(n.of.gca))

a1 <- c()
for(i in 1:length(GCA_split)){
  ex_1 <- GCA_split[[i]]
  id_ <- str_replace_all(unique(ex_1[[1]]), "\\.", "_")

  ex_2 <- unlist(lapply(seq_len(nrow(ex_1)), function(i) {
    value = input[[paste0(id_, i)]]
    if (is.null(value)) NA else value
  }))

  a1 <- append(a1, ex_1[[2]][ex_2])
}
ex_name <-  a1[!is.na(a1)]
# })

output$excludedRows <- #renderPrint({
  ex_name[!duplicated(ex_name)]
# })

#
# Refresh-----------------------------
# information_new <- #eventReactive(input$update_tab4, {

data_list <- data_list
excluded <- ex_name

data_list$rating_data <-
  data_list$rating_data %>%
  filter(!Item_ID %in% excluded)

data_list$item_data <-
  data_list$item_data %>%
  filter(!Item_ID %in% excluded)

information_new <- get_data_info(
  data_list,
  gca = input$tests,
  ald = "ALD",
  location = input$loc,
  WESS = input$WESS,
  gamest = input$gamest,
  domain = input$domain,
  select_domain = input$select_domain,
  modal = F
)
# })
#

## Individual ESS ---------------------------------------------
tab0_new <- #eventReactive(input$run_tab1_new, {
  # waitress_new$start()
  # Obtain Operational Level for Individuals
  gen_tab0(information_new)
# })

tab1_new <- #eventReactive(input$run_tab1_new, {
  gen_tab1(tab0_new, information_new)
# })


#
## Detailed ESS Group Results -----------------------------
#
#
### Obtain Data for Display (Cut Scores)
tab2_new <- #eventReactive(input$run_tab1_new,{
  gen_tab2(tab1_new, information_new)
# })
#

# ## Cut Score Summary-----------------------------
#
## Summary of Cut Scores and impact data
tab3_new  <- #eventReactive(input$run_tab1_new,{
  gen_tab3(tab1_new, information_new) %>%
  tab3_table_pagetb() %>%
  tab3_plots()

# tab3
# })

#
## Item Review -----------------------------
#
### Item Review Ready
tab4_new <- #eventReactive(input$run_tab1_new,{
  gen_tab4(tab1_new, tab2_new, information_new)
# })

## Item Review Display


###########################################################################
## Boostrapping ---------------------------------------------
###########################################################################
input$boot_num <- 100

bootResults <- list()

tab1 <- tab1
tab2 <- tab2
information <- information

level_nm <- information$data_ready$level_nm

ald <- information$base_data$target_nm
loc_nm <- information$base_data$loc_nm
WESS <- information$base_data$WESS
levels <- if(WESS) { "_W$" }else { "_C$" }

n.of.gca <- selected_ids <- input$boot_id

gca <- unique(information$base_data$filtered_data[["GCA"]]) %>%
  sort() %>% as.character()

for(vi in 1:length(selected_ids)) {
  # vi = 1; selected_ids <- gca

  n_level <- length(level_nm[,vi])

  selected_id <- selected_ids[vi]
  gca_p <- which(gca == selected_id)
  selected_cp <- tab1$modal_selected_cp_all[[gca_p]]
  used_data <- tab2$for_tab2_out[[gca_p]][[1]]$t_out

  new_data <-
    used_data %>%
    select(all_of(loc_nm), all_of(ald)) %>%
    mutate(!!ald := as.numeric(str_remove(!!as.name(ald), "Level")))

  names(new_data)[1] <- "location"

  boot_res <-
    boot_ESS(
      new_data = new_data,
      WESS = WESS,
      n_rep = input$boot_num,
      b_prop = 1,
      replace = T,
      keep.sample = F,
      empirical = F,
      n_level = n_level)

  # bootResults$boot_p[[paste0(selected_id,"_boot_plot")]] <-
  bootResults$boot_p[[vi]] <-
    boot_plot(boot_res = boot_res,
              est_data = used_data,
              selected_cp = selected_cp,
              information = information,
              font_size = 18)

  # bootResults$table1[[paste0(selected_id,"_boot_table")]] <-
  bootResults$table1[[vi]] <-
    boot_res %>%
    select(-y) %>%
    spread("lv_nm","x") %>%
    select(-boot_rep)

  # bootResults$table2[[paste0(selected_id,"_boot_table1")]] <-
  bootResults$table2[[vi]] <-
    boot_res %>%
    select(-y) %>%
    spread("lv_nm","x") %>%
    select(-boot_rep) %>%
    describe() %>%
    select(-vars, -trimmed, -mad) %>%
    mutate_if(is.numeric, round, 2)

}

lapply(1:length(selected_ids), function(vi) {

  boot_p <- bootResults$boot_p[[vi]]
  table1 <- bootResults$table1[[vi]]
  table2 <- bootResults$table2[[vi]]

  selected_id <- selected_ids[vi]

  output[[paste0(selected_id,"_boot_graph")]] <- renderPlot({boot_p})
  output[[paste0(selected_id,"_boot_table")]] <- renderDT({
    table1 %>%
      datatable(caption =
                  tags$caption("Results of Bootstrapping draws",
                               style="color:black; font-size: 200%;"),
                options = list(
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'font-size':'120%', 'background-color': '#000', 'color': '#fff'});",
                    "}")
                ))
  })

  output[[paste0(selected_id,"_boot_table1")]] <- renderDT({
    table2 %>%
      datatable(caption =
                  tags$caption("Descriptive statistics of bootstrapping draws",
                               style="color:black; font-size: 200%;"),
                options = list(
                  dom = 't',
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'font-size':'120%','background-color': '#000', 'color': '#fff'});",
                    "}")
                ))
  })

})



###########################################################################
# Update Manual Cut -------------------------------------------------------
###########################################################################

## Obtain Data for Display (Cut Scores) -------------------
tab11 <- # eventReactive(input$update,{
  information <- information
tab0 <- tab0
tab1 <- tab1

n.of.gca <- information$data_ready$id_list$GCA
n.of.tb <- rep(1, length(n.of.gca))
manual_cp <-
  lapply(1:length(n.of.gca), function(vi) {
    # vi = 1 ; vvi = 1
    in_num <- n.of.tb[vi]
    unlist(
      lapply(1:in_num, function(vvi) {
        txt_outname <- paste("txt", vi, vvi, sep = "_")
        cutpo <- input[[txt_outname]]
        cutpoint_inp <- as.numeric(str_split(cutpo, ",")[[1]])
      })
    )
  })

update_tab1(tab0, tab1, information, manual_cp)
# })

tab22 <- #eventReactive(input$update, {
  update_tab2(tab11, information)
# })

## Output for Detailed ESS Group Results -------------------
# observeEvent(input$update,{
information <- information
tab2 <- tab22

loc_nm <- information$base_data$loc_nm
WESS_nm <- information$base_data$WESS

n.of.gca <- information$data_ready$id_list$GCA
n.of.tb <- rep(1, length(n.of.gca))

for_tab2_out <- tab2$for_tab2_out

# put the results into each output
lapply(1:length(n.of.gca), function(vi) {
  # vi = 1; vvi = 1
  in_num <- n.of.tb[vi]
  lapply(1:in_num, function(vvi) {

    t_outname <- paste("t1", vi, vvi, sep = "_")
    p_outname <- paste("p1", vi, vvi, sep = "_")
    ct_outname <- paste("ct", vi, vvi, sep = "_")
    ct_outname_e <- paste("ct_e", vi, vvi, sep = "_")

    sum_outname <- paste("sum", vi, vvi, sep = "_")
    sum_outname_e <- paste("sum_e", vi, vvi, sep = "_")

    gam_outname <-  paste("gam", vi, vvi, sep = "_")

    dataUse_1 <- for_tab2_out[[vi]][[vvi]][["t_out"]]
    crosstabs <- for_tab2_out[[vi]][[vvi]][["crosst"]]
    crosstabs_ec <- for_tab2_out[[vi]][[vvi]][["crosst_ec"]]
    p1 <- for_tab2_out[[vi]][[vvi]][["p1"]]
    gam_fitted <- for_tab2_out[[vi]][[vvi]][["gam_fitted"]]

    plot_data <- for_tab2_out[[vi]][[vvi]][["plot_data"]]
    selected_cp <- for_tab2_out[[vi]][[vvi]][["selected_cp"]]
    selected_cs <- for_tab2_out[[vi]][[vvi]][["selected_cs"]]

    output[[t_outname]] <- renderDT({tab2_table(dataUse_1,information)})
    output[[p_outname]] <- renderPlotly({
      p1 %>% ggplotly_render(plot_data, selected_cp, selected_cs, information)
    })

    output[[ct_outname]] <- renderText({tab2_table_crosst(crosstabs)[[1]]})
    output[[ct_outname_e]] <- renderText({tab2_table_crosst(crosstabs_ec)[[1]]})
    output[[sum_outname]] <- renderText({tab2_table_crosst(crosstabs)[[2]]})
    output[[sum_outname_e]] <- renderText({tab2_table_crosst(crosstabs_ec)[[2]]})

    output[[gam_outname]] <- renderDT({gam_fitted_table(gam_fitted, selected_cs)})
  })
})
# })
## cut score summary -----------------------------------------
tab33 <- #eventReactive(input$update,{
  # tab3 <-
  gen_tab3(tab11, information) %>%
  tab3_table_pagetb() %>%
  tab3_plots()

# tab3
# })
#
## Summary of Cut Scores and impact data Display
# observeEvent(input$update,{
information <- information
tab3 <- tab33
effpage <- tab3$effpage
tab3_plot <- tab3$tab3_plot

gca_id <- information$data_ready$id_list$GCA
tab_id <- unique(str_split(gca_id, "-", simplify = T)[,2])

# put the results into each output
lapply(1:length(tab_id), function(vi) {
  # vi = 1; vvi = 1
  table_outname <- paste("tab3table", vi, sep = "_")

  p1_outname <- paste("tab3p1", vi, sep = "_")
  p2_outname <- paste("tab3p2", vi, sep = "_")
  p3_outname <- paste("tab3p3", vi, sep = "_")

  output[[table_outname]] <- renderText({effpage[[vi]]})

  output[[p1_outname]] <- renderPlot({tab3_plot[[vi]][[1]]})
  output[[p2_outname]] <- renderPlot({tab3_plot[[vi]][[2]]})
  output[[p3_outname]] <- renderPlot({tab3_plot[[vi]][[3]]})

})
# })

#
## Item Review ---------------------------------------------
tab44 <- #eventReactive(input$update,{
  gen_tab4(tab11, tab22, information)
# })
#
## Item Review Display
# observeEvent(input$update,{
information <- information
tab4 <- tab44

GCA_split <-
  tab4$for_tab4_out %>%
  mutate(GCA = factor(GCA, levels = information$data_ready$id_list$GCA)) %>%
  group_split(GCA)

n.of.gca <- information$data_ready$id_list$GCA
n.of.tb <- rep(1, length(n.of.gca))

# put the results into each output
lapply(1:length(n.of.gca), function(vi) {
  # vi = 1; vvi = 1
  in_num <- n.of.tb[vi]
  lapply(1:in_num, function(vvi) {

    review_outname <- paste("out4_", vi, vvi, sep = "_")

    output[[review_outname]] <- renderDataTable({
      tab4_table_review(GCA_split[[vi]])
    })
  })
})
# })

