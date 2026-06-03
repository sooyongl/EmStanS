#' #' #' @include 0_import.r
#' NULL
#'
#' #' generate fake data
#' #'
#' #' @param ngca a numeric indicating the number of GCA
#' #' @param ntable a numeric indicating the number of tables per GCA
#' #' @param npanelist a numeric indicating the number of panelists per table
#' #' @param cor_val a numeric indicating the correlation between locations and ALD
#' #' @param nlevel a numeric indicating the number of levels
#' #' @param sdinp a numeric indicating standard deviation adjusting ESS weights
#' #' @param ecinp a numeric for essencially consistent items
#' #' @param ... a vector of arguments for \code{runif}
#' genFakeDataSet <- function(ngca, ntable, npanelist, cor_val, nlevel, sdinp = 1, ecinp = 0, n, ...) {
#'   # ngca = 3; cor_val = 0.2; n = 100; nlevel = 3; input <- list(100,300);ntable = 3; npanelist = 5; sdinp = 1; ecinp = 0
#'   input <- list(...)
#'
#'   gca <- paste0("M",1:ngca)
#'
#'   adjustment0 <- 0
#'   setup_list <- vector("list", ngca)
#'   panelist_list <- vector("list", ngca * ntable * npanelist)
#'   rating_list <- vector("list", ngca * ntable * npanelist)
#'   itemmeta_list <- vector("list", ngca)
#'   examinee_list <- vector("list", ngca)
#'   i <- 1; j <- 1; k <- 1
#'   for(gca_inp in gca) { # gca_inp = gca[1]
#'
#'     gca.table <- paste0("T",1:ntable)
#'     sim_data <-
#'       genFakeData(
#'         fun = 'runif',
#'         cor_value = cor_val,
#'         nlevel = nlevel,
#'         n = n,
#'         input[[1]],
#'         input[[2]]) %>%
#'       mutate(location = round(location, 3)) %>%
#'       mutate(Item_ID = row_number(), .before = location) %>%
#'       mutate(Item_ID = paste0(gca_inp, "_", Item_ID) )
#'     sim_data$location <- sim_data$location + adjustment0
#'
#'     for(table_inp in gca.table) { # table_inp = gca.table[1]
#'
#'       gca.panel <- paste0(1:npanelist)
#'       for(panel_inp in gca.panel) { # panel_inp = gca.panel[1]
#'
#'         gca_id   <- gca_inp
#'         table_id <- table_inp
#'         panel_id <-paste(gca_inp,table_inp,panel_inp, sep = ".")
#'
#'         sim_data$ALD <- simALD(sim_data$location, cor_val, nlevel)
#'
#'
#'         panelist_list[[k]] <- genPanelist(sim_data, gca_inp, panel_id, table_id)
#'         rating_list[[k]] <- genRating(sim_data, gca_inp, panel_id, table_id)
#'
#'         k <- k + 1
#'       }
#'       j <- j + 1
#'     }
#'
#'     setup_list[[i]] <- genSetup(sim_data, gca_inp, table_inp, sdinp, ecinp)
#'     itemmeta_list[[i]] <- genItemMetaData(sim_data, gca_inp)
#'     examinee_list[[i]] <- genExamineeData(sim_data, gca_inp)
#'
#'     adjustment0 <- adjustment0 + ceiling(runif(1, 10, 40))
#'     i <- i + 1
#'   }
#'
#'   setup <- setup_list %>% bind_rows()
#'   panel <- panelist_list %>% bind_rows()
#'   rating <- rating_list %>% bind_rows()
#'   itemdata <- itemmeta_list %>% bind_rows()
#'   examineedata <- examinee_list %>% Reduce(make_examinee_data, .)
#'
#'   list(
#'     setup = setup,
#'     panelist = panel,
#'     rating = rating,
#'     item_data = itemdata,
#'     examineedata_data = examineedata
#'   )
#' }
#'
#'
#'
#' # Helper function -------------------------------------------
#' #' Generate a single data set containing ALD, OOD, and location given a correlation value
#' genFakeData <- function(fun, cor_value, nlevel, ...) {
#'
#'   location <- genLoc(fun, ...)
#'   new_ald <- simALD(location, cor_value, nlevel)
#'   fake_data <- bind_cols(location = location,
#'                          OOD = 1:length(location),
#'                          ALD = new_ald)
#'
#'   fake_data
#' }
#'
#'
#' #' generate theta
#' genTheta <- function(theta, mean_vec, cor_mat) {
#'
#'   stopifnot( length(mean_vec) == dim(cor_mat)[2] )
#'   stopifnot( eigen( cor_mat )$values > 0 )
#'
#'   shiftMean <- function(theta_mat, mean_vec){
#'     col_means <- colMeans(theta_mat)
#'     mean_centered <- theta_mat - matrix(rep(col_means, dim(theta_mat)[1]), nrow = dim(theta_mat)[1], byrow = T)
#'     theta_mat <- mean_centered + matrix(rep(mean_vec, dim(theta_mat)[1]), nrow = dim(theta_mat)[1], byrow = T)
#'     return(theta_mat)
#'   }
#'
#'   n_col <- ifelse(is.null(dim(theta)), 1, dim(theta)[2])
#'
#'   n <- ifelse(is.null(dim(theta)), length(theta), dim(theta)[1])
#'   k <- ncol(cor_mat)
#'   x <- matrix( rnorm(n*k), nc=k )
#'   x[,seq_len(n_col)] <- theta
#'
#'   y <- x %*% solve(chol(var(x))) %*% chol(cor_mat) # cor(y)
#'   y[,seq_len(n_col)] <- theta  #
#'
#'   y <- shiftMean(y, mean_vec)
#'
#'   return(y)
#' }
#'
#'
#' #' simulate ALD
#' simALD <- function(existing_theta, cor_value, nlevel) {
#'   given_correlation <- matrix(c(
#'     1, cor_value,
#'     cor_value, 1 ), 2, 2 )
#'   given_means <- c(mean(existing_theta), 0)
#'   new_theta <- genTheta(theta = existing_theta, mean_vec = given_means, cor_mat = given_correlation)
#'
#'   new_level <- cut(x = new_theta[,2], breaks = nlevel, labels = F)
#'   new_level[1] <- 1
#'
#'   return(new_level)
#' }
#'
#' #' Generate location values
#' genLoc <- function(fun, ...){
#'   existing_theta <- match.fun(fun)(...)
#'   existing_theta <- sort(existing_theta)
#'   existing_theta
#' }
#'
#'
#' #' Generate a setup data
#' genSetup <- function(sim_data, GCA, table_inp, sdinp, ecinp) {
#'
#'   ALD <- factor(paste0("Level",sim_data[["ALD"]]))
#'   ALD <- paste(levels(ALD), collapse = ", ")
#'
#'   tibble(
#'     GCA = GCA,
#'     `Content Area` = "Math",
#'     `Grade Group` = str_remove(GCA, "M"),
#'     `Number of Tables per Room` = str_remove(table_inp, "T"),
#'     `Level Options` = ALD,
#'     SD = sdinp,
#'     EC = ecinp
#'   )
#' }
#'
#' #' Generate a panelist data
#' genPanelist <- function(sim_data, gca_inp, panel_id, table_id) {
#'
#'   tibble(
#'     GCA = gca_inp,
#'     Panelist = panel_id,
#'     Table = table_id
#'   )
#' }
#'
#' #' Generate a rating data
#' genRating <- function(sim_data, gca_inp, panel_id, table_id) {
#'
#'   Item_ID <- sim_data[["Item_ID"]]
#'   ALD <- paste0("Level",sim_data[["ALD"]])
#'
#'   tibble(
#'     GCA = gca_inp,
#'     Subject = "Math",
#'     Grade = str_remove(gca_inp, "M"),
#'     Round = 1,
#'     Table = table_id,
#'     Panelist = panel_id,
#'     Item_ID = Item_ID,
#'     ALD = ALD
#'   )
#' }
#'
#' #' Generate an item meta data
#' genItemMetaData <- function(sim_data, GCA) {
#'   # GCA = gca_inp
#'   Item_ID <- sim_data[["Item_ID"]]
#'   location <- sim_data[["location"]]
#'   OOD <- sim_data[["OOD"]]
#'
#'   if(length(OOD) >= 100) {
#'
#'     n_ceiling = 4
#'     name_domain <- LETTERS[1:n_ceiling]
#'
#'     domains <- rep(name_domain, floor(length(OOD)/n_ceiling))
#'     domains <- append(domains, rep(name_domain[1], length(OOD) - length(domains)))
#'
#'     tibble(
#'       GCA = GCA,
#'       Domain = domains,
#'       Item_ID = Item_ID,
#'       Total_points = 1,
#'       Point = 1,
#'       Order_of_Difficulty = OOD,
#'       Loc_RP50 = location
#'     )
#'   } else {
#'
#'     tibble(
#'       GCA = GCA,
#'       Item_ID = Item_ID,
#'       Total_points = 1,
#'       Point = 1,
#'       Order_of_Difficulty = OOD,
#'       Loc_RP50 = location
#'     )
#'   }
#' }
#'
#' #' Generate an examinee data
#' genExamineeData <- function(sim_data, GCA) {
#'   location <- sim_data[["location"]]
#'   examinee_score_range<- round(c(min(location) - 0.25*sd(location) , max(location) + 0.25*sd(location)),0)
#'
#'   examinee_score <- examinee_score_range[1]:examinee_score_range[2]
#'   examinee_freq <- rpois(length(examinee_score), 5)
#'   examinee_grade <- GCA
#'
#'   tibble(
#'     score = examinee_score,
#'     freq = examinee_freq,
#'     Grade = examinee_grade)
#' }
#'
#' #
#' make_examinee_data <- function(a1, a2) {
#'
#'   if(nrow(a1) != nrow(a2)) {
#'
#'     diff_row <- nrow(a1) - nrow(a2)
#'     dummy_row <- rep(NA, abs(diff_row))
#'
#'     if(diff_row > 0) {
#'
#'       a_name <- names(a2)
#'       dummy_data <- tibble(!!a_name[1] :=  dummy_row)
#'       for(i in a_name[-1]){
#'         t_add <- tibble(
#'           !!i := dummy_row
#'         )
#'         dummy_data <- bind_cols(dummy_data, t_add)
#'       }
#'       a2 <- bind_rows(a2, dummy_data)
#'     } else {
#'       a_name <- names(a1)
#'       dummy_data <- tibble(!!a_name[1] :=  dummy_row)
#'       for(i in a_name[-1]){
#'         t_add <- tibble(
#'           !!i := dummy_row
#'         )
#'         dummy_data <- bind_cols(dummy_data, t_add)
#'       }
#'       a1 <- bind_rows(a1, dummy_data)
#'     }
#'     bind_cols(a1, a2)
#'   } else {
#'     bind_cols(a1, a2)
#'   }
#' }
