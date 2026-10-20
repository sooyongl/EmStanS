#' @include ess_diagnose.R
NULL

#' Run Embedded Standard Setting Analysis
#'
#' `emstans()` estimates cut scores with Embedded Standard Setting (ESS) from
#' panelists' item-level alignments and item locations. Each grade and round
#' is analysed separately: cut scores are estimated for every panelist and
#' for the modal alignment across panelists (per table and for all tables),
#' and the median and average of the panelist cuts are reported. For the
#' modal result of each grade, the function also reports agreement between
#' aligned and empirical levels, an item review, and impact data.
#'
#' @param filePath Path to an EmStanS Excel workbook (sheets Setup,
#'   Panelists, Ratings, ItemMetaData, ExamineeMetaData). Either `filePath`
#'   or `data` must be given.
#' @param data An object from [ess_data()] or [ess_read_excel()], or a list
#'   of data frames in the workbook's sheet order (setup, panelists, ratings,
#'   item data, examinee data).
#' @param grade Grades to analyse: `NULL` for all, a numeric vector of
#'   positions, or a character vector of grade names. Default `NULL`.
#' @param domain Item subsets to analyse: `"GCA"` for all items of each grade,
#'   `"Domain"` for each content domain separately, or both. Default `"GCA"`.
#' @param targets Name of the ratings column holding the aligned level.
#'   Default `"ALD"`.
#' @param loc Name of the item location column, such as `"RP67"` or
#'   `"RP50"`. Default `"RP67"`.
#' @param median Reserved; cut scores are always reported as modal, median
#'   and average.
#' @param select_domain With `domain = "Domain"`, the domains to analyse.
#'   `NULL` analyses every domain.
#' @param WESS `TRUE` for ESS-Weight (minimise the summed distance of
#'   inconsistent items), `FALSE` for ESS-Count (minimise their number).
#'   Default `TRUE`.
#' @param gamest If `TRUE`, cut scores are the minima of a GAM-smoothed
#'   inconsistency profile. Default `FALSE`.
#' @param font_size Font size for plots. Default `14`.
#' @param digits Decimal places in printed tables. Default `3`.
#'
#' @return An object of class `"ESS"`. Use [extract()] to get its tables,
#'   [report()] for an HTML report, [update()] to set cut scores manually and
#'   [bootstrapping()] for bootstrap cut scores.
#'
#' @examples
#' \dontrun{
#' res <- emstans(filePath = "study.xlsx", loc = "RP67")
#' res
#' extract(res, "cutscore")
#' report(res)
#'
#' # CSV input with custom column names and level labels
#' d <- ess_data(ratings = "ratings.csv", items = "items.csv",
#'               levels = c("Basic", "Intermediate", "Advanced"))
#' res <- emstans(data = d, loc = "RP50")
#' }
#' @export
emstans <- function(filePath = NULL,
                    data = NULL,
                    grade = NULL,
                    domain = "GCA",
                    targets = "ALD",
                    loc = "RP67",
                    median = "modal",
                    select_domain = NULL,
                    WESS = TRUE,
                    gamest = FALSE,
                    font_size = 14,
                    digits = 3) {

  if (is.null(filePath) && is.null(data)) {
    stop("Either `filePath` or `data` must be provided.", call. = FALSE)
  }
  columns <- if (!identical(targets, "ALD")) list(ratings = list(level = targets))

  d <- if (!is.null(data)) {
    if (inherits(data, "ess_data")) data else as_ess_data(data, columns = columns)
  } else {
    ess_read_excel(filePath, columns = columns)
  }

  settings <- list(loc = loc, method = if (isTRUE(WESS)) "weight" else "count",
                   gamest = isTRUE(gamest), domain = domain,
                   select_domain = select_domain, font_size = font_size,
                   digits = digits, source = filePath)

  units <- ess_units(d, loc, domain = domain, select_domain = select_domain, grades = grade)
  ess_compute(d, units, settings)
}

# Run every analysis step and assemble the ESS object.
ess_compute <- function(data, units, settings, manual = NULL) {
  est <- ess_estimate(data, units, settings$method, settings$gamest)
  if (!is.null(manual)) est <- apply_manual_cuts(est, units, manual)
  structure(list(
    data        = data,
    settings    = settings,
    units       = units,
    panelist    = est$panelist,
    modal       = est$modal,
    median      = est$median,
    average     = est$average,
    modal_items = est$modal_items,
    profiles    = lapply(est$modal_fits, `[[`, "profile"),
    agreement   = ess_agreement(est, units),
    review      = ess_item_review(est, units),
    impact      = ess_impact(est, units, data),
    manual      = manual
  ), class = "ESS")
}

# Replace the modal (all tables) cuts of some units with given cut scores.
# Each cut's page is the first item at or above the cut score; operational
# levels, count and weight follow from that page.
apply_manual_cuts <- function(est, units, manual) {
  for (uid in names(manual)) {
    u <- units[[uid]]; cs <- manual[[uid]]
    prof <- est$modal_fits[[uid]]$profile
    rows <- vapply(cs, function(x) {
      r <- which(u$location >= x)[1]
      if (is.na(r)) stop("Cut score ", x, " for `", uid, "` is above every item location.",
                         call. = FALSE)
      r
    }, integer(1))
    ks <- seq.int(2, u$n_levels)
    sel <- est$modal$unit == uid & est$modal$table == "All"
    m <- est$modal[sel, ]
    m <- m[order(m$level), ]
    m$page <- rows
    m$cut_score <- cs
    m$count <- vapply(seq_along(ks), function(j) prof[[paste0("L", ks[j], "_C")]][rows[j]], numeric(1))
    m$weight <- vapply(seq_along(ks), function(j) prof[[paste0("L", ks[j], "_W")]][rows[j]], numeric(1))
    est$modal[sel, ] <- m
    mi <- est$modal_items$unit == uid
    est$modal_items$operational[mi] <- operational_levels(sum(mi), rows)
  }
  est
}

# Level labels ------------------------------------------------------------------

# Label of `level` (integer) in `grade`, as an ordered factor over all labels
# so that tables and plots keep the level order.
level_label <- function(x, grade, level) {
  lv <- x$data$levels
  lab <- lv$label[match(paste(grade, level), paste(lv$grade, lv$level))]
  all_labels <- unique(lv$label[order(lv$level)])
  factor(lab, levels = all_labels)
}

# The unit's grade, for looking up labels from unit ids.
unit_grade <- function(x, unit) {
  vapply(unit, function(u) x$units[[u]]$grade, character(1))
}

# Tables for users ------------------------------------------------------------------
#
# All tables are long (one row per cut or item), with level labels.

cut_table <- function(x, df, who = c("table")) {
  out <- df
  out$level <- level_label(x, out$grade, out$level)
  names(out)[names(out) == "level"] <- "cut_level"
  out[order(match(out$unit, names(x$units)), out$cut_level), , drop = FALSE]
}

ess_tables <- function(x) {
  g <- function(unit) unit_grade(x, unit)
  panelist <- cut_table(x, x$panelist)
  modal    <- cut_table(x, x$modal)
  median   <- cut_table(x, x$median)
  average  <- cut_table(x, x$average)

  review <- x$review
  review$aligned <- level_label(x, g(review$unit), review$aligned)
  review$operational <- level_label(x, g(review$unit), review$operational)

  ag <- x$agreement
  ct <- ag$crosstab
  ct$operational <- level_label(x, g(ct$unit), ct$operational)
  ct$aligned <- level_label(x, g(ct$unit), ct$aligned)

  impact <- x$impact
  impact$level <- level_label(x, g(impact$unit), impact$level)

  list(panelist = panelist, modal = modal, median = median, average = average,
       agreement = ag$summary, crosstab = ct, review = review, impact = impact)
}

# Modal (all tables) cut scores per unit, named by the level each cut opens.
modal_cut_scores <- function(x) {
  m <- x$modal[x$modal$table == "All", ]
  out <- lapply(names(x$units), function(uid) {
    mu <- m[m$unit == uid, ]
    mu <- mu[order(mu$level), ]
    stats::setNames(mu$cut_score, as.character(level_label(x, x$units[[uid]]$grade, mu$level)))
  })
  stats::setNames(out, names(x$units))
}

# Profile of a unit's modal result with labelled columns, e.g. "M count".
labelled_profile <- function(x, unit) {
  u <- x$units[[unit]]
  prof <- x$profiles[[unit]]
  k <- as.integer(sub("^L([0-9]+)_[CW]$", "\\1", names(prof)))
  what <- ifelse(grepl("_C$", names(prof)), "count", "weight")
  names(prof) <- paste(as.character(level_label(x, u$grade, k)), what)
  cbind(data.frame(item_id = u$item_id, page = seq_along(u$item_id), location = u$location,
                   stringsAsFactors = FALSE), prof)
}
