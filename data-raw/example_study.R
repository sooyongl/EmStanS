# Builds inst/extdata/example_study.xlsx, the simulated workbook used in the
# examples of ess_read_excel() and emstans(). All values are simulated with
# generateMockDataSet(); no operational data are used.
#
# Run from the package root:  source("data-raw/example_study.R")
# Needs the writexl package (not a dependency of EmStanS).

devtools::load_all()

set.seed(2026)
sim <- generateMockDataSet(ngca = 2, ntable = 2, npanelist = 3, cor_val = 0.7,
                           nlevel = 3, sdinp = 1, ecinp = 0, n = 30, 300, 500)

writexl::write_xlsx(
  list(Setup            = sim$setup,
       Panelists        = sim$panelist,
       Ratings          = sim$rating,
       ItemMetaData     = sim$item_data,
       ExamineeMetaData = sim$examineedata_data),
  path = "inst/extdata/example_study.xlsx"
)
