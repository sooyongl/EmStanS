
# EmStanS

## Overview

This package aims to support Embedded Standard Setting, calculating
counts and weights.

This is a lite version. For the full version, contact Dan.Lewis@CreativeMeasurement.com.

See also Lewis, D., & Cook, R. (2020). Embedded standard setting: Aligning standard‐setting methodology with contemporary assessment design principles. Educational Measurement: Issues and Practice, 39(1), 8-21.

## Install

``` r
# devtools::install_github("CMS5000/EmStanS", dependencies = T)
devtools::install_github("sooyongl/EmStanS", dependencies = T)
```

## Usage

``` r
library(EmStanS)
#> Warning: replacing previous import 'ggplot2::last_plot' by 'plotly::last_plot'
#> when loading 'EmStanS'
```

``` r
# data <- read.csv(filePath)
fake_data <- genFakeData("runif",
                         cor_value = 0.6, 
                         nlevel = 3, 
                         n = 50, 
                         100, 300)

head(fake_data, 20)
#>    OOD location    ALD
#> 1    1   102.39 Level1
#> 2    2   105.58 Level1
#> 3    3   107.34 Level1
#> 4    4   112.21 Level2
#> 5    5   112.76 Level2
#> 6    6   115.12 Level1
#> 7    7   133.56 Level2
#> 8    8   136.25 Level2
#> 9    9   162.55 Level3
#> 10  10   166.39 Level2
#> 11  11   173.83 Level3
#> 12  12   181.07 Level2
#> 13  13   182.47 Level1
#> 14  14   185.38 Level2
#> 15  15   194.71 Level2
#> 16  16   197.44 Level2
#> 17  17   201.83 Level3
#> 18  18   204.47 Level2
#> 19  19   206.40 Level3
#> 20  20   209.80 Level2

# Or you can use your own data structured by the above table.
```

``` r
res <- EmStanS::emstans(
  data = fake_data, 
  lvname = c("Level1", "Level2", "Level3")
  )
#> Note. Input data must be ordered by OOD, location, and ALD

res
#> $ess_table
#>   OOD location Aligned_Lvl Operational_Lv L2_C L3_C   L2_W    L3_W
#> 1   1   102.39      Level1         Level1    6   29 213.22 2708.95
#> 2   2   105.58      Level1         Level1    5   28 197.27 2619.63
#> 3   3   107.34      Level1         Level1    4   27 190.23 2572.11
#> 4   4   112.21      Level2         Level1    3   26 175.62 2445.49
#> 5   5   112.76      Level2         Level1    4   25 174.52 2431.74
#> 6   6   115.12      Level1         Level1    5   24 172.16 2375.10
#> 
#> $review
#>   OOD location Aligned_Lvl Operational_Lvl  item_status Cut_Score
#> 1   1   102.39      Level1          Level1   Consistent        NA
#> 2   2   105.58      Level1          Level1   Consistent        NA
#> 3   3   107.34      Level1          Level1   Consistent        NA
#> 4   4   112.21      Level2          Level1 Inconsistent    133.56
#> 5   5   112.76      Level2          Level1 Inconsistent    133.56
#> 6   6   115.12      Level1          Level1   Consistent        NA
#>   Cut_Score_upper Lvl_Diff Distance Std. Abs. Distance
#> 1          133.56        0     0.00               0.00
#> 2          133.56        0     0.00               0.00
#> 3          133.56        0     0.00               0.00
#> 4          254.82        1   -21.35              21.35
#> 5          254.82        1   -20.80              20.80
#> 6          133.56        0     0.00               0.00
```

## Launch the Shiny app

``` r
# Launch Shiny app on the local computer
EmStanS::launchEmStanS(local = T)

# Launch Shiny app on the server
# Currently not available
EmStanS::launchEmStanS(local = F)
```
