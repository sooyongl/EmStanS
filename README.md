
# EmStanS

## Overview

This package aims to support Embedded Standard Setting, calculating
counts and weights.

## Install

``` r
devtools::install_github("sooyongl/EmStanS", dependencies = T)
```

The documentation is available at
[here](https://sooyongl.github.io/EmStanS/).

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
#> 1    1   100.98 Level1
#> 2    2   101.87 Level2
#> 3    3   103.68 Level2
#> 4    4   105.69 Level1
#> 5    5   116.04 Level1
#> 6    6   124.02 Level2
#> 7    7   124.55 Level2
#> 8    8   134.94 Level2
#> 9    9   135.53 Level2
#> 10  10   144.81 Level2
#> 11  11   145.70 Level2
#> 12  12   145.85 Level1
#> 13  13   146.89 Level2
#> 14  14   151.33 Level2
#> 15  15   152.63 Level2
#> 16  16   154.83 Level2
#> 17  17   155.57 Level2
#> 18  18   157.38 Level2
#> 19  19   161.37 Level3
#> 20  20   161.48 Level3
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
#> 1   1   100.98      Level1         Level1    5   27 165.82 1605.03
#> 2   2   101.87      Level2         Level1    4   26 162.26 1581.89
#> 3   3   103.68      Level2         Level1    5   25 156.83 1536.64
#> 4   4   105.69      Level1         Level1    6   24 152.81 1488.40
#> 5   5   116.04      Level1         Level2    5   23 142.46 1250.35
#> 6   6   124.02      Level2         Level2    4   22 142.46 1074.79
#> 
#> $review
#>   OOD location Aligned_Lvl Operational_Lvl  item_status Cut_Score
#> 1   1   100.98      Level1          Level1   Consistent        NA
#> 2   2   101.87      Level2          Level1 Inconsistent    116.04
#> 3   3   103.68      Level2          Level1 Inconsistent    116.04
#> 4   4   105.69      Level1          Level1   Consistent        NA
#> 5   5   116.04      Level1          Level2 Inconsistent        NA
#> 6   6   124.02      Level2          Level2   Consistent    116.04
#>   Cut_Score_upper Lvl_Diff Distance Std. Abs. Distance
#> 1          116.04        0     0.00               0.00
#> 2          221.98        1   -14.17              14.17
#> 3          221.98        1   -12.36              12.36
#> 4          116.04        0     0.00               0.00
#> 5          116.04       -1     1.00               1.00
#> 6          221.98        0     0.00               0.00
```

## Launch the Shiny app

``` r
# Launch Shiny app on the local computer
EmStanS::launchEmStanS(local = T)

# Launch Shiny app on the server
EmStanS::launchEmStanS(local = F)
```
