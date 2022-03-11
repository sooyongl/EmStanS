
# EmStanS

## Overview

This package aims to support Embedded Standard Setting, calculating
counts and weights.

## Install

``` r
devtools::install_github("sooyongl/EmStanS")
```

The documentation is available at
[here](https://sooyongl.github.io/EmStanS/).

## Usage

``` r
library(EmStanS)
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
#> 1    1   101.61 Level1
#> 2    2   103.17 Level2
#> 3    3   104.01 Level1
#> 4    4   109.53 Level2
#> 5    5   111.37 Level1
#> 6    6   114.77 Level1
#> 7    7   115.06 Level2
#> 8    8   118.34 Level2
#> 9    9   119.21 Level1
#> 10  10   119.70 Level1
#> 11  11   121.12 Level2
#> 12  12   133.28 Level2
#> 13  13   133.61 Level2
#> 14  14   134.20 Level1
#> 15  15   136.54 Level3
#> 16  16   139.59 Level1
#> 17  17   142.39 Level1
#> 18  18   143.10 Level2
#> 19  19   143.27 Level1
#> 20  20   150.30 Level1
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
#> 1   1   101.61      Level1         Level1   15   42 654.22 2743.12
#> 2   2   103.17      Level2         Level1   14   41 632.38 2679.16
#> 3   3   104.01      Level1         Level1   15   40 621.46 2645.56
#> 4   4   109.53      Level2         Level1   14   39 555.22 2430.28
#> 5   5   111.37      Level1         Level1   15   38 534.98 2360.36
#> 6   6   114.77      Level1         Level1   14   37 500.98 2234.56
#> 
#> $review
#>   OOD location Aligned_Lvl Operational_Lvl  item_status Cut_Score
#> 1   1   101.61      Level1          Level1   Consistent        NA
#> 2   2   103.17      Level2          Level1 Inconsistent    139.59
#> 3   3   104.01      Level1          Level1   Consistent        NA
#> 4   4   109.53      Level2          Level1 Inconsistent    139.59
#> 5   5   111.37      Level1          Level1   Consistent        NA
#> 6   6   114.77      Level1          Level1   Consistent        NA
#>   Cut_Score_upper Lvl_Diff Distance Std. Abs. Distance
#> 1          139.59        0     0.00               0.00
#> 2          249.78        1   -36.42              36.42
#> 3          139.59        0     0.00               0.00
#> 4          249.78        1   -30.06              30.06
#> 5          139.59        0     0.00               0.00
#> 6          139.59        0     0.00               0.00
```

## Launch the Shiny app

``` r
# Launch Shiny app on the local computer
EmStanS::launchEmStanS(local = T)

# Launch Shiny app on the server
EmStanS::launchEmStanS(local = F)
```
