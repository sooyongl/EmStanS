
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
#> 1    1   103.17 Level1
#> 2    2   110.81 Level1
#> 3    3   114.28 Level2
#> 4    4   125.35 Level1
#> 5    5   127.31 Level2
#> 6    6   136.14 Level2
#> 7    7   140.68 Level2
#> 8    8   145.43 Level2
#> 9    9   150.53 Level1
#> 10  10   153.89 Level3
#> 11  11   154.74 Level1
#> 12  12   158.33 Level1
#> 13  13   159.08 Level1
#> 14  14   168.03 Level1
#> 15  15   169.20 Level3
#> 16  16   169.65 Level1
#> 17  17   175.07 Level2
#> 18  18   183.81 Level2
#> 19  19   187.17 Level2
#> 20  20   189.09 Level2
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
#> 1   1   103.17      Level1         Level1   12   31 825.07 2623.61
#> 2   2   110.81      Level1         Level1   11   30 741.03 2394.41
#> 3   3   114.28      Level2         Level1   10   29 706.33 2293.78
#> 4   4   125.35      Level1         Level1   11   28 606.70 1983.82
#> 5   5   127.31      Level2         Level1   10   27 591.02 1930.90
#> 6   6   136.14      Level2         Level1   11   26 529.21 1701.32
#> 
#> $review
#>   OOD location Aligned_Lvl Operational_Lvl  item_status Cut_Score
#> 1   1   103.17      Level1          Level1   Consistent        NA
#> 2   2   110.81      Level1          Level1   Consistent        NA
#> 3   3   114.28      Level2          Level1 Inconsistent    168.03
#> 4   4   125.35      Level1          Level1   Consistent        NA
#> 5   5   127.31      Level2          Level1 Inconsistent    168.03
#> 6   6   136.14      Level2          Level1 Inconsistent    168.03
#>   Cut_Score_upper Lvl_Diff Distance Std. Abs. Distance
#> 1          168.03        0     0.00               0.00
#> 2          168.03        0     0.00               0.00
#> 3          229.02        1   -53.75              53.75
#> 4          168.03        0     0.00               0.00
#> 5          229.02        1   -40.72              40.72
#> 6          229.02        1   -31.89              31.89
```
