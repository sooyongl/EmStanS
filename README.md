
# EmStanS

## Overview

This package aims to support Embedded Standard Setting, calculating
counts and weights.

## Install

``` r
devtools::install_github("sooyongl/EmStanS", dependencies = T)
```

## Usage

``` r
library(EmStanS)
```

``` r
# data <- read.csv(filePath)
fake_data <- genFakeDataSet(ngca = 3, 
               cor_val = 0.2,
               n = 30,
               nlevel = 3,
               ntable = 5,
               npanelist = 5,
               sdinp = 1,
               ecinp = 0,
               100,300)
#> New names:
#> New names:
#> • `score` -> `score...1`
#> • `freq` -> `freq...2`
#> • `Grade` -> `Grade...3`
#> • `score` -> `score...4`
#> • `freq` -> `freq...5`
#> • `Grade` -> `Grade...6`



fake_data
#> $setup
#> # A tibble: 3 × 7
#>   GCA   `Content Area` `Grade Group` `Number of Tables per Room` `Level Options`
#>   <chr> <chr>          <chr>         <chr>                       <chr>          
#> 1 M1    Math           1             5                           Level1, Level2…
#> 2 M2    Math           2             5                           Level1, Level2…
#> 3 M3    Math           3             5                           Level1, Level2…
#> # ℹ 2 more variables: SD <dbl>, EC <dbl>
#> 
#> $panelist
#> # A tibble: 75 × 3
#>   GCA   Panelist Table
#>   <chr> <chr>    <chr>
#> 1 M1    M1.T1.1  T1   
#> 2 M1    M1.T1.2  T1   
#> 3 M1    M1.T1.3  T1   
#> 4 M1    M1.T1.4  T1   
#> 5 M1    M1.T1.5  T1   
#> # ℹ 70 more rows
#> 
#> $rating
#> # A tibble: 2,250 × 8
#>   GCA   Subject Grade Round Table Panelist Item_ID ALD   
#>   <chr> <chr>   <chr> <dbl> <chr> <chr>    <chr>   <chr> 
#> 1 M1    Math    1         1 T1    M1.T1.1  M1_1    Level1
#> 2 M1    Math    1         1 T1    M1.T1.1  M1_2    Level2
#> 3 M1    Math    1         1 T1    M1.T1.1  M1_3    Level1
#> 4 M1    Math    1         1 T1    M1.T1.1  M1_4    Level2
#> 5 M1    Math    1         1 T1    M1.T1.1  M1_5    Level1
#> # ℹ 2,245 more rows
#> 
#> $item_data
#> # A tibble: 90 × 6
#>   GCA   Item_ID Total_points Point Order_of_Difficulty Loc_RP50
#>   <chr> <chr>          <dbl> <dbl>               <int>    <dbl>
#> 1 M1    M1_1               1     1                   1     101.
#> 2 M1    M1_2               1     1                   2     116.
#> 3 M1    M1_3               1     1                   3     116.
#> 4 M1    M1_4               1     1                   4     119.
#> 5 M1    M1_5               1     1                   5     126.
#> # ℹ 85 more rows
#> 
#> $examineedata_data
#> # A tibble: 229 × 9
#>   score...1 freq...2 Grade...3 score...4 freq...5 Grade...6 score...7 freq...8
#>       <int>    <int> <chr>         <int>    <int> <chr>         <int>    <int>
#> 1        87        5 M1               98        7 M2              127        5
#> 2        88        8 M1               99        9 M2              128        5
#> 3        89        3 M1              100        3 M2              129        6
#> 4        90        4 M1              101        4 M2              130        7
#> 5        91        6 M1              102        7 M2              131        8
#> # ℹ 224 more rows
#> # ℹ 1 more variable: Grade...9 <chr>

# Or you can use your own data structured by the following table.
```

``` r
res <- emstans(data = fake_data,
               tests = c(1,2),
               targets = "ALD",
               WESS = T,
               gamest = F,
               median = "modal",
               loc = "Loc_RP50",
               domain = "GCA")
```

## Summary

``` r
summary(res)
#> ESS output
#> ----------
#> Individual Cut:
#> # A tibble: 50 × 16
#>    GCA    Table Panelist Correlation  L2_p  L3_p L2_loc L3_loc  L2_C  L2_W  L3_C
#>    <fct>  <chr> <chr>          <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>
#>  1 M1-All T1    M1.T1.1        0.263     8    25   149.   242.     7 326.     10
#>  2 M1-All T1    M1.T1.2        0.126    11    26   162.   252     13 638.      8
#>  3 M1-All T1    M1.T1.3        0.331     6    15   137.   186.     7 249.     12
#>  4 M1-All T1    M1.T1.4        0.351     4    22   119.   228.     5  62.1    12
#>  5 M1-All T1    M1.T1.5        0.317     9    23   150.   230.     9 310.     10
#>  6 M1-All T2    M1.T2.1        0.175     8    28   149.   287.     7 325.      6
#>  7 M1-All T2    M1.T2.2        0.215    10    23   152.   230.    13 608.      6
#>  8 M1-All T2    M1.T2.3        0.248     8    25   149.   242.     7 263.      8
#>  9 M1-All T2    M1.T2.4        0.219    13    27   170    259.    11 385.      8
#> 10 M1-All T2    M1.T2.5        0.422     6    20   137.   216.     5 123.      9
#>     L3_W L_sum LW_sum num_item total_item
#>    <dbl> <dbl>  <dbl>    <dbl>      <dbl>
#>  1  393.    17   719.       30         60
#>  2  289     21   927.       30         60
#>  3  447.    19   696.       30         60
#>  4  490     17   552.       30         60
#>  5  433.    19   744.       30         60
#>  6  342.    13   667.       30         60
#>  7  294.    19   902.       30         60
#>  8  472     15   735.       30         60
#>  9  424.    19   809.       30         60
#> 10  453.    14   576.       30         60
#> # ℹ 40 more rows
#> 
#> Meidan Cut:
#> # A tibble: 12 × 6
#>    GCA    Table  L2_p L2_loc  L3_p L3_loc
#>    <fct>  <chr> <dbl>  <dbl> <dbl>  <dbl>
#>  1 M1-All T1        8   149.    23   230.
#>  2 M1-All T2        8   149.    25   242.
#>  3 M1-All T3       10   152.    23   230.
#>  4 M1-All T4        7   141     23   230.
#>  5 M1-All T5        8   149.    24   235.
#>  6 M1-All All       8   149.    24   235.
#>  7 M2-All T1        5   135.    22   266.
#>  8 M2-All T2        6   148.    19   221.
#>  9 M2-All T3        9   164.    23   271.
#> 10 M2-All T4        9   164.    24   273.
#> # ℹ 2 more rows
#> 
#> Modal Cut:
#> # A tibble: 12 × 15
#>    GCA    Table Correlation  L2_p  L3_p L2_loc L3_loc  L2_C  L2_W  L3_C  L3_W
#>    <fct>  <chr>       <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 M1-All T1          0.458     6    21   137.   228.     7 164      10 369. 
#>  2 M1-All T2          0.465     6    26   137.   252      3  73.1     6 332. 
#>  3 M1-All T3          0.173    10    23   152.   230.    15 641.      7 284  
#>  4 M1-All T4          0.372     7    25   141    242.     7 198.      8 368. 
#>  5 M1-All T5          0.404     5    26   126.   252      7 234.      6 196. 
#>  6 M1-All All         0.64      5    25   126.   242.     5 118.      3  87.4
#>  7 M2-All T1          0.413     6    24   148.   273.     5 258.      7 258. 
#>  8 M2-All T2          0.479     2    21   119.   252.     1   0       8 414. 
#>  9 M2-All T3          0.442     7    25   159.   273.     7 259.      8 257. 
#> 10 M2-All T4          0.376    10    28   182.   294.     7 379.      6 149. 
#>    L_sum LW_sum num_item total_item
#>    <dbl>  <dbl>    <dbl>      <dbl>
#>  1    17   533.       30         60
#>  2     9   405        30         60
#>  3    22   925.       30         60
#>  4    15   567.       30         60
#>  5    13   430        30         60
#>  6     8   205.       30         60
#>  7    12   516.       30         60
#>  8     9   414.       30         60
#>  9    15   515.       30         60
#> 10    13   528.       30         60
#> # ℹ 2 more rows
#> 
#> Average Cut:
#> # A tibble: 12 × 6
#>    GCA    Table  L2_p L2_loc  L3_p L3_loc
#>    <fct>  <chr> <dbl>  <dbl> <dbl>  <dbl>
#>  1 M1-All T1        8   143.    22   228.
#>  2 M1-All T2       10   152.    25   247.
#>  3 M1-All T3       10   158.    23   234.
#>  4 M1-All T4        7   144.    23   231.
#>  5 M1-All T5        8   148.    24   244.
#>  6 M1-All All       8   149.    24   237.
#>  7 M2-All T1        5   155.    22   264.
#>  8 M2-All T2        6   144.    20   225.
#>  9 M2-All T3        9   168.    22   258.
#> 10 M2-All T4        9   177.    25   275.
#> # ℹ 2 more rows
#> 
#> More details can be extacted using the `extract()` function
```

## Report

With `what = "all"`, the combined report is provided.

### Individual

``` r
report(res, what = "individual")
```

### Detailed

``` r
report(res, what = "detailed")
```

### Summary

``` r
report(res, what = "summary")
```

### Item Review

``` r
report(res, what = "review")
```

## Extract

`extract()` provides the primary results of ESS as a form of `list()`.

``` r
extract(res)
```
