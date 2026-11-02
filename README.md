
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
fake_data <-  generateMockDataSet(ngca = 3, 
               cor_val = 0.2,
               n = 30,
               nlevel = 3,
               ntable = 5,
               npanelist = 5,
               sdinp = 1,
               ecinp = 0,
               100,300)



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
#> 2 M1    Math    1         1 T1    M1.T1.1  M1_2    Level1
#> 3 M1    Math    1         1 T1    M1.T1.1  M1_3    Level1
#> 4 M1    Math    1         1 T1    M1.T1.1  M1_4    Level2
#> 5 M1    Math    1         1 T1    M1.T1.1  M1_5    Level2
#> # ℹ 2,245 more rows
#> 
#> $item_data
#> # A tibble: 90 × 7
#>   GCA   Domain Item_ID Total_points Point Order_of_Difficulty Loc_RP50
#>   <chr> <chr>  <chr>          <dbl> <dbl>               <int>    <dbl>
#> 1 M1    Prob   M1_1               1     1                   1     104.
#> 2 M1    Calc   M1_2               1     1                   2     115.
#> 3 M1    Prob   M1_3               1     1                   3     117.
#> 4 M1    Prob   M1_4               1     1                   4     133.
#> 5 M1    Prob   M1_5               1     1                   5     138.
#> # ℹ 85 more rows
#> 
#> $examineedata_data
#> # A tibble: 642 × 3
#>   score  freq Grade
#>   <int> <int> <chr>
#> 1    91     8 M1   
#> 2    92     6 M1   
#> 3    93     5 M1   
#> 4    94     3 M1   
#> 5    95     9 M1   
#> # ℹ 637 more rows

# Or you can use your own data structured by the following table.
```

``` r
res <- emstans(data = fake_data,
               grades = c(1, 2),
               location = "Loc_RP50",
               method = "weight",
               smooth = FALSE)
```

## Summary

``` r
summary(res)
#>     unit cut_level modal median average agreement weighted_kappa
#> 1 M1-All    Level2 138.4  153.9  152.06     0.633          0.260
#> 2 M1-All    Level3 267.8  225.0  224.72     0.633          0.260
#> 3 M2-All    Level2 164.5  186.0  188.54     0.700          0.466
#> 4 M2-All    Level3 305.4  297.4  281.12     0.700          0.466
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
