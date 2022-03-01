
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
data <- read.csv(filePath)
fit <- emstans(
  data = data, 
  lvname = c("Level1", "Level2", "Level3")
  )
```
