# data_ready

data_ready

## Usage

``` r
data_ready(imprt_data)
```

## Arguments

- imprt_data:

  a list of imported data using
  [`read_data`](https://sooyongl.github.io/EmStanS/reference/read_data.md)

## Value

a list of the data ready for the analysis.

## Examples

``` r
if (FALSE) { # \dontrun{
require(embededss)
filePath <- "data/freq_data.xlsx"
imported_data <- read_data(filePath)
data_list <- data_ready(imported_data)
} # }
```
