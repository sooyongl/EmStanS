# read_data

read_data

## Usage

``` r
read_data(filePath, sheets_name)
```

## Arguments

- filePath:

  a character for a file path.

- sheets_name:

  a list for names of excel sheets

## Value

a named list containing standard setting information (excel data with 5
sheets.)

## Examples

``` r
if (FALSE) { # \dontrun{
filePath <- "data/freq_data.xlsx"
imported_data <- read_data(filePath)
} # }
```
