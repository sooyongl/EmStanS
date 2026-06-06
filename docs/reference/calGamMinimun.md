# Calculate the minimun value of GAM function

Calculate the minimun value of GAM function

## Usage

``` r
calGamMinimun(ess_fit, gam_fit, loc_name)
```

## Arguments

- ess_fit:

  A data frame of fitted ESS-Count or ESS-Weight results.

- gam_fit:

  A fitted GAM object, usually from \`mgcv::gam()\`.

- loc_name:

  A character string giving the column name of the cut-score location
  variable.

## Value

A named numeric vector with the estimated minimum location \`x\` and
value \`y\`.
