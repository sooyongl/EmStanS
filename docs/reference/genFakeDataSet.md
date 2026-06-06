# generate fake data

generate fake data

## Usage

``` r
genFakeDataSet(
  ngca,
  ntable,
  npanelist,
  cor_val,
  nlevel,
  sdinp = 1,
  ecinp = 0,
  n,
  ...
)
```

## Arguments

- ngca:

  a numeric indicating the number of GCA

- ntable:

  a numeric indicating the number of tables per GCA

- npanelist:

  a numeric indicating the number of panelists per table

- cor_val:

  a numeric indicating the correlation between locations and ALD

- nlevel:

  a numeric indicating the number of levels

- sdinp:

  a numeric indicating standard deviation adjusting ESS weights

- ecinp:

  a numeric for essencially consistent items

- n:

  a numeric for items

- ...:

  a vector of arguments for `runif`
