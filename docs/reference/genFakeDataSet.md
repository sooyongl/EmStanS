# Generate a Fake Data Set for Embedded Standard Setting

\`genFakeDataSet()\` generates a simulated data set that follows the
input structure required by \`emstans()\`. The generated data can be
used to test functions in the EmStanS package, demonstrate the expected
data format, or run examples without using operational data.

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

  A numeric value indicating the number of grade clusters or GCAs to
  generate.

- ntable:

  A numeric value indicating the number of tables to generate within
  each GCA.

- npanelist:

  A numeric value indicating the number of panelists to generate within
  each table.

- cor_val:

  A numeric value indicating the intended correlation between item
  locations and aligned level descriptions.

- nlevel:

  A numeric value indicating the number of performance levels or aligned
  level-description categories.

- sdinp:

  A numeric value used to adjust the standard deviation of the values
  used in Weighted Embedded Standard Setting. Default is \`1\`.

- ecinp:

  A numeric value used to control essentially consistent items in the
  simulated item ratings. Default is \`0\`.

- n:

  A numeric value indicating the number of items to generate within each
  GCA.

- ...:

  Additional numeric arguments passed to \`runif()\`, typically the
  minimum and maximum score values used to generate item locations. For
  example, \`100, 300\` generates item locations between 100 and 300.

## Value

A list of simulated data frames formatted for use with \`emstans()\`.
The returned list contains test setup information, panelist information,
item ratings, item metadata, and student data.

## Details

This function is mainly intended for package testing, examples, and
demonstrations. The returned object mimics the data structure expected
by \`emstans()\`, allowing users to inspect the required format before
preparing their own data.

## Examples

``` r
if (FALSE) { # \dontrun{
fake_data <- genFakeDataSet(
  ngca = 3,
  ntable = 5,
  npanelist = 5,
  cor_val = 0.2,
  nlevel = 3,
  sdinp = 1,
  ecinp = 0,
  n = 30,
  100,
  300
)
} # }
```
