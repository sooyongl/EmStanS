# Extract Components from an ESS Object

\`extract.ESS()\` extracts selected results from an object of class
\`"ESS"\`. The function is an S3 method for ESS objects and allows users
to retrieve specific components of the Embedded Standard Setting output,
such as individual-level results, detailed results, summary tables,
review tables, or cut scores.

## Usage

``` r
# S3 method for class 'ESS'
extract(x, what = "all", ...)
```

## Arguments

- x:

  An object of class \`"ESS"\` returned by \`emstans()\`.

- what:

  A character string indicating which component to extract. Available
  options are \`"all"\`, \`"individual"\`, \`"detailed"\`,
  \`"summary"\`, \`"review"\`, and \`"cutscore"\`. If \`"all"\`, all
  available extractable components are returned. Default is \`"all"\`.

- ...:

  Additional arguments passed to or from other methods. Currently not
  used.

## Value

An object extracted from \`x\`, depending on the value of \`what\`. If
\`what = "all"\`, a list of available ESS output components is returned.
For other values of \`what\`, the corresponding ESS component is
returned.

## Details

This function provides a convenient interface for accessing stored
results in an ESS object without directly indexing the internal list
structure. It is intended to be used after running \`emstans()\`.
