# Extract Components from an ESS Object

\`extract.ESS()\` extracts selected components from an object of class
\`"ESS"\`. It provides a convenient interface for retrieving stored
Embedded Standard Setting results without directly indexing the internal
object structure.

## Usage

``` r
# S3 method for class 'ESS'
report(x, what = "all")
```

## Arguments

- x:

  An object of class \`"ESS"\` returned by \`emstans()\`.

- what:

  A character string indicating which component to extract. Available
  options are \`"all"\`, \`"individual"\`, \`"detailed"\`,
  \`"summary"\`, \`"review"\`, and \`"setup"\`. If \`what = "all"\`, all
  available extractable components are returned. Default is \`"all"\`.

- ...:

  Additional arguments passed to or from other methods. Currently not
  used.

## Value

An extracted component from \`x\`, depending on the value of \`what\`.
If \`what = "all"\`, a list of available ESS output components is
returned. If \`what\` is one of \`"individual"\`, \`"detailed"\`,
\`"summary"\`, \`"review"\`, or \`"setup"\`, the corresponding ESS
component is returned.

## Details

This function is intended for use after running \`emstans()\`. It is an
S3 method for objects of class \`"ESS"\`.
