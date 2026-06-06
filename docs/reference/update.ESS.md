# Update Manual Cut Points in an ESS Object

Updates an existing \`ESS\` object by replacing or adding manually
specified cut points. Manual cut points are provided through \`...\` as
named numeric vectors, where each name identifies a model, domain,
scale, or other component used in the \`ESS\` object.

## Usage

``` r
# S3 method for class 'ESS'
update(output, ...)
```

## Arguments

- output:

  An object of class \`ESS\`.

- ...:

  Named numeric vectors of manual cut scores Each argument should be
  supplied in the form \`name = c(lower, upper)\` or \`name = c(cp1,
  cp2, ...)\`. For example, \`M1 = c(100, 200)\` specifies manual cut
  points for \`M1\`.

## Value

An updated object of class \`ESS\` containing the supplied manual cut
points.

## Details

The values supplied through \`...\` are captured internally as a named
list using \`list(...)\`. Therefore, all arguments in \`...\` should be
named. These names are used to match the provided manual cut points to
the corresponding elements of the \`ESS\` object.

## Examples

``` r
if (FALSE) { # \dontrun{
ess2 <- update.ESS(
  ess,
  M1 = c(100, 200),
  M2 = c(56, 170)
)
} # }
```
