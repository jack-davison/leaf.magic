# Icon Mapping

Conveniently maps data values to icons for use in
[`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md),
analogous to
[`leaflet::colorFactor()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html),
[`leaflet::colorBin()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html)
and
[`leaflet::colorQuantile()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html).
Icons are, by their nature, discrete, so there is no analogue to
[`leaflet::colorNumeric()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html).
For
[`leaflet::colorBin()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html)
and
[`leaflet::colorQuantile()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html),
it would make sense to pick "ordered" icons such as the "hourglass" or
"temperature" icon families in Font Awesome.

## Usage

``` r
iconFactor(icons, domain, na.icon = "question")

iconBin(
  icons,
  domain,
  bins = length(icons),
  na.icon = "question",
  right = FALSE
)

iconQuantile(
  icons,
  domain,
  n = length(icons),
  probs = seq(0, 1, length.out = n + 1),
  na.icon = "question",
  right = FALSE
)
```

## Arguments

- icons:

  The icons that values will be mapped to. Note that the `library`
  (e.g., `"fontawesome"`) is defined in
  [`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md).

- domain:

  The possible values that can be mapped. For `iconFactor()` this should
  be categorical data. For `iconBin()` and `iconQuantile()`

- na.icon:

  The icon to return for missing or out-of-scope values.

- bins, right:

  Passed to [`cut()`](https://rdrr.io/r/base/cut.html).

- n:

  Number of equal-size quantiles desired. For more precise control, use
  the `probs` argument instead.

- probs:

  A more precise alternative to `probs`. Passed directly to
  [`quantile()`](https://rdrr.io/r/stats/quantile.html).

## Value

A function that takes a single parameter, `x`; when called with a vector
of factors/characters (for `iconFactor()`) or numbers (for `iconBin()`
and `iconQuantile()`), icon names are returned.
