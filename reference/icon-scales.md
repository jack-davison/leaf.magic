# Icon Mapping

Conveniently maps data values to icons for use in
[`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md).
These functions mirror
[`leaflet::colorFactor()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html),
[`leaflet::colorBin()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html)
and
[`leaflet::colorQuantile()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html):
given the same `domain`, `levels`, `bins`, `pretty`, `n`, `probs`,
`reverse` and `right` arguments, an icon function and a colour function
will split the data in exactly the same way. If `icons` and `palette`
have the same length (one per level/bin), each level therefore receives
a unique and consistent icon-colour pair. Icons are, by their nature,
discrete, so there is no analogue to
[`leaflet::colorNumeric()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html).
For `iconBin()` and `iconQuantile()`, it makes sense to pick "ordered"
icons such as the "hourglass" or "temperature" icon families in Font
Awesome.

## Usage

``` r
iconFactor(
  icons,
  domain,
  levels = NULL,
  ordered = FALSE,
  na.icon = "auto",
  reverse = FALSE
)

iconBin(
  icons,
  domain,
  bins = length(icons),
  pretty = TRUE,
  na.icon = "auto",
  reverse = FALSE,
  right = FALSE
)

iconQuantile(
  icons,
  domain,
  n = length(icons),
  probs = seq(0, 1, length.out = n + 1),
  na.icon = "auto",
  reverse = FALSE,
  right = FALSE
)
```

## Arguments

- icons:

  The icons that values will be mapped to, one per level or bin. Icons
  may be repeated. Note that the `library` (e.g., `"fontawesome"`) is
  defined in
  [`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md).

- domain:

  The possible values that can be mapped. For `iconFactor()` this should
  be categorical data (character or factor). For `iconBin()` and
  `iconQuantile()` this should be numeric. If `NULL`, levels/bins are
  calculated from the data each time the function is called, as in
  `{leaflet}`.

- levels:

  An alternate way of specifying levels; if specified, `domain` is
  ignored.

- ordered:

  If `TRUE` and `domain` needs to be coerced to a factor, treat it as
  already in the correct order.

- na.icon:

  The icon to return for missing or out-of-scope values. The default,
  `"auto"`, is drawn by
  [`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md)
  and
  [`addIconLegend()`](https://jack-davison.github.io/leaf.magic/reference/addIconLegend.md)
  as a question mark from whichever `library` they use. If the scale
  maps values to marker shapes instead, `"auto"` is drawn as `"none"`
  (the icon alone). `NA` draws no icon (or, for shapes, no marker) and
  leaves missing values out of legends.

- reverse:

  If `TRUE`, the order of `icons` is reversed.

- bins:

  Either a numeric vector of two or more unique cut points or a single
  number (greater than or equal to 2) giving the number of intervals
  into which the domain values are to be cut. Defaults to
  `length(icons)`.

- pretty:

  Whether to use the function
  [`pretty()`](https://rdrr.io/r/base/pretty.html) to generate the bins
  when `bins` is a single number. Note that when `pretty = TRUE`, the
  number of bins may not equal `bins`, and may therefore not equal the
  number of `icons`; a warning is given if so. Use `pretty = FALSE` to
  guarantee one bin per icon.

- right:

  Should the intervals be closed on the right (and open on the left)?
  Passed to [`cut()`](https://rdrr.io/r/base/cut.html). Note that, to
  match
  [`leaflet::colorQuantile()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html),
  `right` is ignored by `iconQuantile()` when `domain` is not `NULL`.

- n:

  Number of equal-size quantiles desired. For more precise control, use
  the `probs` argument instead. Defaults to `length(icons)`.

- probs:

  A more precise alternative to `n`. Passed directly to
  [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html).

## Value

A function that takes a single parameter, `x`; when called with a vector
of factors/characters (for `iconFactor()`) or numbers (for `iconBin()`
and `iconQuantile()`), icon names are returned.

## Matching icons to levels

Levels are ordered in the same way as
[`leaflet::colorFactor()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html):
factor levels if `domain` is a factor, the order of appearance if
`ordered = TRUE`, and otherwise sorted alphabetically. For `iconBin()`
and `iconQuantile()`, bins are ordered from lowest to highest. The first
icon is assigned to the first level, the second to the second, and so
on.

Colour palettes can interpolate between colours when there are more
levels than colours; icons cannot. If the number of icons differs from
the number of levels/bins, each level is given the icon at the
equivalent position along the list of icons (so some icons may be
repeated or skipped) and a warning is issued.

## Examples

``` r
types <- c("Urban Traffic", "Urban Industrial", "Urban Background")

# levels are sorted, as in leaflet::colorFactor()
iconPal <- iconFactor(c("house", "industry", "car"), types)
colorPal <- leaflet::colorFactor(c("#28A197", "#12436D", "#801650"), types)
data.frame(type = types, icon = iconPal(types), color = colorPal(types))
#>               type     icon   color
#> 1    Urban Traffic      car #801650
#> 2 Urban Industrial industry #12436D
#> 3 Urban Background    house #28A197

# numeric data
binPal <- iconBin(
  c("temperature-low", "temperature-half", "temperature-high"),
  domain = 0:30
)
binPal(c(2, 15, 28))
#> [1] "temperature-low"  "temperature-half" "temperature-high"
```
