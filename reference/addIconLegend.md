# Add an Icon legend to a map

Add an Icon legend to a map, either by manually specifying icons,
labels, and colours, or by providing an icon scale function (from
[`iconFactor()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md),
[`iconBin()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
or
[`iconQuantile()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md))
in the same way as
[`leaflet::addLegend()`](https://rstudio.github.io/leaflet/reference/addLegend.html).
Useful in conjunction with
[`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md)
to communicate the meaning of icons/colours.

## Usage

``` r
addIconLegend(
  map,
  icons,
  labels,
  colors = "black",
  title = NULL,
  library = "fontawesome",
  position = c("topright", "bottomright", "bottomleft", "topleft"),
  layerId = NULL,
  className = "info legend",
  data = leaflet::getMapData(map),
  pal,
  values,
  na.label = "NA",
  labFormat = leaflet::labelFormat(),
  opacity = 1,
  group = NULL
)
```

## Arguments

- map:

  a map widget object created from
  [`leaflet()`](https://rstudio.github.io/leaflet/reference/leaflet.html)

- icons:

  Name of the icons, from the icon library defined by `library`. A full
  list of available icons can be found using
  [`fontawesome::fa_metadata()`](https://rstudio.github.io/fontawesome/reference/fa_metadata.html),
  <https://icons.getbootstrap.com/>, <https://ionic.io/ionicons> or
  <https://lucide.dev/icons/>. Should not be provided if `pal` is used.

- labels:

  Labels for each `icon`; should be the same length as `icons`. Labels
  are inserted as HTML, so may contain formatting such as
  `"PM<sub>2.5</sub>"`. If `pal` is used, labels are generated using
  `labFormat`, but can be overridden by providing `labels`.

- colors:

  Colours to use for each `icon`. If `length(colors) == 1L` it is
  recycled for all icons. Otherwise should be the same length as
  `icons`. If `pal` is used, `colors` may also be a colour scale
  function, such as one created using
  [`leaflet::colorFactor()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html),
  [`leaflet::colorBin()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html)
  or
  [`leaflet::colorQuantile()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html).

- title:

  the legend title; optional. If `pal` is used and `values` is a
  formula, the title defaults to the variable name in the formula, as in
  [`leaflet::addLegend()`](https://rstudio.github.io/leaflet/reference/addLegend.html).

- library:

  One of `"fontawesome"`, `"bootstrap"`, `"ionicons"`, or `"lucide"`
  defining the icon library of interest. Defaults to `"fontawesome"`.
  Alternatively, `"text"` shows `icon` as text, which is useful for
  numbered or lettered markers (e.g., `icon = 1:10`). Text is bold and
  shrinks to fit, so is best kept to around three characters. `"svg"`
  uses `icon` as raw SVG markup (e.g.,
  `'<svg viewBox="0 0 24 24">...</svg>'`), so any icon can be used.
  Parts of the SVG which use `currentColor` or have no fill of their own
  take on `iconColor`. Only use SVG markup you trust, as it is inserted
  directly into legends.

- position:

  position of control: `"topleft"`, `"topright"`, `"bottomleft"`, or
  `"bottomright"`.

- layerId:

  the layer id

- className:

  extra CSS classes to append to the control, space separated

- data:

  the data object from which the argument values are derived; by
  default, it is the `data` object provided to
  [`leaflet()`](https://rstudio.github.io/leaflet/reference/leaflet.html)
  initially, but can be overridden

- pal:

  An icon scale function created using
  [`iconFactor()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md),
  [`iconBin()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
  or
  [`iconQuantile()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md).
  If provided, `icons` should not be.

- values:

  The values used to generate icons from `pal`. Either a vector or a
  formula to be evaluated against `data` (e.g., `~type`). May be omitted
  for scales with a fixed `domain` or `levels`, except for
  [`iconQuantile()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
  without a `domain`.

- na.label:

  The legend label for missing `values`.

- labFormat:

  A function to format the labels derived from `pal` and `values`. See
  [`leaflet::labelFormat()`](https://rstudio.github.io/leaflet/reference/addLegend.html).

- opacity:

  The opacity of the legend icons, between `0` and `1`. Note that this
  defaults to `1`, rather than the `0.5` of
  [`leaflet::addLegend()`](https://rstudio.github.io/leaflet/reference/addLegend.html),
  so that icons are shown in their true colours.

- group:

  The group of a layer (e.g., the markers created using
  [`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md)).
  If provided, the legend is shown/hidden alongside that group, e.g.,
  using
  [`leaflet::addLayersControl()`](https://rstudio.github.io/leaflet/reference/addLayersControl.html).
  In Shiny, using `group` with
  [`leaflet::leafletProxy()`](https://rstudio.github.io/leaflet/reference/leafletProxy.html)
  requires the original map to already contain a grouped icon legend.

## Value

a
[leaflet](https://rstudio.github.io/leaflet/reference/leaflet-package.html)
widget

## Using icon scales

Like
[`leaflet::addLegend()`](https://rstudio.github.io/leaflet/reference/addLegend.html),
a legend can be generated automatically by passing an icon scale
function to `pal` alongside the `values` used on the map. The icons and
labels are then determined from the levels or bins of the scale, and
labels are formatted using `labFormat`. For example, an
[`iconBin()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
scale would be shown as one icon per bin, labelled with the range of
that bin.

In this case `colors` may also be a colour scale function, such as one
created using
[`leaflet::colorBin()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html)
with the same settings as the icon scale, giving a legend of matched
icon-colour pairs.

Legend items are derived exactly as in
[`leaflet::addLegend()`](https://rstudio.github.io/leaflet/reference/addLegend.html):
factor scales show the sorted unique `values`, bin scales show one item
per bin, and quantile scales show one item per quantile of `values`
(with the quantile values in a hover tooltip). An icon legend and a
colour legend built from scales with the same settings will therefore
always agree. Unlike
[`leaflet::addLegend()`](https://rstudio.github.io/leaflet/reference/addLegend.html),
`values` may be omitted when the scale has a fixed `domain`/`levels`,
and
[`iconBin()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
scales with no `domain` are supported. For the latter, the legend shows
only the bins that contain `values`, with the same icons (and colours,
if `colors` is a colour scale) as the map. Like
[`leaflet::colorBin()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html),
such a scale assigns icons based on which bins are occupied, so empty
bins have no icon to show.

If any `values` are missing, an extra legend item is added using the
`na.icon` of the scale and labelled with `na.label`. Its colour is the
`na.color` of `colors` if it is a colour scale, and otherwise grey
(`"#808080"`, the default `na.color` in `{leaflet}`). As in
[`leaflet::addLegend()`](https://rstudio.github.io/leaflet/reference/addLegend.html),
this is skipped if `na.label` is already one of the labels, if `na.icon`
is `NA`, or if `colors` is a colour scale with a transparent `na.color`.

## Examples

``` r
library(leaflet)
addIconLegend(
  map = leaflet(),
  icons = c("beer", "school"),
  labels = c("Pub", "University"),
  colors = c("red", "blue"),
  title = "Student Hangout"
)

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addControl","args":["<div style='margin-bottom:3px'><strong>Student Hangout<\/strong><\/div><svg preserveAspectRatio=\"none\" aria-hidden=\"true\" role=\"img\" viewBox=\"0 0 512 512\" style=\"height:1em;width:1em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:red;overflow:visible;position:relative;\"><path d=\"M32 64c0-17.7 14.3-32 32-32H352c17.7 0 32 14.3 32 32V96h51.2c42.4 0 76.8 34.4 76.8 76.8V274.9c0 30.4-17.9 57.9-45.6 70.2L384 381.7V416c0 35.3-28.7 64-64 64H96c-35.3 0-64-28.7-64-64V64zM384 311.6l56.4-25.1c4.6-2.1 7.6-6.6 7.6-11.7V172.8c0-7.1-5.7-12.8-12.8-12.8H384V311.6zM160 144c0-8.8-7.2-16-16-16s-16 7.2-16 16V368c0 8.8 7.2 16 16 16s16-7.2 16-16V144zm64 0c0-8.8-7.2-16-16-16s-16 7.2-16 16V368c0 8.8 7.2 16 16 16s16-7.2 16-16V144zm64 0c0-8.8-7.2-16-16-16s-16 7.2-16 16V368c0 8.8 7.2 16 16 16s16-7.2 16-16V144z\"/><\/svg>&nbsp;&nbsp;&nbsp;Pub<br><svg preserveAspectRatio=\"none\" aria-hidden=\"true\" role=\"img\" viewBox=\"0 0 640 512\" style=\"height:1em;width:1em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:blue;overflow:visible;position:relative;\"><path d=\"M337.8 5.4C327-1.8 313-1.8 302.2 5.4L166.3 96H48C21.5 96 0 117.5 0 144V464c0 26.5 21.5 48 48 48H256V416c0-35.3 28.7-64 64-64s64 28.7 64 64v96H592c26.5 0 48-21.5 48-48V144c0-26.5-21.5-48-48-48H473.7L337.8 5.4zM96 192h32c8.8 0 16 7.2 16 16v64c0 8.8-7.2 16-16 16H96c-8.8 0-16-7.2-16-16V208c0-8.8 7.2-16 16-16zm400 16c0-8.8 7.2-16 16-16h32c8.8 0 16 7.2 16 16v64c0 8.8-7.2 16-16 16H512c-8.8 0-16-7.2-16-16V208zM96 320h32c8.8 0 16 7.2 16 16v64c0 8.8-7.2 16-16 16H96c-8.8 0-16-7.2-16-16V336c0-8.8 7.2-16 16-16zm400 16c0-8.8 7.2-16 16-16h32c8.8 0 16 7.2 16 16v64c0 8.8-7.2 16-16 16H512c-8.8 0-16-7.2-16-16V336zM232 176a88 88 0 1 1 176 0 88 88 0 1 1 -176 0zm88-48c-8.8 0-16 7.2-16 16v32c0 8.8 7.2 16 16 16h32c8.8 0 16-7.2 16-16s-7.2-16-16-16H336V144c0-8.8-7.2-16-16-16z\"/><\/svg>&nbsp;&nbsp;&nbsp;University","topright",null,"info legend"]}]},"evals":[],"jsHooks":[]}
# use icon scales, as with leaflet::addLegend()
icons <- c("temperature-low", "temperature-half", "temperature-high")
iconPal <- iconBin(icons, domain = 0:30, bins = 3, pretty = FALSE)
colorPal <- colorBin("RdYlBu", domain = 0:30, bins = 3, pretty = FALSE,
                     reverse = TRUE)

leaflet() |>
  addIconLegend(
    pal = iconPal,
    values = c(2, 15, 28),
    colors = colorPal,
    title = "Temperature",
    labFormat = labelFormat(suffix = "&deg;C")
  )

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addControl","args":["<div style='margin-bottom:3px'><strong>Temperature<\/strong><\/div><svg preserveAspectRatio=\"none\" aria-hidden=\"true\" role=\"img\" viewBox=\"0 0 512 512\" style=\"height:1em;width:1em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#91BFDB;overflow:visible;position:relative;\"><path d=\"M448 96a32 32 0 1 0 -64 0 32 32 0 1 0 64 0zM320 96a96 96 0 1 1 192 0A96 96 0 1 1 320 96zM144 64c-26.5 0-48 21.5-48 48V276.5c0 17.3-7.1 31.9-15.3 42.5C70.2 332.6 64 349.5 64 368c0 44.2 35.8 80 80 80s80-35.8 80-80c0-18.5-6.2-35.4-16.7-48.9c-8.2-10.6-15.3-25.2-15.3-42.5V112c0-26.5-21.5-48-48-48zM32 112C32 50.2 82.1 0 144 0s112 50.1 112 112V276.5c0 .1 .1 .3 .2 .6c.2 .6 .8 1.6 1.7 2.8c18.9 24.4 30.1 55 30.1 88.1c0 79.5-64.5 144-144 144S0 447.5 0 368c0-33.2 11.2-63.8 30.1-88.1c.9-1.2 1.5-2.2 1.7-2.8c.1-.3 .2-.5 .2-.6V112zM192 368c0 26.5-21.5 48-48 48s-48-21.5-48-48c0-20.9 13.4-38.7 32-45.3V272c0-8.8 7.2-16 16-16s16 7.2 16 16v50.7c18.6 6.6 32 24.4 32 45.3z\"/><\/svg>&nbsp;&nbsp;&nbsp;0 &ndash; 10&deg;C<br><svg preserveAspectRatio=\"none\" aria-hidden=\"true\" role=\"img\" viewBox=\"0 0 320 512\" style=\"height:1em;width:1em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FFFFBF;overflow:visible;position:relative;\"><path d=\"M160 64c-26.5 0-48 21.5-48 48V276.5c0 17.3-7.1 31.9-15.3 42.5C86.2 332.6 80 349.5 80 368c0 44.2 35.8 80 80 80s80-35.8 80-80c0-18.5-6.2-35.4-16.7-48.9c-8.2-10.6-15.3-25.2-15.3-42.5V112c0-26.5-21.5-48-48-48zM48 112C48 50.2 98.1 0 160 0s112 50.1 112 112V276.5c0 .1 .1 .3 .2 .6c.2 .6 .8 1.6 1.7 2.8c18.9 24.4 30.1 55 30.1 88.1c0 79.5-64.5 144-144 144S16 447.5 16 368c0-33.2 11.2-63.8 30.1-88.1c.9-1.2 1.5-2.2 1.7-2.8c.1-.3 .2-.5 .2-.6V112zM208 368c0 26.5-21.5 48-48 48s-48-21.5-48-48c0-20.9 13.4-38.7 32-45.3V208c0-8.8 7.2-16 16-16s16 7.2 16 16V322.7c18.6 6.6 32 24.4 32 45.3z\"/><\/svg>&nbsp;&nbsp;&nbsp;10 &ndash; 20&deg;C<br><svg preserveAspectRatio=\"none\" aria-hidden=\"true\" role=\"img\" viewBox=\"0 0 512 512\" style=\"height:1em;width:1em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FC8D59;overflow:visible;position:relative;\"><path d=\"M416 64a32 32 0 1 1 0 64 32 32 0 1 1 0-64zm0 128A96 96 0 1 0 416 0a96 96 0 1 0 0 192zM96 112c0-26.5 21.5-48 48-48s48 21.5 48 48V276.5c0 17.3 7.1 31.9 15.3 42.5C217.8 332.6 224 349.5 224 368c0 44.2-35.8 80-80 80s-80-35.8-80-80c0-18.5 6.2-35.4 16.7-48.9C88.9 308.4 96 293.8 96 276.5V112zM144 0C82.1 0 32 50.2 32 112V276.5c0 .1-.1 .3-.2 .6c-.2 .6-.8 1.6-1.7 2.8C11.2 304.2 0 334.8 0 368c0 79.5 64.5 144 144 144s144-64.5 144-144c0-33.2-11.2-63.8-30.1-88.1c-.9-1.2-1.5-2.2-1.7-2.8c-.1-.3-.2-.5-.2-.6V112C256 50.2 205.9 0 144 0zm0 416c26.5 0 48-21.5 48-48c0-20.9-13.4-38.7-32-45.3V112c0-8.8-7.2-16-16-16s-16 7.2-16 16V322.7c-18.6 6.6-32 24.4-32 45.3c0 26.5 21.5 48 48 48z\"/><\/svg>&nbsp;&nbsp;&nbsp;20 &ndash; 30&deg;C","topright",null,"info legend"]}]},"evals":[],"jsHooks":[]}
```
