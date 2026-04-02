# Add an Icon legend to a map

Manually specify icons, labels, and colours to add an Icon legend to a
map. Useful in conjunction with
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
  data = leaflet::getMapData(map)
)
```

## Arguments

- map:

  a map widget object created from
  [`leaflet()`](https://rstudio.github.io/leaflet/reference/leaflet.html)

- icons:

  Name of the icons, passed to
  [`fontawesome::fa()`](https://rstudio.github.io/fontawesome/reference/fa.html)
  or
  [`bsicons::bs_icon()`](https://rdrr.io/pkg/bsicons/man/bs_icon.html).
  A full list of available icons can be found using
  [`fontawesome::fa_metadata()`](https://rstudio.github.io/fontawesome/reference/fa_metadata.html),
  <https://icons.getbootstrap.com/> or <https://ionic.io/ionicons>.

- labels:

  Labels for each `icon`; should be the same length as `icons`.

- colors:

  Colours to use for each `icon`. If `length(colors) == 1L` it is
  recycled for all icons. Otherwise should be the same length as
  `icons`.

- title:

  the legend title; optional.

- library:

  One of `"fontawesome"`, `"bootstrap"`, `"ionicons"`, or `"lucide"`
  defining the icon library of interest. Defaults to `"fontawesome"`.

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

## Value

a
[leaflet](https://rstudio.github.io/leaflet/reference/leaflet-package.html)
widget

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

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addControl","args":["<div style='margin-bottom:3px'><strong>Student Hangout<\/strong><\/div><svg preserveAspectRatio=\"none\" aria-hidden=\"true\" role=\"img\" viewBox=\"0 0 512 512\" style=\"height:1em;width:1em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:red;overflow:visible;position:relative;\"><path d=\"M32 64c0-17.7 14.3-32 32-32H352c17.7 0 32 14.3 32 32V96h51.2c42.4 0 76.8 34.4 76.8 76.8V274.9c0 30.4-17.9 57.9-45.6 70.2L384 381.7V416c0 35.3-28.7 64-64 64H96c-35.3 0-64-28.7-64-64V64zM384 311.6l56.4-25.1c4.6-2.1 7.6-6.6 7.6-11.7V172.8c0-7.1-5.7-12.8-12.8-12.8H384V311.6zM160 144c0-8.8-7.2-16-16-16s-16 7.2-16 16V368c0 8.8 7.2 16 16 16s16-7.2 16-16V144zm64 0c0-8.8-7.2-16-16-16s-16 7.2-16 16V368c0 8.8 7.2 16 16 16s16-7.2 16-16V144zm64 0c0-8.8-7.2-16-16-16s-16 7.2-16 16V368c0 8.8 7.2 16 16 16s16-7.2 16-16V144z\"/><\/svg>   Pub<br><svg preserveAspectRatio=\"none\" aria-hidden=\"true\" role=\"img\" viewBox=\"0 0 640 512\" style=\"height:1em;width:1em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:blue;overflow:visible;position:relative;\"><path d=\"M337.8 5.4C327-1.8 313-1.8 302.2 5.4L166.3 96H48C21.5 96 0 117.5 0 144V464c0 26.5 21.5 48 48 48H256V416c0-35.3 28.7-64 64-64s64 28.7 64 64v96H592c26.5 0 48-21.5 48-48V144c0-26.5-21.5-48-48-48H473.7L337.8 5.4zM96 192h32c8.8 0 16 7.2 16 16v64c0 8.8-7.2 16-16 16H96c-8.8 0-16-7.2-16-16V208c0-8.8 7.2-16 16-16zm400 16c0-8.8 7.2-16 16-16h32c8.8 0 16 7.2 16 16v64c0 8.8-7.2 16-16 16H512c-8.8 0-16-7.2-16-16V208zM96 320h32c8.8 0 16 7.2 16 16v64c0 8.8-7.2 16-16 16H96c-8.8 0-16-7.2-16-16V336c0-8.8 7.2-16 16-16zm400 16c0-8.8 7.2-16 16-16h32c8.8 0 16 7.2 16 16v64c0 8.8-7.2 16-16 16H512c-8.8 0-16-7.2-16-16V336zM232 176a88 88 0 1 1 176 0 88 88 0 1 1 -176 0zm88-48c-8.8 0-16 7.2-16 16v32c0 8.8 7.2 16 16 16h32c8.8 0 16-7.2 16-16s-7.2-16-16-16H336V144c0-8.8-7.2-16-16-16z\"/><\/svg>   University","topright",null,"info legend"]}]},"evals":[],"jsHooks":[]}
```
