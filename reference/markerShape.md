# Define a custom marker shape

Creates a marker shape for use with the `marker` argument of
[`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md).
A shape is drawn using SVG in its own coordinate system, `width` units
wide and `height` units tall. The icon is then placed in a square within
the shape, and the whole marker is scaled so that it is `markerSize`
pixels wide on the map. All of the built-in shapes (`"marker"`,
`"circle"`, etc.) are defined in this way.

## Usage

``` r
markerShape(
  path,
  width = 100,
  height = 100,
  icon = c(x = width/2, y = height/2, size = min(width, height)/2),
  anchor = c(x = width/2, y = height/2),
  popup = anchor,
  shadow = c("drop", "cast", "none")
)
```

## Arguments

- path:

  The shape to draw. Either the `d` attribute of an SVG `<path>` (e.g.,
  `"M50 2 98 98 2 98z"` for a triangle), or SVG markup of one or more
  elements (e.g., `'<circle cx="50" cy="50" r="48"/>'`). Use `NULL` for
  no shape, in which case only the icon is drawn.

- width, height:

  The size of the coordinate system the shape is drawn in. Their ratio
  defines the aspect ratio of the marker.

- icon:

  Where to draw the icon, as a numeric vector of the `x` and `y`
  coordinates of its centre and its `size` (the side of the square the
  icon is fitted into). Unnamed vectors are taken to be in the order
  `x`, `y`, `size`.

- anchor:

  The coordinates of the point of the shape which sits on the marker's
  location on the map, as a numeric vector of `x` and `y`. Defaults to
  the centre of the shape. For pointed shapes, such as `"marker"`, this
  should be the tip.

- popup:

  The coordinates from which popups open, as a numeric vector of `x` and
  `y`. Defaults to `anchor`.

- shadow:

  The style of shadow drawn beneath the marker when
  `magicIcons(shadow = TRUE)`. One of:

  - `"drop"` (the default): a soft shadow just below the shape, as if it
    were lying flat on the map. Suits shapes centred on their location.

  - `"cast"`: a shadow cast from `anchor` up and to the right, as if the
    shape were standing upright on its tip, like the default
    [`leaflet::addMarkers()`](https://rstudio.github.io/leaflet/reference/map-layers.html)
    marker. Suits pointed shapes.

  - `"none"`: no shadow.

## Value

A `leafmagic_marker_shape` object.

## Drawing shapes

The shape is filled with `markerColor` and outlined in a slightly darker
colour with a stroke width of `2` units, so `path` should not usually
set its own `fill` or `stroke`. As the outline is centred on the edge of
the shape, leave a margin of at least `1` unit between the shape and the
edges of the coordinate system so that it isn't clipped.

## Shadows

As in `{leaflet}`, shadows are drawn as separate images in the map's
shadow pane, so a marker's shadow never covers another marker. Shadows
are generated from the shape itself, so work for any shape. Shapes with
no `path` never have a shadow.

## See also

[`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md)

## Examples

``` r
# a downwards-pointing triangle, anchored at its tip
arrow <- markerShape(
  "M2 2h96L50 98z",
  icon = c(x = 50, y = 34, size = 36),
  anchor = c(x = 50, y = 98),
  popup = c(x = 50, y = 10),
  shadow = "cast"
)
arrow
#> <markerShape> 100 x 100
#> * icon: size 36 centred at (50, 34)
#> * anchor: (50, 98)
#> * popup: (50, 10)
#> * shadow: cast

# use it with magicIcons()
library(leaflet)
leaflet(port_talbot) |>
  addTiles() |>
  addMarkers(
    lng = ~longitude,
    lat = ~latitude,
    icon = magicIcons("industry", marker = arrow)
  )

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"https://openstreetmap.org/copyright/\">OpenStreetMap<\/a>,  <a href=\"https://opendatacommons.org/licenses/odbl/\">ODbL<\/a>"}]},{"method":"addMarkers","args":[[51.579656,51.58493,51.589942,51.591479,51.576676,51.590233,51.58395,51.572433,51.5798],[-3.766465,-3.801069,-3.772005,-3.779197,-3.759005,-3.786049,-3.770822,-3.751092,-3.76169],{"iconUrl":{"data":"data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCAxMDAgMTAwIiB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEwMCI+PGcgZmlsbD0iIzM4YWFkZCIgc3Ryb2tlPSIjMkM4OEIwIiBzdHJva2Utd2lkdGg9IjIiIHN0cm9rZS1saW5lam9pbj0icm91bmQiPjxwYXRoIGQ9Ik0yIDJoOTZMNTAgOTh6Ii8+PC9nPjxzdmcgYXJpYS1oaWRkZW49InRydWUiIHJvbGU9ImltZyIgdmlld0JveD0iMCAwIDU3NiA1MTIiIHg9IjMyIiB5PSIxNiIgd2lkdGg9IjM2IiBoZWlnaHQ9IjM2IiBjb2xvcj0iIzAwMDAwMCIgZmlsbD0iIzAwMDAwMCIgc3Ryb2tlPSIjMDAwMDAwIiBzdHJva2Utd2lkdGg9IjUuMTIiPjxwYXRoIGQ9Ik02NCAzMkM0Ni4zIDMyIDMyIDQ2LjMgMzIgNjRWMzA0djQ4IDgwYzAgMjYuNSAyMS41IDQ4IDQ4IDQ4SDQ5NmMyNi41IDAgNDgtMjEuNSA0OC00OFYzMDQgMTUyLjJjMC0xOC4yLTE5LjQtMjkuNy0zNS40LTIxLjFMMzUyIDIxNS40VjE1Mi4yYzAtMTguMi0xOS40LTI5LjctMzUuNC0yMS4xTDE2MCAyMTUuNFY2NGMwLTE3LjctMTQuMy0zMi0zMi0zMkg2NHoiLz48L3N2Zz48L3N2Zz4K","index":0},"iconWidth":30,"iconHeight":30,"iconAnchorX":15,"iconAnchorY":29.4,"shadowUrl":{"data":"data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9Ii05IDQxIDE2NiA2NiIgd2lkdGg9IjE2NiIgaGVpZ2h0PSI2NiI+PGRlZnM+PGZpbHRlciBpZD0ic2hhZG93IiBmaWx0ZXJVbml0cz0idXNlclNwYWNlT25Vc2UiIHg9Ii05IiB5PSI0MSIgd2lkdGg9IjE2NiIgaGVpZ2h0PSI2NiI+PGZlQ29sb3JNYXRyaXggdHlwZT0ibWF0cml4IiB2YWx1ZXM9IjAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAuMzUgMCIvPjxmZUdhdXNzaWFuQmx1ciBzdGREZXZpYXRpb249IjIuNSIvPjwvZmlsdGVyPjwvZGVmcz48ZyBmaWx0ZXI9InVybCgjc2hhZG93KSI+PGcgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIC0wLjUgMC41IDQ5IDQ5KSIgZmlsbD0iIzAwMCIgc3Ryb2tlPSIjMDAwIiBzdHJva2Utd2lkdGg9IjIiIHN0cm9rZS1saW5lam9pbj0icm91bmQiPjxwYXRoIGQ9Ik0yIDJoOTZMNTAgOTh6Ii8+PC9nPjwvZz48L3N2Zz4K","index":0},"shadowWidth":49.8,"shadowHeight":19.8,"shadowAnchorX":17.7,"shadowAnchorY":17.1,"popupAnchorX":2.220446049250313e-16,"popupAnchorY":-26.4},null,null,{"interactive":true,"draggable":false,"keyboard":true,"title":"","alt":"","zIndexOffset":0,"opacity":1,"riseOnHover":false,"riseOffset":250},null,null,null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[51.572433,51.591479],"lng":[-3.801069,-3.751092]}},"evals":[],"jsHooks":[]}
```
