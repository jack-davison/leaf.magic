# Create a set of Font Awesome, Bootstrap, Ionicons or Lucide Markers

This function is intended as a more up-to-date implementation of
[`leaflet::awesomeIcons()`](https://rstudio.github.io/leaflet/reference/awesomeIcons.html).
Key benefits include having access to modern Font Awesome, Bootstrap,
Ionicons and Lucide icons, and allowing any colour to be used for the
marker and icon.

## Usage

``` r
magicIcons(
  icon = "circle",
  markerColor = leaf.magic::awesomePalette$blue,
  iconColor = "auto",
  marker = "marker",
  markerSize = 30L,
  outlineColor = NULL,
  outlineWidth = NULL,
  library = "fontawesome",
  className = NULL,
  dir = tempdir(),
  shadow = TRUE
)
```

## Arguments

- icon:

  Name of the icon. A full list of available icons can be found using
  [`fontawesome::fa_metadata()`](https://rstudio.github.io/fontawesome/reference/fa_metadata.html),
  <https://icons.getbootstrap.com/>, <https://ionic.io/ionicons> or
  <https://lucide.dev/icons/>, depending on `library`. When
  `library = "text"`, the text (or numbers) to show in each marker
  instead, and when `library = "svg"`, a string of SVG markup.

- markerColor:

  The color of the marker. Not used when `marker = "none"`.

- iconColor:

  The color of the icon. `"auto"` chooses black or white for each
  marker, whichever has the greater contrast with `markerColor`. When
  `marker = "none"`, `"auto"` instead gives the icon the colour
  `markerColor`, as there is no marker for it to contrast with.

- marker:

  The shape of the marker. Defaults to `"marker"`, which uses the
  standard teardrop shaped pin, similar to
  [`leaflet::addMarkers()`](https://rstudio.github.io/leaflet/reference/map-layers.html).
  The other built-in shapes are:

  - Pointed shapes, which sit on their location with their tip:
    `"marker-square"` (a pin with a square head) and `"callout"` (a
    speech bubble).

  - Shapes centred on their location: `"circle"`, `"square"`,
    `"diamond"`, `"triangle"`, `"hexagon"`, `"star"` and `"heart"`.

  - `"none"`, which removes the marker entirely and places the icon
    directly on the map.

  Custom shapes can be created using
  [`markerShape()`](https://jack-davison.github.io/leaf.magic/reference/markerShape.md).
  `marker` may be a character vector of shape names, a single
  [`markerShape()`](https://jack-davison.github.io/leaf.magic/reference/markerShape.md),
  or a list of
  [`markerShape()`](https://jack-davison.github.io/leaf.magic/reference/markerShape.md)s
  and/or shape names to use a different shape for each marker.

- markerSize:

  The width of the marker in pixels. Defaults to `30`, which is roughly
  the same size as
  [`leaflet::addMarkers()`](https://rstudio.github.io/leaflet/reference/map-layers.html).

- outlineColor:

  The colour of the outline around each marker. Defaults to a slightly
  darker shade of `markerColor`. A contrasting colour (e.g., `"white"`)
  helps markers stand out against similarly coloured basemaps.

- outlineWidth:

  The width of the outline in pixels. Defaults to a subtle outline which
  scales with `markerSize` (around half a pixel at the default size).
  Markers are not shrunk to make room for wider outlines, so they grow
  by the extra width.

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

- className:

  a custom class name to assign to both icon and shadow images

- dir:

  The directory in which markers are saved. By default this is
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html), which is deleted
  at the end of each R session. Providing an alternative directory will
  allow markers to persist between R sessions. `dir` is created if it
  does not already exist.

- shadow:

  Should markers have a shadow? Defaults to `TRUE`. As with the default
  [`leaflet::addMarkers()`](https://rstudio.github.io/leaflet/reference/map-layers.html)
  marker, pointed shapes such as `"marker"` cast a shadow up and to the
  right, while other shapes have a soft shadow just beneath them.
  `"none"` never has a shadow. The shadow of a custom shape is set using
  the `shadow` argument of
  [`markerShape()`](https://jack-davison.github.io/leaf.magic/reference/markerShape.md).

## Value

a
[`leaflet::iconList()`](https://rstudio.github.io/leaflet/reference/iconList.html),
to be passed to the `icon` argument of
[`leaflet::addMarkers()`](https://rstudio.github.io/leaflet/reference/map-layers.html).
If any argument is a formula, a formula which builds that
[`leaflet::iconList()`](https://rstudio.github.io/leaflet/reference/iconList.html)
from the layer's data is returned instead.

## Details

Each marker is built as a single SVG image, with the icon nested inside
of the marker shape, and saved to `dir`. As markers are vector images,
they stay sharp at any `markerSize` and on high resolution screens.
Markers are only built once for each combination of icon, colours,
marker shape and library, so you may find the use of `magicIcons()`
speeds up the more you use it in a single session.

Each icon library is provided by a separate package which must be
installed to use it: `{fontawesome}`, `{bsicons}` or `{lucidr}`.
Ionicons are downloaded from <https://unpkg.com> the first time each
icon is used in a session, so require an internet connection.

## Using formulas

As with
[`leaflet::awesomeIcons()`](https://rstudio.github.io/leaflet/reference/awesomeIcons.html),
`icon`, `markerColor`, `iconColor`, `marker`, `markerSize`,
`outlineColor` and `outlineWidth` may be one-sided formulas (e.g.,
`~site_type`), which are evaluated against the data of the layer the
markers are added to. Markers are then built when the layer is added, so
`magicIcons()` must be passed directly to the `icon` argument of
[`leaflet::addMarkers()`](https://rstudio.github.io/leaflet/reference/map-layers.html).
Formulas and constants can be mixed freely, and formulas may call other
functions, such as an
[`iconFactor()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
scale or a
[`leaflet::colorFactor()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html)
palette (e.g., `markerColor = ~pal(site_type)`).

## See also

[`markerShape()`](https://jack-davison.github.io/leaf.magic/reference/markerShape.md),
[`addIconLegend()`](https://jack-davison.github.io/leaf.magic/reference/addIconLegend.md),
[`iconFactor()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md),
[`iconBin()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md),
[`iconQuantile()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)

## Author

Jack Davison

## Examples

``` r
# create map
library(leaflet)
port_talbot |>
  dplyr::mutate(
    icon = dplyr::recode_values(
      site_type,
      "Urban Industrial" ~ "industry",
      "Urban Traffic" ~ "car",
      "Urban Background" ~ "city",
      default = "x"
    ),
    color = dplyr::recode_values(
      site_type,
      "Urban Industrial" ~ "#12436D",
      "Urban Traffic" ~ "#801650",
      "Urban Background" ~ "#28A197",
      default = "#3D3D3DFF"
    )
  ) |>
  leaflet() |>
  addTiles() |>
  addMarkers(
    icon = magicIcons(icon = ~icon, markerColor = ~color, iconColor = "white"),
    lat = ~latitude,
    lng = ~longitude,
    popup = ~site
  )

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"https://openstreetmap.org/copyright/\">OpenStreetMap<\/a>,  <a href=\"https://opendatacommons.org/licenses/odbl/\">ODbL<\/a>"}]},{"method":"addMarkers","args":[[51.579656,51.58493,51.589942,51.591479,51.576676,51.590233,51.58395,51.572433,51.5798],[-3.766465,-3.801069,-3.772005,-3.779197,-3.759005,-3.786049,-3.770822,-3.751092,-3.76169],{"iconUrl":{"data":["data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCAxMDAgMTMyIiB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEzMiI+PGcgZmlsbD0iIzEyNDM2RCIgc3Ryb2tlPSIjMEUzNTU3IiBzdHJva2Utd2lkdGg9IjIiIHN0cm9rZS1saW5lam9pbj0icm91bmQiPjxwYXRoIGQ9Ik01MCAxMzBDMzggMTE0IDIgODAgMiA1MGE0OCA0OCAwIDAgMSA5NiAwYzAgMzAtMzYgNjQtNDggODB6Ii8+PC9nPjxzdmcgYXJpYS1oaWRkZW49InRydWUiIHJvbGU9ImltZyIgdmlld0JveD0iMCAwIDU3NiA1MTIiIHg9IjI0IiB5PSIyNCIgd2lkdGg9IjUyIiBoZWlnaHQ9IjUyIiBjb2xvcj0id2hpdGUiIGZpbGw9IndoaXRlIiBzdHJva2U9IiNDQ0NDQ0MiIHN0cm9rZS13aWR0aD0iNS4xMiI+PHBhdGggZD0iTTY0IDMyQzQ2LjMgMzIgMzIgNDYuMyAzMiA2NFYzMDR2NDggODBjMCAyNi41IDIxLjUgNDggNDggNDhINDk2YzI2LjUgMCA0OC0yMS41IDQ4LTQ4VjMwNCAxNTIuMmMwLTE4LjItMTkuNC0yOS43LTM1LjQtMjEuMUwzNTIgMjE1LjRWMTUyLjJjMC0xOC4yLTE5LjQtMjkuNy0zNS40LTIxLjFMMTYwIDIxNS40VjY0YzAtMTcuNy0xNC4zLTMyLTMyLTMySDY0eiIvPjwvc3ZnPjwvc3ZnPgo=","data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCAxMDAgMTMyIiB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEzMiI+PGcgZmlsbD0iIzgwMTY1MCIgc3Ryb2tlPSIjNjYxMTQwIiBzdHJva2Utd2lkdGg9IjIiIHN0cm9rZS1saW5lam9pbj0icm91bmQiPjxwYXRoIGQ9Ik01MCAxMzBDMzggMTE0IDIgODAgMiA1MGE0OCA0OCAwIDAgMSA5NiAwYzAgMzAtMzYgNjQtNDggODB6Ii8+PC9nPjxzdmcgYXJpYS1oaWRkZW49InRydWUiIHJvbGU9ImltZyIgdmlld0JveD0iMCAwIDUxMiA1MTIiIHg9IjI0IiB5PSIyNCIgd2lkdGg9IjUyIiBoZWlnaHQ9IjUyIiBjb2xvcj0id2hpdGUiIGZpbGw9IndoaXRlIiBzdHJva2U9IiNDQ0NDQ0MiIHN0cm9rZS13aWR0aD0iNS4xMiI+PHBhdGggZD0iTTEzNS4yIDExNy40TDEwOS4xIDE5Mkg0MDIuOWwtMjYuMS03NC42QzM3Mi4zIDEwNC42IDM2MC4yIDk2IDM0Ni42IDk2SDE2NS40Yy0xMy42IDAtMjUuNyA4LjYtMzAuMiAyMS40ek0zOS42IDE5Ni44TDc0LjggOTYuM0M4OC4zIDU3LjggMTI0LjYgMzIgMTY1LjQgMzJIMzQ2LjZjNDAuOCAwIDc3LjEgMjUuOCA5MC42IDY0LjNsMzUuMiAxMDAuNWMyMy4yIDkuNiAzOS42IDMyLjUgMzkuNiA1OS4yVjQwMHY0OGMwIDE3LjctMTQuMyAzMi0zMiAzMkg0NDhjLTE3LjcgMC0zMi0xNC4zLTMyLTMyVjQwMEg5NnY0OGMwIDE3LjctMTQuMyAzMi0zMiAzMkgzMmMtMTcuNyAwLTMyLTE0LjMtMzItMzJWNDAwIDI1NmMwLTI2LjcgMTYuNC00OS42IDM5LjYtNTkuMnpNMTI4IDI4OGEzMiAzMiAwIDEgMCAtNjQgMCAzMiAzMiAwIDEgMCA2NCAwem0yODggMzJhMzIgMzIgMCAxIDAgMC02NCAzMiAzMiAwIDEgMCAwIDY0eiIvPjwvc3ZnPjwvc3ZnPgo=","data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCAxMDAgMTMyIiB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEzMiI+PGcgZmlsbD0iIzI4QTE5NyIgc3Ryb2tlPSIjMjA4MDc4IiBzdHJva2Utd2lkdGg9IjIiIHN0cm9rZS1saW5lam9pbj0icm91bmQiPjxwYXRoIGQ9Ik01MCAxMzBDMzggMTE0IDIgODAgMiA1MGE0OCA0OCAwIDAgMSA5NiAwYzAgMzAtMzYgNjQtNDggODB6Ii8+PC9nPjxzdmcgYXJpYS1oaWRkZW49InRydWUiIHJvbGU9ImltZyIgdmlld0JveD0iMCAwIDY0MCA1MTIiIHg9IjI0IiB5PSIyNCIgd2lkdGg9IjUyIiBoZWlnaHQ9IjUyIiBjb2xvcj0id2hpdGUiIGZpbGw9IndoaXRlIiBzdHJva2U9IiNDQ0NDQ0MiIHN0cm9rZS13aWR0aD0iNS4xMiI+PHBhdGggZD0iTTQ4MCA0OGMwLTI2LjUtMjEuNS00OC00OC00OEgzMzZjLTI2LjUgMC00OCAyMS41LTQ4IDQ4Vjk2SDIyNFYyNGMwLTEzLjMtMTAuNy0yNC0yNC0yNHMtMjQgMTAuNy0yNCAyNFY5NkgxMTJWMjRjMC0xMy4zLTEwLjctMjQtMjQtMjRTNjQgMTAuNyA2NCAyNFY5Nkg0OEMyMS41IDk2IDAgMTE3LjUgMCAxNDR2OTZWNDY0YzAgMjYuNSAyMS41IDQ4IDQ4IDQ4SDMwNGgzMiA5Nkg1OTJjMjYuNSAwIDQ4LTIxLjUgNDgtNDhWMjQwYzAtMjYuNS0yMS41LTQ4LTQ4LTQ4SDQ4MFY0OHptOTYgMzIwdjMyYzAgOC44LTcuMiAxNi0xNiAxNkg1MjhjLTguOCAwLTE2LTcuMi0xNi0xNlYzNjhjMC04LjggNy4yLTE2IDE2LTE2aDMyYzguOCAwIDE2IDcuMiAxNiAxNnpNMjQwIDQxNkgyMDhjLTguOCAwLTE2LTcuMi0xNi0xNlYzNjhjMC04LjggNy4yLTE2IDE2LTE2aDMyYzguOCAwIDE2IDcuMiAxNiAxNnYzMmMwIDguOC03LjIgMTYtMTYgMTZ6TTEyOCA0MDBjMCA4LjgtNy4yIDE2LTE2IDE2SDgwYy04LjggMC0xNi03LjItMTYtMTZWMzY4YzAtOC44IDcuMi0xNiAxNi0xNmgzMmM4LjggMCAxNiA3LjIgMTYgMTZ2MzJ6TTU2MCAyNTZjOC44IDAgMTYgNy4yIDE2IDE2djMyYzAgOC44LTcuMiAxNi0xNiAxNkg1MjhjLTguOCAwLTE2LTcuMi0xNi0xNlYyNzJjMC04LjggNy4yLTE2IDE2LTE2aDMyek0yNTYgMTc2djMyYzAgOC44LTcuMiAxNi0xNiAxNkgyMDhjLTguOCAwLTE2LTcuMi0xNi0xNlYxNzZjMC04LjggNy4yLTE2IDE2LTE2aDMyYzguOCAwIDE2IDcuMiAxNiAxNnpNMTEyIDE2MGM4LjggMCAxNiA3LjIgMTYgMTZ2MzJjMCA4LjgtNy4yIDE2LTE2IDE2SDgwYy04LjggMC0xNi03LjItMTYtMTZWMTc2YzAtOC44IDcuMi0xNiAxNi0xNmgzMnpNMjU2IDMwNGMwIDguOC03LjIgMTYtMTYgMTZIMjA4Yy04LjggMC0xNi03LjItMTYtMTZWMjcyYzAtOC44IDcuMi0xNiAxNi0xNmgzMmM4LjggMCAxNiA3LjIgMTYgMTZ2MzJ6TTExMiAzMjBIODBjLTguOCAwLTE2LTcuMi0xNi0xNlYyNzJjMC04LjggNy4yLTE2IDE2LTE2aDMyYzguOCAwIDE2IDcuMiAxNiAxNnYzMmMwIDguOC03LjIgMTYtMTYgMTZ6bTMwNC00OHYzMmMwIDguOC03LjIgMTYtMTYgMTZIMzY4Yy04LjggMC0xNi03LjItMTYtMTZWMjcyYzAtOC44IDcuMi0xNiAxNi0xNmgzMmM4LjggMCAxNiA3LjIgMTYgMTZ6TTQwMCA2NGM4LjggMCAxNiA3LjIgMTYgMTZ2MzJjMCA4LjgtNy4yIDE2LTE2IDE2SDM2OGMtOC44IDAtMTYtNy4yLTE2LTE2VjgwYzAtOC44IDcuMi0xNiAxNi0xNmgzMnptMTYgMTEydjMyYzAgOC44LTcuMiAxNi0xNiAxNkgzNjhjLTguOCAwLTE2LTcuMi0xNi0xNlYxNzZjMC04LjggNy4yLTE2IDE2LTE2aDMyYzguOCAwIDE2IDcuMiAxNiAxNnoiLz48L3N2Zz48L3N2Zz4K"],"index":[0,0,1,1,1,2,0,2,2]},"iconWidth":30,"iconHeight":39.6,"iconAnchorX":15,"iconAnchorY":39,"shadowUrl":{"data":"data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9Ii05IDU3IDE4MiA4MiIgd2lkdGg9IjE4MiIgaGVpZ2h0PSI4MiI+PGRlZnM+PGZpbHRlciBpZD0ic2hhZG93IiBmaWx0ZXJVbml0cz0idXNlclNwYWNlT25Vc2UiIHg9Ii05IiB5PSI1NyIgd2lkdGg9IjE4MiIgaGVpZ2h0PSI4MiI+PGZlQ29sb3JNYXRyaXggdHlwZT0ibWF0cml4IiB2YWx1ZXM9IjAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAuMzUgMCIvPjxmZUdhdXNzaWFuQmx1ciBzdGREZXZpYXRpb249IjIuNSIvPjwvZmlsdGVyPjwvZGVmcz48ZyBmaWx0ZXI9InVybCgjc2hhZG93KSI+PGcgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIC0wLjUgMC41IDY1IDY1KSIgZmlsbD0iIzAwMCIgc3Ryb2tlPSIjMDAwIiBzdHJva2Utd2lkdGg9IjIiIHN0cm9rZS1saW5lam9pbj0icm91bmQiPjxwYXRoIGQ9Ik01MCAxMzBDMzggMTE0IDIgODAgMiA1MGE0OCA0OCAwIDAgMSA5NiAwYzAgMzAtMzYgNjQtNDggODB6Ii8+PC9nPjwvZz48L3N2Zz4K","index":0},"shadowWidth":54.6,"shadowHeight":24.6,"shadowAnchorX":17.7,"shadowAnchorY":21.9,"popupAnchorX":2.220446049250313e-16,"popupAnchorY":-31.8},null,null,{"interactive":true,"draggable":false,"keyboard":true,"title":"","alt":"","zIndexOffset":0,"opacity":1,"riseOnHover":false,"riseOffset":250},["Prince Street 2","Little Warren","Theodore Road","Talbot Road","Twll-yn-y-Wal Park","Docks","Margam","Dyffryn School","Port Talbot"],null,null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[51.572433,51.591479],"lng":[-3.801069,-3.751092]}},"evals":[],"jsHooks":[]}
```
