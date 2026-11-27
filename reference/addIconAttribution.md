# Conveniently add Icon Attributions to a Leaflet Map

Adds a short character string to the bottom-right of the map widget with
the attribution and license of an icon library. `group` and `layerId`
can be used to add or remove the attribution - e.g., if the markers
using
[`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md)
are hidden.

## Usage

``` r
addIconAttribution(map, library = "fontawesome", layerId = NULL, group = NULL)
```

## Arguments

- map:

  a map widget object created from
  [`leaflet()`](https://rstudio.github.io/leaflet/reference/leaflet.html)

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

- layerId:

  the layer id

- group:

  the name of the group the newly created layers should belong to (for
  [`clearGroup()`](https://rstudio.github.io/leaflet/reference/remove.html)
  and
  [`addLayersControl()`](https://rstudio.github.io/leaflet/reference/addLayersControl.html)
  purposes). Human-friendly group names are permitted–they need not be
  short, identifier-style names. Any number of layers and even different
  types of layers (e.g., markers and polygons) can share the same group
  name.

## Value

a
[leaflet](https://rstudio.github.io/leaflet/reference/leaflet-package.html)
widget

## Examples

``` r
library(leaflet)
leaflet(port_talbot) |>
  addTiles() |>
  addMarkers(
    lng = ~longitude,
    lat = ~latitude,
    icon = magicIcons("industry"),
    group = "Sites"
  ) |>
  addIconAttribution(group = "Sites") |>
  addLayersControl(overlayGroups = "Sites")

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"https://openstreetmap.org/copyright/\">OpenStreetMap<\/a>,  <a href=\"https://opendatacommons.org/licenses/odbl/\">ODbL<\/a>"}]},{"method":"addMarkers","args":[[51.579656,51.58493,51.589942,51.591479,51.576676,51.590233,51.58395,51.572433,51.5798],[-3.766465,-3.801069,-3.772005,-3.779197,-3.759005,-3.786049,-3.770822,-3.751092,-3.76169],{"iconUrl":{"data":"data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCAxMDAgMTMyIiB3aWR0aD0iMTAwIiBoZWlnaHQ9IjEzMiI+PGcgZmlsbD0iIzM4YWFkZCIgc3Ryb2tlPSIjMkM4OEIwIiBzdHJva2Utd2lkdGg9IjIiIHN0cm9rZS1saW5lam9pbj0icm91bmQiPjxwYXRoIGQ9Ik01MCAxMzBDMzggMTE0IDIgODAgMiA1MGE0OCA0OCAwIDAgMSA5NiAwYzAgMzAtMzYgNjQtNDggODB6Ii8+PC9nPjxzdmcgYXJpYS1oaWRkZW49InRydWUiIHJvbGU9ImltZyIgdmlld0JveD0iMCAwIDU3NiA1MTIiIHg9IjI0IiB5PSIyNCIgd2lkdGg9IjUyIiBoZWlnaHQ9IjUyIiBjb2xvcj0iIzAwMDAwMCIgZmlsbD0iIzAwMDAwMCIgc3Ryb2tlPSIjMDAwMDAwIiBzdHJva2Utd2lkdGg9IjUuMTIiPjxwYXRoIGQ9Ik02NCAzMkM0Ni4zIDMyIDMyIDQ2LjMgMzIgNjRWMzA0djQ4IDgwYzAgMjYuNSAyMS41IDQ4IDQ4IDQ4SDQ5NmMyNi41IDAgNDgtMjEuNSA0OC00OFYzMDQgMTUyLjJjMC0xOC4yLTE5LjQtMjkuNy0zNS40LTIxLjFMMzUyIDIxNS40VjE1Mi4yYzAtMTguMi0xOS40LTI5LjctMzUuNC0yMS4xTDE2MCAyMTUuNFY2NGMwLTE3LjctMTQuMy0zMi0zMi0zMkg2NHoiLz48L3N2Zz48L3N2Zz4K","index":0},"iconWidth":30,"iconHeight":39.6,"iconAnchorX":15,"iconAnchorY":39,"shadowUrl":{"data":"data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9Ii05IDU3IDE4MiA4MiIgd2lkdGg9IjE4MiIgaGVpZ2h0PSI4MiI+PGRlZnM+PGZpbHRlciBpZD0ic2hhZG93IiBmaWx0ZXJVbml0cz0idXNlclNwYWNlT25Vc2UiIHg9Ii05IiB5PSI1NyIgd2lkdGg9IjE4MiIgaGVpZ2h0PSI4MiI+PGZlQ29sb3JNYXRyaXggdHlwZT0ibWF0cml4IiB2YWx1ZXM9IjAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAgMCAwIDAuMzUgMCIvPjxmZUdhdXNzaWFuQmx1ciBzdGREZXZpYXRpb249IjIuNSIvPjwvZmlsdGVyPjwvZGVmcz48ZyBmaWx0ZXI9InVybCgjc2hhZG93KSI+PGcgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIC0wLjUgMC41IDY1IDY1KSIgZmlsbD0iIzAwMCIgc3Ryb2tlPSIjMDAwIiBzdHJva2Utd2lkdGg9IjIiIHN0cm9rZS1saW5lam9pbj0icm91bmQiPjxwYXRoIGQ9Ik01MCAxMzBDMzggMTE0IDIgODAgMiA1MGE0OCA0OCAwIDAgMSA5NiAwYzAgMzAtMzYgNjQtNDggODB6Ii8+PC9nPjwvZz48L3N2Zz4K","index":0},"shadowWidth":54.6,"shadowHeight":24.6,"shadowAnchorX":17.7,"shadowAnchorY":21.9,"popupAnchorX":2.220446049250313e-16,"popupAnchorY":-31.8},null,"Sites",{"interactive":true,"draggable":false,"keyboard":true,"title":"","alt":"","zIndexOffset":0,"opacity":1,"riseOnHover":false,"riseOffset":250},null,null,null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addTiles","args":["data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII=",null,"Sites",{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"Icons by <a href=\"https://fontawesome.com\" target=\"_blank\" rel=\"noopener noreferrer\">FontAwesome<\/a>, licensed under <a href=\"https://fontawesome.com/license/free\" target=\"_blank\" rel=\"noopener noreferrer\">CC BY 4.0<\/a>"}]},{"method":"addLayersControl","args":[[],"Sites",{"collapsed":true,"autoZIndex":true,"position":"topright"}]}],"limits":{"lat":[51.572433,51.591479],"lng":[-3.801069,-3.751092]}},"evals":[],"jsHooks":[]}
```
