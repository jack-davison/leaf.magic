# leaf.magic (development version)

## Breaking Changes

* `iconFactor()`, `iconBin()` and `iconQuantile()` have been rewritten to behave identically to `leaflet::colorFactor()`, `leaflet::colorBin()` and `leaflet::colorQuantile()`. Given the same settings, each level/bin now receives a consistent icon-colour pair. In particular, `iconFactor()` now sorts character domains alphabetically (or uses factor levels) rather than using the order of appearance; use `ordered = TRUE` for the old behaviour. Similarly, `iconBin()` now uses `pretty = TRUE` by default, as `colorBin()` does, which may produce a different number of bins than icons; use `pretty = FALSE` for the old behaviour.

* `magicIcons()` now builds each marker as a single SVG image, rather than compositing PNG images. Markers are therefore sharp at any `markerSize` and on high resolution screens, and icons are centred precisely within each marker shape. Marker shapes are now drawn by `{leaf.magic}` itself rather than borrowed from Font Awesome, so look slightly different to before.

* `{fontawesome}` has moved from Imports to Suggests, so every icon library is now optional; install `{fontawesome}`, `{bsicons}` or `{lucidr}` to use the respective library. `{magick}` and `{rsvg}` are no longer required.

## New Features

* New `markerShape()` function to define custom marker shapes using SVG, including where the icon sits, which point of the shape is anchored to the map location, and where popups open. Custom shapes can be passed to the `marker` argument of `magicIcons()`, either alone or in a list alongside other shapes and shape names.

* `magicIcons()` gains several new built-in marker shapes: `"marker-square"` (a pin with a square head), `"callout"` (a speech bubble), `"triangle"` and `"hexagon"`.

* `addIconLegend()` gains `pal`, `values`, `na.label` and `labFormat` arguments, allowing a legend to be generated automatically from an `iconFactor()`, `iconBin()` or `iconQuantile()` scale in the same way as `leaflet::addLegend()` (#10). Legend items, labels and missing value handling match `leaflet::addLegend()`, so icon and colour legends built from equivalent scales always agree. When `pal` is used, `colors` may also be a `{leaflet}` colour scale function, giving a legend of matched icon-colour pairs.

* `addIconLegend()` gains `group` and `opacity` arguments, as in `leaflet::addLegend()`. `group` allows the legend to be shown and hidden alongside a layer group.

* `iconFactor()` gains `levels`, `ordered` and `reverse` arguments; `iconBin()` gains `pretty` and `reverse`; and `iconQuantile()` gains `reverse`, all matching their `{leaflet}` equivalents. A `domain` of `NULL` is now also supported.

## Bug Fixes

* Outlined Ionicons (e.g., `"car-outline"`) are now drawn in `iconColor` in `magicIcons()`; previously they were always black.

* `magicIcons()` no longer produces pixelated icons when `library = "bootstrap"` or `library = "lucide"`. Icons are now rendered at a high resolution before being scaled down. Note that markers previously cached in a persistent `dir` will need to be deleted to benefit from this fix.

* `magicIcons()` now includes the package version in the file names of cached markers, so markers saved in a persistent `dir` are re-rendered after upgrading `{leaf.magic}`.

* `magicIcons()` now creates `dir` if it does not already exist, and cleans up its intermediate temporary files.

* `magicIcons()` and `addIconLegend()` now error informatively if their vector arguments are not length 1 or a common length. Previously, lengths that divided evenly (e.g., 2 icons and 4 colours) were silently recycled.

* Ionicons are now downloaded once per session rather than every time they are used, and a failed download (e.g., a misspelled icon name, or no internet connection) now gives an informative error.

* `addIconAttribution()` no longer makes a network request for every map tile.

* `addIconLegend()` now correctly spaces icons and labels; previously the intended spacing was collapsed by HTML into a single space.

* `addIconLegend()` legends for `iconBin()` scales with no `domain` now always match the map. Previously, if some bins contained no `values`, the legend could show different icons and colours from the markers. Only occupied bins are now shown.

* `addIconLegend()` now uses grey (`"#808080"`, as in `{leaflet}`) for the missing value item when `colors` is a single colour, rather than the colour used for every other item.

* `addIconLegend()` now gives an informative error if `icons` or `labels` is missing and `pal` is not used.

* `magicIcons()` now hashes the cache file names of markers, so any icon name gives a valid file name, and equivalent colours (e.g., `"red"`, `"#ff0000"` and `"#FF0000"`) share one cached marker.

## Dependency Changes

* `{purrr}` now requires version 1.0.0 or later, and `{dplyr}` (used in examples) version 1.2.0 or later.

* `{grDevices}` and `{stats}` are now explicitly imported.

* `iconBin()` and `iconQuantile()` no longer assign icons to the wrong bins when `domain` is unsorted or a bin is empty.

* `iconFactor()` now matches factor inputs by label rather than by their underlying integer codes.

* `iconFactor()` now allows the same icon to be used for multiple levels.

* `iconBin()` now works when `bins` is a vector of break points.

# leaf.magic 0.2.0

## Dependency Changes

* `{rlang}` is now directly imported.

* `{bsicons}` and `{lucidr}` are now suggested packages.

## New Features

* Added Lucide (<https://lucide.dev/>) icons, powered by `{lucidr}`.

# leaf.magic 0.1.0

## New Features

* `magicIcons()` allows users to use any up-to-date Font Awesome, Bootstrap, or Ionicons icon.

* `addIconLegend()` add a legend layer to a `{leaflet}` map with Font Awesome, Bootstrap, or Ionicons icons. 

* `iconFactor()`, `iconBin()`, and `iconQuantile()` exist as analogues to `leaflet::colorFactor()`, `leaflet::colorBin()` and `leaflet::colorQuantile()`.

* `awesomePalette` provides hex-codes used by `leaflet::awesomeMarkers()`.

* The `port_talbot` dataset is provided to demonstrate `{leaf.magic}` functionality.
