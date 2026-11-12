# leaf.magic (development version)

## Breaking Changes

* `iconFactor()`, `iconBin()` and `iconQuantile()` have been rewritten to behave identically to `leaflet::colorFactor()`, `leaflet::colorBin()` and `leaflet::colorQuantile()`. Given the same settings, each level/bin now receives a consistent icon-colour pair. In particular, `iconFactor()` now sorts character domains alphabetically (or uses factor levels) rather than using the order of appearance; use `ordered = TRUE` for the old behaviour. Similarly, `iconBin()` now uses `pretty = TRUE` by default, as `colorBin()` does, which may produce a different number of bins than icons; use `pretty = FALSE` for the old behaviour.

## New Features

* `iconFactor()` gains `levels`, `ordered` and `reverse` arguments; `iconBin()` gains `pretty` and `reverse`; and `iconQuantile()` gains `reverse`, all matching their `{leaflet}` equivalents. A `domain` of `NULL` is now also supported.

## Bug Fixes

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
