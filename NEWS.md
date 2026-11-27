# leaf.magic 0.3.0

## Breaking changes

* `magicIcons()` now builds each marker as a single SVG rather than compositing PNGs, so markers are sharp at any size and icons are precisely centred. Marker shapes are now drawn by `{leaf.magic}` itself, so look slightly different.

* `magicIcons()`'s `iconColor` now defaults to `"auto"`, which picks black or white icons depending on the marker colour, rather than always white. Use `iconColor = "white"` for the old behaviour.

* `iconFactor()`, `iconBin()` and `iconQuantile()` now behave identically to their `leaflet::color*()` equivalents, so the same settings always give matching icon-colour pairs. This changes two defaults:
  * `iconFactor()` sorts levels alphabetically (or uses factor levels) rather than by order of appearance. Use `ordered = TRUE` for the old behaviour.
  * `iconBin()` uses `pretty = TRUE`, which may change the number of bins. Use `pretty = FALSE` for the old behaviour.

* All icon libraries are now optional: `{fontawesome}` has moved to Suggests alongside `{bsicons}` and `{lucidr}`. `{magick}` and `{rsvg}` are no longer needed.

## New features

* `magicIcons()` now accepts formulas (e.g., `icon = ~site_type`), evaluated against the layer's data as in `leaflet::awesomeIcons()`.

* Icons:
  * `magicIcons(library = "text")` shows text or numbers in markers instead of icons, e.g., for numbered stops.
  * `magicIcons(library = "svg")` uses raw SVG markup as the icon, so any icon can be used.
  * `magicIcons(icon = NA)` draws the marker shape with no icon, e.g., when the shape alone carries the meaning. Likewise, `marker = NA` draws the icon with no marker, like `"none"`.

* Markers:
  * `magicIcons(iconColor = "auto")` picks black or white icons, whichever contrasts more with each marker.
  * `magicIcons()` gains `outlineColor` and `outlineWidth` to style the outline around each marker, e.g., a white halo to stand out on busy basemaps.
  * `magicIcons()` markers now cast shadows like `{leaflet}`'s default marker; turn them off with `shadow = FALSE` (#12).
  * New marker shapes: `"marker-square"`, `"callout"`, `"triangle"` and `"hexagon"`.
  * New `markerShape()` defines custom SVG marker shapes, including icon position, anchor point, popup position and shadow style. Pass them to the `marker` argument of `magicIcons()`.

* Legends:
  * `addIconLegend()` can now be built directly from an icon scale using `pal` and `values`, like `leaflet::addLegend()` (#10). `colors` may also be a `{leaflet}` colour scale.
  * `addIconLegend()` gains `na.label`, `labFormat`, `group` and `opacity` arguments, as in `leaflet::addLegend()`.
  * `addIconLegend()` gains `markers` to draw each item as the marker used on the map (shape, colour and icon) rather than a bare icon, and `iconColors` to colour icons within markers. `markers` may also be an icon scale mapping data to shapes, which can drive the legend on its own to show shapes alone.

* Icon scales:
  * `iconFactor()` gains `levels`, `ordered` and `reverse`; `iconBin()` gains `pretty` and `reverse`; `iconQuantile()` gains `reverse`.
  * All three now accept `domain = NULL`.

## Bug fixes

* `magicIcons()`:
  * Outlined Ionicons (e.g., `"car-outline"`) now use `iconColor` rather than always being black.
  * Cached markers are now invalidated when `{leaf.magic}` is upgraded, work with any icon name, and are shared between equivalent colours (e.g., `"red"` and `"#FF0000"`).
  * `dir` is created if it doesn't exist.
  * Colours which R understands but CSS doesn't (e.g., `"grey40"`) are no longer drawn as black.

* `addIconLegend()`:
  * Icons and labels are now spaced correctly.
  * Legends for `iconBin()` scales without a `domain` now always match the map.
  * The missing value item is now grey, as in `{leaflet}`, when `colors` is a single colour.

* Icon scales:
  * `iconBin()` and `iconQuantile()` no longer assign icons to the wrong bins when `domain` is unsorted or a bin is empty.
  * `iconBin()` now accepts a vector of break points for `bins`.
  * `iconFactor()` now matches factors by label rather than integer code, and allows the same icon for multiple levels.

* Ionicons are now downloaded once per session, and failed downloads give an informative error.

* `addIconAttribution()` no longer makes a network request for every map tile.

* `magicIcons()` and `addIconLegend()` now error informatively on mismatched argument lengths, rather than silently recycling. `addIconLegend()` also errors informatively if `icons` or `labels` is missing.

## Dependencies

* `{htmltools}`, `{grDevices}` and `{stats}` are now imported.

* `{purrr}` now requires 1.0.0 or later, `{rlang}` 1.0.0 or later, and `{dplyr}` (used in examples) 1.2.0 or later.

# leaf.magic 0.2.0

## Dependencies

* `{rlang}` is now directly imported.

* `{bsicons}` and `{lucidr}` are now suggested packages.

## New features

* Added Lucide (<https://lucide.dev/>) icons, powered by `{lucidr}`.

# leaf.magic 0.1.0

## New features

* `magicIcons()` allows users to use any up-to-date Font Awesome, Bootstrap, or Ionicons icon.

* `addIconLegend()` adds a legend layer to a `{leaflet}` map with Font Awesome, Bootstrap, or Ionicons icons.

* `iconFactor()`, `iconBin()`, and `iconQuantile()` exist as analogues to `leaflet::colorFactor()`, `leaflet::colorBin()` and `leaflet::colorQuantile()`.

* `awesomePalette` provides hex-codes used by `leaflet::awesomeMarkers()`.

* The `port_talbot` dataset is provided to demonstrate `{leaf.magic}` functionality.
