# Changelog

## leaf.magic 0.3.0

### Breaking changes

- [`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md)
  now builds each marker as a single SVG rather than compositing PNGs,
  so markers are sharp at any size and icons are precisely centred.
  Marker shapes are now drawn by
  [leaf.magic](https://jack-davison.github.io/leaf.magic/) itself, so
  look slightly different.

- [`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md)’s
  `iconColor` now defaults to `"auto"`, which picks black or white icons
  depending on the marker colour, rather than always white. Use
  `iconColor = "white"` for the old behaviour.

- [`iconFactor()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md),
  [`iconBin()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
  and
  [`iconQuantile()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
  now behave identically to their `leaflet::color*()` equivalents, so
  the same settings always give matching icon-colour pairs. This changes
  two defaults:

  - [`iconFactor()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
    sorts levels alphabetically (or uses factor levels) rather than by
    order of appearance. Use `ordered = TRUE` for the old behaviour.
  - [`iconBin()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
    uses `pretty = TRUE`, which may change the number of bins. Use
    `pretty = FALSE` for the old behaviour.

- All icon libraries are now optional:
  [fontawesome](https://github.com/rstudio/fontawesome) has moved to
  Suggests alongside [bsicons](https://github.com/rstudio/bsicons) and
  [lucidr](https://github.com/hyperverse-r/lucidr).
  [magick](https://docs.ropensci.org/magick/) and
  [rsvg](https://docs.ropensci.org/rsvg/) are no longer needed.

### New features

- [`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md)
  now accepts formulas (e.g., `icon = ~site_type`), evaluated against
  the layer’s data as in
  [`leaflet::awesomeIcons()`](https://rstudio.github.io/leaflet/reference/awesomeIcons.html).

- Icons:

  - `magicIcons(library = "text")` shows text or numbers in markers
    instead of icons, e.g., for numbered stops.
  - `magicIcons(library = "svg")` uses raw SVG markup as the icon, so
    any icon can be used.
  - `magicIcons(icon = NA)` draws the marker shape with no icon, e.g.,
    when the shape alone carries the meaning. Likewise, `marker = NA`
    draws the icon with no marker, like `"none"`.

- Markers:

  - `magicIcons(iconColor = "auto")` picks black or white icons,
    whichever contrasts more with each marker.
  - [`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md)
    gains `outlineColor` and `outlineWidth` to style the outline around
    each marker, e.g., a white halo to stand out on busy basemaps.
  - [`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md)
    markers now cast shadows like
    [leaflet](https://rstudio.github.io/leaflet/)’s default marker; turn
    them off with `shadow = FALSE`
    ([\#12](https://github.com/jack-davison/leaf.magic/issues/12)).
  - New marker shapes: `"marker-square"`, `"callout"`, `"triangle"` and
    `"hexagon"`.
  - New
    [`markerShape()`](https://jack-davison.github.io/leaf.magic/reference/markerShape.md)
    defines custom SVG marker shapes, including icon position, anchor
    point, popup position and shadow style. Pass them to the `marker`
    argument of
    [`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md).

- Legends:

  - [`addIconLegend()`](https://jack-davison.github.io/leaf.magic/reference/addIconLegend.md)
    can now be built directly from an icon scale using `pal` and
    `values`, like
    [`leaflet::addLegend()`](https://rstudio.github.io/leaflet/reference/addLegend.html)
    ([\#10](https://github.com/jack-davison/leaf.magic/issues/10)).
    `colors` may also be a [leaflet](https://rstudio.github.io/leaflet/)
    colour scale.
  - [`addIconLegend()`](https://jack-davison.github.io/leaf.magic/reference/addIconLegend.md)
    gains `na.label`, `labFormat`, `group` and `opacity` arguments, as
    in
    [`leaflet::addLegend()`](https://rstudio.github.io/leaflet/reference/addLegend.html).
  - [`addIconLegend()`](https://jack-davison.github.io/leaf.magic/reference/addIconLegend.md)
    gains `markers` to draw each item as the marker used on the map
    (shape, colour and icon) rather than a bare icon, and `iconColors`
    to colour icons within markers. `markers` may also be an icon scale
    mapping data to shapes, which can drive the legend on its own to
    show shapes alone.

- Icon scales:

  - [`iconFactor()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
    gains `levels`, `ordered` and `reverse`;
    [`iconBin()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
    gains `pretty` and `reverse`;
    [`iconQuantile()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
    gains `reverse`.
  - All three now accept `domain = NULL`.
  - `na.icon` now defaults to `"auto"`, which
    [`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md)
    and
    [`addIconLegend()`](https://jack-davison.github.io/leaf.magic/reference/addIconLegend.md)
    draw as a question mark from whichever icon library is in use
    (previously always the Font Awesome `"question"` icon, which failed
    in other libraries). When a scale maps to marker shapes, `"auto"` is
    drawn as `"none"`.

### Bug fixes

- [`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md):

  - Outlined Ionicons (e.g., `"car-outline"`) now use `iconColor` rather
    than always being black.
  - Cached markers are now invalidated when
    [leaf.magic](https://jack-davison.github.io/leaf.magic/) is
    upgraded, work with any icon name, and are shared between equivalent
    colours (e.g., `"red"` and `"#FF0000"`).
  - `dir` is created if it doesn’t exist.
  - Colours which R understands but CSS doesn’t (e.g., `"grey40"`) are
    no longer drawn as black.

- [`addIconLegend()`](https://jack-davison.github.io/leaf.magic/reference/addIconLegend.md):

  - Icons and labels are now spaced correctly.
  - Legends for
    [`iconBin()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
    scales without a `domain` now always match the map.
  - The missing value item is now grey, as in
    [leaflet](https://rstudio.github.io/leaflet/), when `colors` is a
    single colour.

- Icon scales:

  - [`iconBin()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
    and
    [`iconQuantile()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
    no longer assign icons to the wrong bins when `domain` is unsorted
    or a bin is empty.
  - [`iconBin()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
    now accepts a vector of break points for `bins`.
  - [`iconFactor()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
    now matches factors by label rather than integer code, and allows
    the same icon for multiple levels.

- Ionicons are now downloaded once per session, and failed downloads
  give an informative error.

- [`addIconAttribution()`](https://jack-davison.github.io/leaf.magic/reference/addIconAttribution.md)
  no longer makes a network request for every map tile.

- [`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md)
  and
  [`addIconLegend()`](https://jack-davison.github.io/leaf.magic/reference/addIconLegend.md)
  now error informatively on mismatched argument lengths, rather than
  silently recycling.
  [`addIconLegend()`](https://jack-davison.github.io/leaf.magic/reference/addIconLegend.md)
  also errors informatively if `icons` or `labels` is missing.

### Dependencies

- [htmltools](https://github.com/rstudio/htmltools), `{grDevices}` and
  `{stats}` are now imported.

- [purrr](https://purrr.tidyverse.org/) now requires 1.0.0 or later,
  [rlang](https://rlang.r-lib.org) 1.0.0 or later, and
  [dplyr](https://dplyr.tidyverse.org) (used in examples) 1.2.0 or
  later.

## leaf.magic 0.2.0

### Dependencies

- [rlang](https://rlang.r-lib.org) is now directly imported.

- [bsicons](https://github.com/rstudio/bsicons) and
  [lucidr](https://github.com/hyperverse-r/lucidr) are now suggested
  packages.

### New features

- Added Lucide (<https://lucide.dev/>) icons, powered by
  [lucidr](https://github.com/hyperverse-r/lucidr).

## leaf.magic 0.1.0

### New features

- [`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md)
  allows users to use any up-to-date Font Awesome, Bootstrap, or
  Ionicons icon.

- [`addIconLegend()`](https://jack-davison.github.io/leaf.magic/reference/addIconLegend.md)
  adds a legend layer to a [leaflet](https://rstudio.github.io/leaflet/)
  map with Font Awesome, Bootstrap, or Ionicons icons.

- [`iconFactor()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md),
  [`iconBin()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md),
  and
  [`iconQuantile()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
  exist as analogues to
  [`leaflet::colorFactor()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html),
  [`leaflet::colorBin()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html)
  and
  [`leaflet::colorQuantile()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html).

- `awesomePalette` provides hex-codes used by
  `leaflet::awesomeMarkers()`.

- The `port_talbot` dataset is provided to demonstrate
  [leaf.magic](https://jack-davison.github.io/leaf.magic/)
  functionality.
