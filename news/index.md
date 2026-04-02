# Changelog

## leaf.magic 0.2.0

### Dependency Changes

- [rlang](https://rlang.r-lib.org) is now directly imported.

- [bsicons](https://github.com/rstudio/bsicons) and
  [lucidr](https://github.com/hyperverse-r/lucidr) are now suggested
  packages.

### New Features

- Added Lucide (<https://lucide.dev/>) icons, powered by
  [lucidr](https://github.com/hyperverse-r/lucidr).

## leaf.magic 0.1.0

### New Features

- [`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md)
  allows users to use any up-to-date Font Awesome, Bootstrap, or
  Ionicons icon.

- [`addIconLegend()`](https://jack-davison.github.io/leaf.magic/reference/addIconLegend.md)
  add a legend layer to a [leaflet](https://rstudio.github.io/leaflet/)
  map with Font Awesome, Bootstrap, or Ionicons icons.

- [`iconFactor()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md),
  `iconBins()`, and
  [`iconQuantile()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
  exist as analogues to
  [`leaflet::colorFactor()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html),
  `leaflet::colorBins()` and
  [`leaflet::colorQuantile()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html).

- `awesomePalette` provides hex-codes used by
  `leaflet::awesomeMarkers()`.

- The `port_talbot` dataset is provided to demonstrate
  [leaf.magic](https://jack-davison.github.io/leaf.magic/)
  functionality.
