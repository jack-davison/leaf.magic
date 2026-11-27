# Icon Scales & Legends

This article covers mapping data to icons, and explaining those icons
with legends. It uses the same `port_talbot` data as [Getting
Started](https://jack-davison.github.io/leaf.magic/articles/leaf-magic.md):

``` r

library(leaflet)
library(leaf.magic)

port_talbot$open_year <- as.integer(format(port_talbot$start_date, "%Y"))
port_talbot$popup <- paste0(
  "<strong>", toupper(port_talbot$site), "</strong> (", port_talbot$code, ")",
  "<hr>Site Type: ", port_talbot$site_type,
  "<br>Opened: ", port_talbot$open_year
)
```

## Icon Scales

[leaf.magic](https://jack-davison.github.io/leaf.magic/) has three
functions for mapping data to icons, which are the icon equivalents of
[leaflet](https://rstudio.github.io/leaflet/)’s colour functions:

| Icons | Colours | Data |
|----|----|----|
| [`iconFactor()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md) | [`colorFactor()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html) | Categories |
| [`iconBin()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md) | [`colorBin()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html) | Numbers, split into bins |
| [`iconQuantile()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md) | [`colorQuantile()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html) | Numbers, split into quantiles |

Each takes some `icons` and a `domain`, and returns a function which
turns data into icon names.

``` r

site_types <- c("Urban Industrial", "Urban Background", "Urban Traffic")

# like leaflet::colorFactor(), levels are sorted alphabetically:
# "Urban Background", "Urban Industrial", "Urban Traffic"
iconPal <- iconFactor(
  icons = c("house", "industry", "car"),
  domain = site_types
)

iconPal(c("Urban Traffic", "Urban Background"))
#> [1] "car"   "house"
```

``` r

leaflet(port_talbot) |>
  addTiles() |>
  addMarkers(
    popup = ~popup,
    icon = magicIcons(
      icon = ~ iconPal(site_type)
    )
  )
```

[`iconBin()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
and
[`iconQuantile()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
map numeric data to icons. In this case, it makes most sense to choose
icons that have some inherent “order” to them.

These functions take the same arguments as
[`colorFactor()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html),
[`colorBin()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html)
and
[`colorQuantile()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html)
and split the data in exactly the same way, so if you give an icon
function and a colour function the same settings (and the same number of
icons as colours), every level gets a consistent icon-colour pair. They
are also icon-led - the `bins` and `n` arguments default to the length
of the provided `icons`, so you don’t need to specify them if you’re
happy letting the functions decide the break-points/`probs` for you.

``` r

iconPalYr <- iconQuantile(
  icons = c("hourglass-start", "hourglass-half", "hourglass-end"),
  domain = port_talbot$open_year
)

leaflet(port_talbot) |>
  addTiles() |>
  addMarkers(
    popup = ~popup,
    icon = magicIcons(
      icon = ~ iconPalYr(open_year)
    )
  )
```

Icon scales don’t just work for icons - they map data to any set of
names. For example, they can map data to [marker
shapes](https://jack-davison.github.io/leaf.magic/articles/markers.html#shapes),
or to text for [text
markers](https://jack-davison.github.io/leaf.magic/articles/markers.html#text-markers).

## Icon Legends

[`addIconLegend()`](https://jack-davison.github.io/leaf.magic/reference/addIconLegend.md)
constructs icon legends. It works just like
[`leaflet::addLegend()`](https://rstudio.github.io/leaflet/reference/addLegend.html):
give it an icon scale as `pal` along with the `values` used on the map,
and it works out the icons and labels for you. Icon and colour legends
can be used side-by-side.

``` r

catPal <- colorFactor(c("#12436D", "#28A197", "#801650"), domain = site_types)
yearPal <- colorNumeric("viridis", domain = port_talbot$open_year)

leaflet(port_talbot) |>
  addTiles() |>
  addMarkers(
    popup = ~popup,
    icon = magicIcons(
      icon = ~ iconPal(site_type),
      markerColor = ~ yearPal(open_year)
    )
  ) |>
  addLegend(
    pal = yearPal,
    values = ~open_year,
    title = "Opening Year",
    labFormat = labelFormat(big.mark = "")
  ) |>
  addIconLegend(
    pal = iconPal,
    values = ~site_type,
    title = "Site Type"
  )
```

### Combined Legends

Often, the colour of the marker (or icon) and the icon itself will
align. In that case, a colour scale can be passed to the `colors`
argument of
[`addIconLegend()`](https://jack-davison.github.io/leaf.magic/reference/addIconLegend.md)
to create an efficient, combined legend. As long as the icon and colour
scales share the same settings, every icon is paired with the right
colour.

``` r

leaflet(port_talbot) |>
  addTiles() |>
  addMarkers(
    popup = ~popup,
    icon = magicIcons(
      icon = ~ iconPal(site_type),
      markerColor = ~ catPal(site_type)
    )
  ) |>
  addIconLegend(
    pal = iconPal,
    values = ~site_type,
    colors = catPal,
    title = "Site Type"
  )
```

This also works for numeric scales; for example, an
[`iconQuantile()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
scale gives one legend item per quantile.

``` r

# colorQuantile() defaults to n = 4, so match the 3 icons of iconPalYr
colPalYr <- colorQuantile(
  c("#12436D", "#28A197", "#801650"),
  domain = port_talbot$open_year,
  n = 3
)

leaflet(port_talbot) |>
  addTiles() |>
  addMarkers(
    popup = ~popup,
    icon = magicIcons(
      icon = ~ iconPalYr(open_year),
      markerColor = ~ colPalYr(open_year)
    )
  ) |>
  addIconLegend(
    pal = iconPalYr,
    values = ~open_year,
    colors = colPalYr,
    title = "Opening Year"
  )
```

### Marker Legends

By default, legends show bare icons in `colors`. Use `markers` to draw
each item as a small version of the marker on the map instead, with the
shape filled with `colors` and the icon on top, coloured exactly as in
[`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md).

``` r

leaflet(port_talbot) |>
  addTiles() |>
  addMarkers(
    popup = ~popup,
    icon = magicIcons(
      icon = ~ iconPal(site_type),
      markerColor = ~ catPal(site_type)
    )
  ) |>
  addIconLegend(
    pal = iconPal,
    values = ~site_type,
    colors = catPal,
    markers = "marker",
    title = "Site Type"
  )
```

Because icon scales map values to any string, they can map data to
marker *shapes* too. When the shape is the only thing mapped to a
variable, pass its scale to `markers` and leave out `pal`: the legend
then shows the shapes alone. Below, the icon and colour show the site
type, while the shape shows whether the site is still open.

``` r

statusPal <- iconFactor(c("square", "circle"), domain = c(FALSE, TRUE))

leaflet(port_talbot) |>
  addTiles() |>
  addMarkers(
    popup = ~popup,
    icon = magicIcons(
      icon = ~ iconPal(site_type),
      markerColor = ~ catPal(site_type),
      marker = ~ statusPal(open)
    )
  ) |>
  addIconLegend(
    pal = iconPal,
    values = ~site_type,
    colors = catPal,
    title = "Site Type"
  ) |>
  addIconLegend(
    markers = statusPal,
    values = ~open,
    labels = c("Closed", "Open"),
    colors = "grey40",
    title = "Status"
  )
```

### Manual Legends

Legends can also be built by hand with the `icons`, `labels` and
`colors` arguments, and - like
[`leaflet::addLegend()`](https://rstudio.github.io/leaflet/reference/addLegend.html) -
shown and hidden alongside a layer using `group`.

``` r

leaflet(port_talbot) |>
  addTiles() |>
  addMarkers(
    popup = ~popup,
    icon = magicIcons(
      icon = ~ iconPal(site_type),
      markerColor = ~ catPal(site_type)
    ),
    group = "Sites"
  ) |>
  addIconLegend(
    icons = c("industry", "car", "house"),
    labels = c("Industrial", "Traffic", "Background"),
    colors = c("#28A197", "#801650", "#12436D"),
    title = "Site Type",
    group = "Sites"
  ) |>
  addLayersControl(
    overlayGroups = "Sites",
    options = layersControlOptions(FALSE)
  )
```

## Attribution

If you use [leaf.magic](https://jack-davison.github.io/leaf.magic/), you
may want to add an attribution to Font Awesome or one of the other icon
providers (set using `library`).
[`addIconAttribution()`](https://jack-davison.github.io/leaf.magic/reference/addIconAttribution.md)
is a convenient way to do this, and even comes with its own `group` and
`layerId` options for precise “layer” control. Try toggling the
breweries on and off in the example below.

``` r

leaflet(breweries91) |>
  addTiles() |>
  addMarkers(icon = magicIcons("beer", "red"), group = "Breweries") |>
  addIconAttribution(group = "Breweries") |>
  addLayersControl(
    overlayGroups = "Breweries",
    options = layersControlOptions(FALSE)
  )
```
