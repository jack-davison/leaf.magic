# Customising Markers

This article covers the many ways
[`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md)
markers can be customised. It uses the same `port_talbot` data and
scales as [Getting
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

site_types <- c("Urban Industrial", "Urban Background", "Urban Traffic")
iconPal <- iconFactor(c("house", "industry", "car"), domain = site_types)
catPal <- colorFactor(c("#12436D", "#28A197", "#801650"), domain = site_types)
```

## Icons

### Icon Libraries

By default,
[`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md)
uses Font Awesome icons, but the `library` argument lets you choose from
four icon libraries. Each is provided by a different package, which
you’ll need to have installed to use it:

| `library` | Icons | Package |
|----|----|----|
| `"fontawesome"` | <https://fontawesome.com/search?ic=free> | [fontawesome](https://github.com/rstudio/fontawesome) |
| `"bootstrap"` | <https://icons.getbootstrap.com/> | [bsicons](https://github.com/rstudio/bsicons) |
| `"lucide"` | <https://lucide.dev/icons/> | [lucidr](https://github.com/hyperverse-r/lucidr) |
| `"ionicons"` | <https://ionic.io/ionicons> | None - icons are downloaded when first used, so require an internet connection |

For example, here are some Lucide icons:

``` r

leaflet(port_talbot) |>
  addTiles() |>
  addMarkers(
    popup = ~popup,
    icon = magicIcons(
      icon = "factory",
      library = "lucide"
    )
  )
```

### Text Markers

Sometimes the best “icon” is a short piece of text, like a number or a
letter. Setting `library = "text"` shows `icon` as text inside the
marker. Text shrinks to fit as it gets longer, so it works best with up
to around three characters.

For example, we can label each site with the last two digits of the year
it opened:

``` r

yearPal <- colorNumeric("viridis", domain = port_talbot$open_year)

leaflet(port_talbot) |>
  addTiles() |>
  addMarkers(
    popup = ~popup,
    icon = magicIcons(
      icon = ~ format(start_date, "%y"),
      markerColor = ~ yearPal(open_year),
      marker = "circle",
      library = "text"
    )
  ) |>
  addLegend(
    pal = yearPal,
    values = ~open_year,
    title = "Opening Year",
    labFormat = labelFormat(big.mark = "")
  )
```

### Custom SVG Icons

If the icon you need isn’t in any of the icon libraries,
`library = "svg"` lets you provide your own icon as a string of SVG
markup. Any parts of the SVG which have no fill of their own (or use
`currentColor`) take on `iconColor`, while any hard-coded colours are
kept.

``` r

bolt <- '<svg viewBox="0 0 24 24"><path d="M13 2 4 14h7l-1 8 9-12h-7z"/></svg>'

leaflet(port_talbot) |>
  addTiles() |>
  addMarkers(
    popup = ~popup,
    icon = magicIcons(
      icon = bolt,
      markerColor = "#28A197",
      library = "svg"
    )
  )
```

## Colours

[`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md)
has two arguments related to colour; `markerColor` which colours the
marker, and `iconColor` which colours the icon. Any colour can be
supplied to these arguments - either a constant value or colour values
mapped to a column using, for example,
[`leaflet::colorFactor()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html).
`iconColor` defaults to `"auto"`, which picks black or white for each
marker, whichever contrasts more with its `markerColor`.

``` r

leaflet(port_talbot) |>
  addTiles() |>
  addMarkers(
    popup = ~popup,
    icon = magicIcons(
      icon = ~ iconPal(site_type),
      markerColor = ~ catPal(site_type),
      iconColor = "#F7F7F7"
    )
  ) |>
  addLegend(
    pal = catPal,
    values = ~site_type,
    title = "Site Type"
  )
```

[leaf.magic](https://jack-davison.github.io/leaf.magic/) also exports
the `awesomePalette` list, which are hex codes for the colours used in
[`leaflet::awesomeIcons()`](https://rstudio.github.io/leaflet/reference/awesomeIcons.html).
You could use these if you want to recreate the colour scheme of
[`awesomeIcons()`](https://rstudio.github.io/leaflet/reference/awesomeIcons.html)
with the flexibility of
[`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md).

``` r

cols <- names(awesomePalette)[names(awesomePalette) != "white"]
breweries91$facolor <- sample(cols, nrow(breweries91), replace = TRUE)
breweries91$hexcolor <- unlist(awesomePalette[breweries91$facolor], use.names = FALSE)

leaflet(breweries91) |>
  addTiles() |>
  addAwesomeMarkers(
    icon = ~ awesomeIcons(
      icon = "circle",
      markerColor = facolor,
      iconColor = "#FFFFFF",
      library = "fa"
    ),
    group = "Awesome"
  ) |>
  addMarkers(
    icon = magicIcons(
      icon = "circle",
      markerColor = ~hexcolor,
      iconColor = "#FFFFFF"
    ),
    group = "Magic"
  ) |>
  addLayersControl(
    baseGroups = c("Awesome", "Magic")
  )
```

## Size

Unlike
[`leaflet::awesomeIcons()`](https://rstudio.github.io/leaflet/reference/awesomeIcons.html),
[`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md)
can be easily re-sized. Much like in
[`leaflet::addCircleMarkers()`](https://rstudio.github.io/leaflet/reference/map-layers.html),
this can vary with another variable; let’s make the closed sites a bit
smaller than the open ones with `markerSize`, which sets the width of
the marker in pixels. Note the default is `30L`, which is roughly the
same size as the default
[`leaflet::addMarkers()`](https://rstudio.github.io/leaflet/reference/map-layers.html)
marker.

``` r

leaflet(port_talbot) |>
  addTiles() |>
  addMarkers(
    popup = ~popup,
    icon = magicIcons(
      icon = ~ iconPal(site_type),
      markerColor = ~ catPal(site_type),
      markerSize = ~ ifelse(open, 30L, 20L)
    )
  )
```

## Shapes

While the default marker type, the “tear-drop” marker, is useful in most
instances, you may find other marker shapes of interest. These are:

- **Pointed shapes**, which sit on their location with their tip, like
  the default marker: `"marker"`, `"marker-square"` (a pin with a square
  head) and `"callout"` (a speech bubble).

- **Centred shapes**, which sit centred on their location: `"circle"`,
  `"square"`, `"diamond"`, `"triangle"`, `"hexagon"`, `"star"` and
  `"heart"`.

- `"none"`, which removes the marker entirely and places the icon
  directly on the map itself. With no marker to contrast with, an
  `"auto"` `iconColor` takes on the `markerColor` instead.

Use the layer control in the map below to try them out.

``` r

shapes <- c(
  "marker", "marker-square", "callout", "circle", "square", "diamond",
  "triangle", "hexagon", "star", "heart", "none"
)

map <- leaflet(port_talbot) |>
  addTiles()

for (shape in shapes) {
  map <- map |>
    addMarkers(
      popup = ~popup,
      icon = magicIcons(
        icon = ~ iconPal(site_type),
        markerColor = ~ catPal(site_type),
        marker = shape
      ),
      group = shape
    )
}

map |>
  addLayersControl(
    baseGroups = shapes,
    options = layersControlOptions(FALSE)
  ) |>
  addIconLegend(
    position = "bottomleft",
    pal = iconPal,
    values = ~site_type,
    colors = catPal,
    title = "Site Type"
  )
```

Note that `marker` can vary with some variable, allowing the marker
shape itself to encode extra information. As marker shapes are just
names,
[`iconFactor()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
can map data to them. Passing that scale to the `markers` argument of
[`addIconLegend()`](https://jack-davison.github.io/leaf.magic/reference/addIconLegend.md)
draws the real marker shapes in the legend (see [Icon Scales &
Legends](https://jack-davison.github.io/leaf.magic/articles/scales-and-legends.html#marker-legends)
for more).

``` r

markPal <- iconFactor(c("circle", "diamond", "square"), site_types)

leaflet(port_talbot) |>
  addTiles() |>
  addMarkers(icon = magicIcons(
    marker = ~ markPal(site_type),
    markerSize = ~ ifelse(markPal(site_type) == "diamond", 35, 30)
  )) |>
  addIconLegend(
    markers = markPal,
    values = ~site_type,
    icons = "circle",
    title = "Site Type"
  )
```

### Custom Shapes

If none of the built-in shapes suit, you can design your own with
[`markerShape()`](https://jack-davison.github.io/leaf.magic/reference/markerShape.md).
A shape is drawn with SVG in its own coordinate system - by default, 100
units wide and 100 units tall - and you tell
[`markerShape()`](https://jack-davison.github.io/leaf.magic/reference/markerShape.md):

- `path`: the outline of the shape, as SVG path data or SVG markup.
- `icon`: the centre (`x`, `y`) and `size` of the square the icon is
  drawn in.
- `anchor`: the point of the shape that sits on the marker’s location.
- `popup`: where popups open from.

The shape is filled with `markerColor` and given a darker outline for
you. For example, here’s a downward-pointing arrow, anchored at its tip:

``` r

arrow <- markerShape(
  path = "M2 2h96L50 98z",
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
```

A custom shape can be passed to `marker` just like a built-in one. To
use a different shape for each marker, give `marker` a list of shapes
and/or shape names. Here, open sites use the arrow and closed sites use
a circle.

``` r

leaflet(port_talbot) |>
  addTiles() |>
  addMarkers(
    popup = ~popup,
    icon = magicIcons(
      icon = ~ iconPal(site_type),
      markerColor = ~ catPal(site_type),
      marker = ~ lapply(open, \(x) if (x) arrow else "circle")
    )
  ) |>
  addIconLegend(
    pal = iconPal,
    values = ~site_type,
    colors = catPal,
    title = "Site Type"
  )
```

## Shadows

Like the default [leaflet](https://rstudio.github.io/leaflet/) marker,
[leaf.magic](https://jack-davison.github.io/leaf.magic/) markers have
shadows. Pointed shapes cast a shadow up and to the right from their
tip, as if they were standing on the map, while other shapes have a soft
shadow just beneath them. Shadows are drawn underneath *all* of the
markers, so a marker’s shadow never covers another marker.

Shadows can be turned off with `shadow = FALSE`. Use the layer control
below to compare. For custom shapes, the `shadow` argument of
[`markerShape()`](https://jack-davison.github.io/leaf.magic/reference/markerShape.md)
chooses between a `"cast"` shadow, a `"drop"` shadow, or `"none"`.

``` r

addShadowMarkers <- function(map, shadow) {
  map |>
    addMarkers(
      popup = ~popup,
      icon = magicIcons(
        icon = ~ iconPal(site_type),
        markerColor = ~ catPal(site_type),
        shadow = shadow
      ),
      group = if (shadow) "With Shadows" else "Without Shadows"
    )
}

leaflet(port_talbot) |>
  addTiles() |>
  addShadowMarkers(TRUE) |>
  addShadowMarkers(FALSE) |>
  addLayersControl(
    baseGroups = c("With Shadows", "Without Shadows"),
    options = layersControlOptions(FALSE)
  )
```

## Outlines

Each marker has a thin outline in a slightly darker shade of its colour.
`outlineColor` and `outlineWidth` (in pixels) change this, which can
help markers stand out against a busy basemap.

``` r

leaflet(port_talbot) |>
  addTiles() |>
  addMarkers(
    popup = ~popup,
    icon = magicIcons(
      icon = ~ iconPal(site_type),
      markerColor = ~ catPal(site_type),
      marker = "circle",
      outlineColor = "white",
      outlineWidth = 2
    )
  )
```

## Accessibility

Markers are images, so screen readers can’t tell what they show.
[leaflet](https://rstudio.github.io/leaflet/)’s
[`markerOptions()`](https://rstudio.github.io/leaflet/reference/map-options.html)
can give each marker alternative text with `alt`, and a tooltip shown
when hovering with `title`. Both can be mapped to your data with a
formula, just like `popup`.

``` r

leaflet(port_talbot) |>
  addTiles() |>
  addMarkers(
    popup = ~popup,
    icon = magicIcons(
      icon = ~ iconPal(site_type),
      markerColor = ~ catPal(site_type)
    ),
    options = markerOptions(
      alt = ~ paste(site, "-", site_type),
      title = ~site
    )
  )
```

## Caching

Markers are saved to a temporary directory by default, which is cleared
at the end of each R session. Each marker is only built once per
session, so maps with many repeated markers are quick to draw. If you
build the same markers often (for example, in a Shiny app), the `dir`
argument of
[`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md)
can point to a permanent directory so that markers are only ever built
once.
