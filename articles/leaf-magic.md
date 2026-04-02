# Getting Started with {leaf.magic}

``` r
library(leaflet)
library(leaf.magic)
```

## Purpose

The purpose of [leaf.magic](https://jack-davison.github.io/leaf.magic/)
is to extend R [leaflet](https://rstudio.github.io/leaflet/)’s
capability to use modern icon sets. While
[`leaflet::addAwesomeMarkers()`](https://rstudio.github.io/leaflet/reference/addAwesomeMarkers.html)
exists, it has a few core disadvantages; markers are a fixed size, can
only be one of 19 colours, and pull icons from significantly out of date
icon sets.

[leaf.magic](https://jack-davison.github.io/leaf.magic/) uses
contemporary R packages - currently
[fontawesome](https://github.com/rstudio/fontawesome) and
[bsicons](https://github.com/rstudio/bsicons) - to access up-to-date SVG
icon sets, [magick](https://docs.ropensci.org/magick/) to knit them onto
map markers, and then
[`leaflet::makeIcon()`](https://rstudio.github.io/leaflet/reference/makeIcon.html)
to translate them into a
[leaflet](https://rstudio.github.io/leaflet/)-usable map marker.

Some core advantages of
[`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md)
over
[`leaflet::awesomeIcons()`](https://rstudio.github.io/leaflet/reference/awesomeIcons.html)
are:

- Markers pull from up-to-date, larger icon sets.

- Markers and icons can be any colour, and can be resized.

One *disadvantage* is that
[`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md)
may be slower initially to create large numbers of different icons.
However, once a marker has been created in an R session, it will be
significantly quicker to redraw, closing the gap between
[`awesomeIcons()`](https://rstudio.github.io/leaflet/reference/awesomeIcons.html)
and
[`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md).

## Example Data

In this document we’ll use the in-built `port_talbot` dataset, which
details the location of some air quality measurement stations around the
town of Port Talbot, Wales, UK, some of which are open and some of which
have closed.

We can give this data a look using vanilla
[leaflet](https://rstudio.github.io/leaflet/):

``` r
port_talbot$open_year <- as.integer(format(port_talbot$start_date, "%Y"))

port_talbot$popup <-
  paste0(
    "<strong>",
    toupper(port_talbot$site),
    "</strong> (",
    port_talbot$code,
    ")<hr>Site Type: ",
    port_talbot$site_type,
    "<br>Opened: ",
    port_talbot$open_year
  )

leaflet(port_talbot) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addMarkers(popup = ~popup)
```

## Simple Example

We can use the
[`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md)
function to swap out the default markers for some
[leaf.magic](https://jack-davison.github.io/leaf.magic/) markers. The
important argument here is `icon`, which is the (in this case) Font
Awesome icon of interest[¹](#fn1).

``` r
leaflet(port_talbot) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addMarkers(
    popup = ~popup,
    icon = magicIcons(
      icon = "cloud"
    )
  )
```

## Varying Icons

[leaf.magic](https://jack-davison.github.io/leaf.magic/) allows you to
vary icons by some other variable. You can do this manually, or use one
of [leaf.magic](https://jack-davison.github.io/leaf.magic/) constructor
functions like
[`iconFactor()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md).

``` r
site_types <- c("Urban Industrial", "Urban Background", "Urban Traffic")

iconPal <- iconFactor(
  icons = c("industry", "house", "car"),
  domain = site_types
)

leaflet(port_talbot) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addMarkers(
    popup = ~popup,
    icon = ~ magicIcons(
      icon = iconPal(site_type)
    )
  )
```

Note that there’s also
[`iconBin()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
and
[`iconQuantile()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
which can help map icons to numeric data. In this case, it makes most
sense to choose icons that have some inherent “order” to them.

These functions don’t have all of the features of
[`colorBin()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html)
and
[`colorQuantile()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html),
in part due to the differences between mapping a continuous aesthetic
like colour and a discrete aesthetic like icons. A nice feature is that
these are very much icon-led - the `breaks` and `n` arguments default to
the length of the provided `icons`, so you don’t need to specify them if
you’re happy letting the functions decide the break-points/`probs` for
you.

``` r
iconPalYr <- iconQuantile(
  icons = c("hourglass-start", "hourglass-half", "hourglass-end"),
  domain = port_talbot$open_year
)

leaflet(port_talbot) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addMarkers(
    popup = ~popup,
    icon = ~ magicIcons(
      icon = iconPalYr(open_year)
    )
  )
```

## Varying Colours

[`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md)
has two arguments related to colour; `markerColor` which colours the
tear-drop-shaped marker, and `iconColor` which colours the icon. Any
hex-code can be supplied to these arguments - either a constant value or
colour values mapped to a column using
[`leaflet::colorFactor()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html).

``` r
catPal <- colorFactor(c("#12436D", "#28A197", "#801650"), port_talbot$site_type)

leaflet(port_talbot) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addMarkers(
    popup = ~popup,
    icon = ~ magicIcons(
      icon = iconPal(site_type),
      markerColor = catPal(site_type),
      iconColor = "#F7F7F7FF"
    )
  ) |>
  addLegend(
    pal = catPal,
    values = port_talbot$site_type,
    title = "Site Type"
  )
```

As the markers themselves can be set to *any* colour, we can even use a
continuous scale (e.g., from
[`leaflet::colorNumeric()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html)).

``` r
palNum <- colorNumeric("viridis", domain = port_talbot$open_year)

leaflet(port_talbot) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addMarkers(
    popup = ~popup,
    icon = ~ magicIcons(
      icon = iconPalYr(open_year),
      markerColor = palNum(open_year),
      iconColor = "white"
    )
  ) |>
  addLegend(
    pal = palNum,
    values = port_talbot$open_year,
    title = "Opening Year",
    labFormat = labelFormat(big.mark = "")
  )
```

## Varying Size

Unlike
[`leaflet::awesomeIcons()`](https://rstudio.github.io/leaflet/reference/awesomeIcons.html),
[`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md)
can be easily re-sized. Much like in
[`leaflet::addCircleMarkers()`](https://rstudio.github.io/leaflet/reference/map-layers.html),
this can vary with another variable; let’s make the closed sites a bit
smaller than the open ones with `markerSize`. Note the default is `30L`,
which is roughly the same size as the default
[`leaflet::addMarkers()`](https://rstudio.github.io/leaflet/reference/map-layers.html)
marker.

``` r
leaflet(port_talbot) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addMarkers(
    popup = ~popup,
    icon = ~ magicIcons(
      icon = iconPal(site_type),
      iconColor = "white",
      markerSize = ifelse(open, 30L, 20L),
      markerColor = catPal(site_type)
    )
  ) |>
  addLegend(
    pal = catPal,
    values = port_talbot$site_type,
    title = "Site Type"
  )
```

## Icon Legends

[leaf.magic](https://jack-davison.github.io/leaf.magic/) provides the
[`addIconLegend()`](https://jack-davison.github.io/leaf.magic/reference/addIconLegend.md)
function to help construct an icon legend, similar to
[`leaflet::addLegend()`](https://rstudio.github.io/leaflet/reference/addLegend.html)
constructs colour legends.

``` r
leaflet(port_talbot) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addMarkers(
    popup = ~popup,
    icon = ~ magicIcons(
      icon = iconPal(site_type),
      markerColor = palNum(open_year),
      iconColor = "white"
    )
  ) |>
  addLegend(
    pal = palNum,
    values = port_talbot$open_year,
    title = "Opening Year",
    labFormat = labelFormat(big.mark = "")
  ) |>
  addIconLegend(
    icons = c("industry", "car", "house"),
    labels = site_types,
    title = "Site Type"
  )
```

Often, the colour of the marker (or icon) and the icon itself will
align. In that case, the `colors` argument of
[`addIconLegend()`](https://jack-davison.github.io/leaf.magic/reference/addIconLegend.md)
can be used to create an efficient, combined legend.

``` r
leaflet(port_talbot) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addMarkers(
    popup = ~popup,
    icon = ~ magicIcons(
      icon = iconPal(site_type),
      iconColor = "white",
      markerColor = catPal(site_type)
    )
  ) |>
  addIconLegend(
    icons = iconPal(site_types),
    labels = site_types,
    colors = catPal(site_types),
    title = "Site Type"
  )
```

## Alternative Marker Types

While the default marker type, the “tear-drop” marker, is useful in most
instances, you may find other markers types of interest. These include
circle, square, diamond, heart, and star-shaped markers. Additionally,
users can specify `"none"` and remove the marker entirely, placing the
icon directly on the map itself.

``` r
addQuickMarker <- function(map, marker) {
  iconColor <- "white"
  markerColor <- catPal(port_talbot$site_type)
  if (marker == "none") {
    iconColor <- markerColor
  }
  map |>
    addMarkers(
      popup = ~ popup,
      icon = ~ magicIcons(
        icon = iconPal(site_type),
        iconColor = iconColor,
        markerColor = markerColor,
        marker = marker
      ),
      group = marker
    )
}

leaflet(port_talbot) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addQuickMarker("circle") |>
  addQuickMarker("square") |>
  addQuickMarker("diamond") |>
  addQuickMarker("heart") |>
  addQuickMarker("star") |>
  addQuickMarker("none") |>
  addLayersControl(
    baseGroups = c("circle", "square", "diamond", "heart", "star", "none"),
    options = layersControlOptions(FALSE)
  ) |>
  addIconLegend(
    position = "bottomleft",
    icons = iconPal(site_types),
    labels = site_types,
    colors = catPal(site_types),
    title = "Site Type"
  )
```

Note that `marker` can vary with some variable, allowing the marker
style itself to encode extra information.

``` r
markPal <- iconFactor(c("circle", "diamond", "square"), site_types)

leaflet(port_talbot) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addMarkers(icon = ~ magicIcons(
    marker = markPal(site_type),
    markerSize = ifelse(markPal(site_type) == "diamond", 35, 30)
  )) |>
  addIconLegend(
    title = "Site Type",
    icons = c("fas fa-circle", "fas fa-diamond", "fas fa-square"),
    site_types
  )
```

## Other Utilities

[leaf.magic](https://jack-davison.github.io/leaf.magic/) exports the
`awesomePalette` list, which are hex codes for the colours used in
[`leaflet::awesomeIcons()`](https://rstudio.github.io/leaflet/reference/awesomeIcons.html).
You could use these if you want to recreate the colour scheme of
[`awesomeIcons()`](https://rstudio.github.io/leaflet/reference/awesomeIcons.html)
with the flexibility of
[`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md).

``` r
cols <- names(awesomePalette)[names(awesomePalette) != "white"]

breweries91$facolor <- sample(cols, nrow(breweries91), replace = TRUE)

breweries91$hexcolor <- unlist(use.names = FALSE, awesomePalette[breweries91$facolor])

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
    icon = ~ magicIcons(
      icon = "fas fa-circle",
      markerColor = hexcolor,
      iconColor = "#FFFFFF"
    ),
    group = "Magic"
  ) |>
  addLayersControl(
    baseGroups = c("Awesome", "Magic")
  )
```

If you use [leaf.magic](https://jack-davison.github.io/leaf.magic/), you
may want to add an attribution to Font Awesome or one of the other icon
providers.
[`addIconAttribution()`](https://jack-davison.github.io/leaf.magic/reference/addIconAttribution.md)
is a convenient way to do this, and even comes with its own `group` and
`layerId` options for precise “layer” control. Try toggling the
breweries on and off in the example below.

``` r
leaflet(breweries91) |>
  addTiles() |>
  addMarkers(icon = magicIcons("beer", "red", "white"), group = "Breweries") |>
  addIconAttribution(group = "Breweries") |>
  addLayersControl(overlayGroups = "Breweries",
                   options = leaflet::layersControlOptions(FALSE))
```

------------------------------------------------------------------------

1.  See
    [`fontawesome::fa_metadata()`](https://rstudio.github.io/fontawesome/reference/fa_metadata.html)
    for a complete list
