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

[leaf.magic](https://jack-davison.github.io/leaf.magic/) takes icons
from up-to-date icon libraries - Font Awesome, Bootstrap, Lucide and
Ionicons - and nests them inside its own SVG marker shapes, which
[`leaflet::makeIcon()`](https://rstudio.github.io/leaflet/reference/makeIcon.html)
then turns into [leaflet](https://rstudio.github.io/leaflet/)-usable map
markers.

Some core advantages of
[`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md)
over
[`leaflet::awesomeIcons()`](https://rstudio.github.io/leaflet/reference/awesomeIcons.html)
are:

- Markers pull from up-to-date, larger icon sets - or use your own text
  and SVG icons.

- Markers and icons can be any colour, and can be resized. As markers
  are vector images, they stay sharp at any size and on high resolution
  screens.

- Markers come in a range of shapes - and you can design your own.

- Icons can be mapped to data with
  [`iconFactor()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md),
  [`iconBin()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
  and
  [`iconQuantile()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md),
  and explained with
  [`addIconLegend()`](https://jack-davison.github.io/leaf.magic/reference/addIconLegend.md),
  just like colours in [leaflet](https://rstudio.github.io/leaflet/).

This article is a quick tour. For more detail, see:

- [Customising
  Markers](https://jack-davison.github.io/leaf.magic/articles/markers.html):
  icon libraries, text and SVG icons, colours, sizes, shapes, shadows,
  outlines and accessibility.

- [Icon Scales &
  Legends](https://jack-davison.github.io/leaf.magic/articles/scales-and-legends.html):
  mapping data to icons, and explaining them with legends.

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
  addTiles() |>
  addMarkers(popup = ~popup)
```

## Your First Magic Markers

[`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md)
swaps out the default markers for
[leaf.magic](https://jack-davison.github.io/leaf.magic/) markers. Pass
it to the `icon` argument of
[`leaflet::addMarkers()`](https://rstudio.github.io/leaflet/reference/map-layers.html).
The most important argument is `icon`, which is the (by default, Font
Awesome) icon of interest[^1].

``` r

leaflet(port_talbot) |>
  addTiles() |>
  addMarkers(
    popup = ~popup,
    icon = magicIcons(
      icon = "cloud",
      markerColor = "#12436D"
    )
  )
```

## Mapping Data to Markers

Like
[`leaflet::awesomeIcons()`](https://rstudio.github.io/leaflet/reference/awesomeIcons.html),
any of the arguments of
[`magicIcons()`](https://jack-davison.github.io/leaf.magic/reference/magicIcons.md)
can be a formula, which is evaluated using the layer’s data. Combined
with an icon scale like
[`iconFactor()`](https://jack-davison.github.io/leaf.magic/reference/icon-scales.md)
and a colour scale like
[`leaflet::colorFactor()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html),
this lets markers show what kind of site each one is:

``` r

site_types <- c("Urban Industrial", "Urban Background", "Urban Traffic")

iconPal <- iconFactor(c("house", "industry", "car"), domain = site_types)
catPal <- colorFactor(c("#12436D", "#28A197", "#801650"), domain = site_types)

leaflet(port_talbot) |>
  addTiles() |>
  addMarkers(
    popup = ~popup,
    icon = magicIcons(
      icon = ~ iconPal(site_type),
      markerColor = ~ catPal(site_type)
    )
  )
```

You might have noticed we didn’t choose an `iconColor`. It defaults to
`"auto"`, which picks black or white for each marker - whichever
contrasts more with its `markerColor`. This really helps with continuous
colour scales, where no single icon colour would suit every marker:

``` r

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
  )
```

## Adding a Legend

[`addIconLegend()`](https://jack-davison.github.io/leaf.magic/reference/addIconLegend.md)
works just like
[`leaflet::addLegend()`](https://rstudio.github.io/leaflet/reference/addLegend.html):
give it the icon scale as `pal` along with the `values` used on the map.
Pass a matching colour scale to `colors` to show icons and colours
together in a single legend.

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

## Next Steps

That’s the basics! There’s lots more that
[leaf.magic](https://jack-davison.github.io/leaf.magic/) can do:

- [Customising
  Markers](https://jack-davison.github.io/leaf.magic/articles/markers.html)
  covers using other icon libraries, text and your own SVG icons,
  resizing markers, marker shapes (including designing your own),
  shadows, outlines, and making markers accessible.

- [Icon Scales &
  Legends](https://jack-davison.github.io/leaf.magic/articles/scales-and-legends.html)
  covers mapping numeric data to icons, the different ways of building
  legends, and attributing icon libraries.

[^1]: See
    [`fontawesome::fa_metadata()`](https://rstudio.github.io/fontawesome/reference/fa_metadata.html)
    for a complete list
