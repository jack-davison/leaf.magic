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
