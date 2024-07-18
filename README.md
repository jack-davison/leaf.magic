
# {leaf.magick} - Use Modern Icon Libraries in Leaflet Markers <a href="https://jack-davison.github.io/leaf.magick/"><img src="man/figures/logo.png" align="right" height="138" alt="leaf.magick website" /></a>

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/leaf.magick)](https://CRAN.R-project.org/package=leaf.magick)
[![R-CMD-check](https://github.com/jack-davison/leaf.magick/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jack-davison/leaf.magick/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `{leaf.magick}` is to overcome the somewhat outdated "Font Awesome" implementation in vanilla R `{leaflet}` by constructing markers on the fly using `{fontawesome}` and `{magick}`. There are three issues `{leaf.magick}` sets out to overcome:

* `leaflet::awesomeIcons()` is limited to Font Awesome 4 👎

* `leaflet::awesomeIcons()` is limited to a handful of discrete colours 👎

* `leaflet::awesomeIcons()` is limited to a single marker size 👎

* Vanilla `{leaflet}` doesn't provide any additional features to construct legends, icon palettes, or such like 👎

`{leaf.magick}` attempts to solve all of this:

* `magicIcons()` uses up-to-date versions of Font Awesome and Boostrap Icons, powered by `{fontawesome}` and `{bsicons}` ✅

* `magicIcons()` uses `{fontawesome}` to grab a marker shape, and can therefore colour and resize it however the user desires ✅

* `{leaf.magick}` exports functions like `addIconLegend()` to help developers communicate the meanings of icons to users ✅

## Installation

You can install the development version of `{leaf.magick}` like so:

``` r
# install.packages("pak")
pak::pak("jack-davison/leaf.magick")
```

![](man/figures/webshot.png)


