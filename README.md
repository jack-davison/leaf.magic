
# {leaf.magic} - Use Modern Icon Libraries in Leaflet Markers <a href="https://jack-davison.github.io/leaf.magic/"><img src="man/figures/logo.png" align="right" height="138" alt="leaf.magic website" /></a>

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/leaf.magic)](https://CRAN.R-project.org/package=leaf.magic)
[![R-CMD-check](https://github.com/jack-davison/leaf.magic/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jack-davison/leaf.magic/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `{leaf.magic}` is to overcome the somewhat outdated "Font Awesome" implementation in vanilla R `{leaflet}` by constructing markers on the fly using `{fontawesome}` or `{bsicons}` and `{magick}`. There are three issues `{leaf.magic}` sets out to overcome:

* `leaflet::awesomeIcons()` is limited to Font Awesome 4 👎

* `leaflet::awesomeIcons()` is limited to a handful of discrete colours 👎

* `leaflet::awesomeIcons()` is limited to a single marker size 👎

* Vanilla `{leaflet}` doesn't provide any additional features to construct legends, icon palettes, or such like 👎

`{leaf.magic}` attempts to solve all of this:

* `magicIcons()` uses up-to-date versions of Font Awesome, Bootstrap, and Ionicons Icons, powered by `{fontawesome}` and `{bsicons}` ✅

* `magicIcons()` uses `{fontawesome}` to grab a marker shape, and can therefore colour and resize it however the user desires ✅

* `{leaf.magic}` exports functions like `iconBin()` and `addIconLegend()` to help developers communicate the meanings of icons to users ✅

`{leaf.magic}` is thoroughly documented on the [package website](https://jack-davison.github.io/leaf.magic/), which includes a [getting started guide](https://jack-davison.github.io/leaf.magic/articles/leaf-magic.html).

## Installation

You can install the development version of `{leaf.magic}` like so:

``` r
# install.packages("pak")
pak::pak("jack-davison/leaf.magic")
```

![A screenshot of a map produced using the {leaf.magic} R package.](man/figures/webshot.png)

## Attributions

`{leaf.magic}` is licensed under the [MIT License](https://jack-davison.github.io/leaf.magic/LICENSE.html), and would not be possible without the following icon libraries:

* **Font Awesome** - [Website](https://fontawesome.com/) | [License](https://fontawesome.com/license/free) | [R Package {fontawesome}](https://github.com/rstudio/fontawesome)

* **Bootstrap Icons** - [Website](https://icons.getbootstrap.com/) | [License](https://getbootstrap.com/docs/5.3/about/license/) | [R Package {bsicons}](https://github.com/rstudio/bsicons)

* **Ionicons** - [Website](https://ionic.io/ionicons) | [License](https://github.com/ionic-team/ionicons/blob/main/LICENSE)

## Code of Conduct

Please note that the leaf.magic project is released with a [Contributor Code of Conduct](https://jack-davison.github.io/leaf.magic/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
