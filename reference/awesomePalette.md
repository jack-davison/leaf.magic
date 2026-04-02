# Palette used by [`leaflet::awesomeIcons()`](https://rstudio.github.io/leaflet/reference/awesomeIcons.html)

[`leaflet::awesomeIcons()`](https://rstudio.github.io/leaflet/reference/awesomeIcons.html)
takes one of 19 colours, but its "red" colour is not the same as R's
base "red" defined in
[`colors()`](https://rdrr.io/r/grDevices/colors.html). This list maps
the awesome colours onto the hex codes they represent.

## Usage

``` r
awesomePalette
```

## Format

An object of class `list` of length 19.

## Examples

``` r
unlist(awesomePalette)
#>        red    darkred   lightred     orange      beige      green  darkgreen 
#>  "#d63e2a"  "#a23336"  "#ff8e7f"  "#f69730"  "#ffcb92"  "#72b026"  "#728224" 
#> lightgreen       blue   darkblue  lightblue     purple darkpurple       pink 
#>  "#bbf970"  "#38aadd"  "#0066a2"  "#8adaff"  "#d152b8"  "#5b396b"  "#ff91ea" 
#>  cadetblue      white       gray  lightgray      black 
#>  "#436978"  "#fbfbfb"  "#575757"  "#a3a3a3"  "#303030" 
```
