# Air Quality Monitoring in Neath Port Talbot, 2024

A subset of fixed monitoring stations which make up the Welsh Air
Quality Network in Neath Port Talbot, Wales. Data were obtained and
correct as of July 2024. This dataset is provided for demonstration of
`leaf.magic` functions.

## Usage

``` r
port_talbot
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 9
rows and 8 columns.

## Source

<https://www.airquality.gov.wales/>

## Details

- code:

  The unique code for the measurement site

- site:

  The human-readable site name for each site

- latitude,longitude:

  The decimal latitude/longitude coordinates of the site

- site_type:

  The site classification of the site; in this case, either Urban
  Industrial, Traffic, or Background. See
  <https://www.airquality.gov.wales/about-air-quality/monitoring/monitoring-locations>
  for more information.

- open:

  Logical; was the site still open at time of data compilation?

- start_date,end_date:

  The opening and closing date of the site. An `NA` in `end_date` means
  the site is still open

## Examples

``` r
# view data
port_talbot
#> # A tibble: 9 × 8
#>   code  site   latitude longitude site_type open  start_date          end_date  
#>   <chr> <chr>     <dbl>     <dbl> <chr>     <lgl> <dttm>              <date>    
#> 1 PT11  Princ…     51.6     -3.77 Urban In… TRUE  2014-03-17 00:00:00 NA        
#> 2 PTLW  Littl…     51.6     -3.80 Urban In… TRUE  2012-01-24 00:00:00 NA        
#> 3 PT10  Theod…     51.6     -3.77 Urban Tr… FALSE 2009-04-01 00:00:00 2018-06-04
#> 4 PT8   Talbo…     51.6     -3.78 Urban Tr… FALSE 2009-01-15 00:00:00 2018-06-04
#> 5 PT9   Twll-…     51.6     -3.76 Urban Tr… FALSE 2008-12-13 00:00:00 2021-03-18
#> 6 PT7   Docks      51.6     -3.79 Urban Ba… FALSE 2008-10-13 00:00:00 2018-06-04
#> 7 PT4   Margam     51.6     -3.77 Urban In… TRUE  2007-07-24 00:00:00 NA        
#> 8 PT6   Dyffr…     51.6     -3.75 Urban Ba… TRUE  2007-07-16 00:00:00 NA        
#> 9 PT    Port …     51.6     -3.76 Urban Ba… FALSE 1997-01-09 00:00:00 2007-07-24
```
