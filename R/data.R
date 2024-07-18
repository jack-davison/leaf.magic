#' Air Quality Monitoring in Neath Port Talbot, 2024
#'
#' A subset of fixed monitoring stations which make up the Welsh Air Quality
#' Network in Neath Port Talbot, Wales. Data were obtained and correct as of
#' July 2024. This dataset is provided for demonstration of `leaf.magic`
#' functions.
#'
#' @format ## `port_talbot`
#' A data frame with 9 rows and 8 columns:
#' \describe{
#'   \item{code}{The unique code for the measurement site}
#'   \item{site}{The human-readable site name for each site}
#'   \item{latitude,longitude}{The decimal latitude/longitude coordinates of the site}
#'   \item{site_type}{The site classification of the site; in this case, either Urban Industrial, Traffic, or Background. See <https://www.airquality.gov.wales/about-air-quality/monitoring/monitoring-locations> for more information.}
#'   \item{open}{Logical; was the site still open at time of data compilation?}
#'   \item{start_date,end_date}{The opening and closing date of the site. An `NA` in `end_date` means the site is still open}
#' }
#' @source <https://www.airquality.gov.wales/>
#' @examples
#' # view data
#' port_talbot
"port_talbot"

#' Palette used by [leaflet::awesomeIcons()]
#'
#' [leaflet::awesomeIcons()] takes one of 19 colours, but its "red" colour is not the same as R's base "red" defined in [colors()]. This list maps the awesome colours onto the hex codes they represent.
#'
#' @format ## `awesomePalette`
#' A named list of length 19.
#'
#' @examples
#' unlist(awesomePalette)
"awesomePalette"
