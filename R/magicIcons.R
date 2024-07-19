#' Create a set of Font Awesome or Bootstrap Markers
#'
#' This function is intended as a more up-to-date implementation of
#' [leaflet::awesomeIcons()]. Key benefits include having access to modern Font
#' Awesome and Bootstrap icons, and allowing any colour to be used the marker
#' and icon.
#'
#' @param icon Name of the Font Awesome icon, passed to [fontawesome::fa()] or
#'   [bsicons::bs_icon()]. A full list of available icons can be found using
#'   [fontawesome::fa_metadata()] or at <https://icons.getbootstrap.com/>.
#' @param markerColor The color of the teardrop-shaped marker.
#' @param iconColor The color of the fontawesome icon.
#' @param markerSize The size of the marker. Defaults to `30`, which is roughly
#'   the same size as [leaflet::addMarkers()].
#' @param library One of `"fontawesome"` or `"bootstrap"`, defining the icon
#'   library of interest. Defaults to `"fontawesome"`.
#' @inheritParams leaflet::makeIcon
#'
#' @return a [leaflet::iconList()], to be passed to the `icon` argument of
#'   [leaflet::addMarkers()]
#' @export
#'
#' @seealso [addIconLegend()], [iconFactor()], [iconBin()], [iconQuantile()]
#'
#' @author Jack Davison
#'
#' @examples
#' # create map
#' library(leaflet)
#' port_talbot %>%
#'   dplyr::mutate(
#'     icon = dplyr::case_match(
#'       site_type,
#'       "Urban Industrial" ~ "industry",
#'       "Urban Traffic" ~ "car",
#'       "Urban Background" ~ "city",
#'       .default = "x"
#'     ),
#'     color = dplyr::case_match(
#'       site_type,
#'       "Urban Industrial" ~ "#12436D",
#'       "Urban Traffic" ~ "#801650",
#'       "Urban Background" ~ "#28A197",
#'       .default = "#3D3D3DFF"
#'     )
#'   ) %>%
#'   leaflet() %>%
#'   addProviderTiles("CartoDB.Voyager") %>%
#'   addMarkers(
#'     icon = ~ magicIcons(icon, color, "white"),
#'     popup = ~site
#'   )
magicIcons <- function(icon = "circle",
                       markerColor = NULL,
                       iconColor = NULL,
                       markerSize = 30L,
                       library = "fontawesome",
                       className = NULL) {
  library <- match.arg(library, c("fontawesome", "bootstrap"))

  combinations <-
    data.frame(
      icon = icon,
      markerColor = markerColor,
      iconColor = iconColor,
      markerSize = markerSize
    )

  unique_combos <- unique(combinations)

  make_fa_icon <- function(icon, markerColor, iconColor, markerSize) {
    time <- Sys.time() %>% as.numeric()

    t_pin <- tempfile(pattern = paste0(time, "pin_"))

    t_logo <- tempfile(pattern = paste0(time, "logo_"))

    t_shadow <- tempfile(pattern = paste0(time, "shadow_"))

    if (library == "fontawesome") {
      fontawesome::fa_png(icon, file = t_logo, fill = iconColor)
    } else if (library == "bootstrap") {
      icon <- as.character(bsicons::bs_icon(icon, size = "1em"))
      icon <- gsub("currentColor", iconColor, icon)
      rsvg::rsvg_png(charToRaw(icon), file = t_logo)
    }

    fontawesome::fa_png("location-pin", file = t_pin, fill = markerColor)

    pin <- magick::image_read(t_pin)

    logo <- magick::image_read(t_logo) %>% magick::image_scale("x200")

    h_adj <- (magick::image_info(pin)$width - magick::image_info(logo)$width) /
      2

    v_adj <- (magick::image_info(pin)$height - magick::image_info(logo)$height) /
      3.5

    marker <-
      magick::image_composite(pin, logo, offset = paste0("+", h_adj, "+", v_adj))

    shadow <- marker %>%
      magick::image_background("transparent") %>%
      magick::image_shadow_mask() %>%
      magick::image_resize("424x552")

    t <- tempfile()

    magick::image_write(marker, t)

    magick::image_write(shadow, t_shadow)

    ratio <- 512 / 384

    leaflet::makeIcon(
      iconUrl = t,
      iconWidth = markerSize,
      iconHeight = markerSize * ratio,
      iconAnchorX = markerSize / 2,
      iconAnchorY = markerSize * ratio,
      shadowUrl = t_shadow,
      shadowWidth = markerSize * 1.2,
      shadowHeight = markerSize * ratio * 1.1,
      shadowAnchorX = ((markerSize * 1.2) / 2),
      shadowAnchorY = (markerSize * ratio * 1.05),
      popupAnchorX = .Machine$double.eps,
      popupAnchorY = -(markerSize * ratio) * 0.8,
      className = className
    )
  }

  icons <- purrr::pmap(unique_combos, make_fa_icon, .progress = TRUE)

  unique_combos$themarker <- icons

  combinations <-
    merge(combinations,
          unique_combos,
          by = c("icon", "markerColor", "iconColor", "markerSize"))

  do.call(leaflet::iconList, combinations$themarker)
}

