
#' Create a set of Font Awesome Markers
#'
#' This function is intended as a more up-to-date implementation of
#' [leaflet::awesomeIcons()]. Key benefits include having access to modern Font
#' Awesome icons, and allowing any colour to be used the marker and icon.
#'
#' @param icon Name of the Font Awesome icon, passed to [fontawesome::fa()]. A
#'   full list of available icons can be found using
#'   [fontawesome::fa_metadata()].
#' @param markerColor The color of the teardrop-shaped marker.
#' @param iconColor The color of the fontawesome icon.
#' @inheritParams leaflet::makeIcon
#'
#' @return a [leaflet::iconList()], to be passed to the `icon` argument of
#'   [leaflet::addMarkers()]
#' @export
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
#'   addMarkers(icon = ~ faIcons(icon, color, "white"),
#'              popup = ~ site)
faIcons <- function(icon = "circle",
                    markerColor = NULL,
                    iconColor = NULL,
                    className = NULL) {
  combinations <-
    dplyr::tibble(icon = icon,
                  markerColor = markerColor,
                  iconColor = iconColor)

  unique_combos <- dplyr::distinct(combinations)

  make_fa_icon <- function(icon, markerColor, iconColor) {
    time <- Sys.time() %>% as.numeric()

    t_pin <- tempfile(pattern = paste0(time, "pin_"))

    t_logo <- tempfile(pattern = paste0(time, "logo_"))

    t_shadow <- tempfile(pattern = paste0(time, "shadow_"))

    fontawesome::fa_png("location-pin", file = t_pin, fill = markerColor)

    fontawesome::fa_png(icon, file = t_logo, fill = iconColor)

    pin <- magick::image_read(t_pin)

    logo <- magick::image_read(t_logo) %>% magick::image_scale("x200")

    h_adj <- (magick::image_info(pin)$width - magick::image_info(logo)$width) /
      2

    v_adj <- (magick::image_info(pin)$height - magick::image_info(logo)$height) /
      3.5

    marker <-
      magick::image_composite(pin, logo, offset = glue::glue("+{h_adj}+{v_adj}"))

    shadow <- marker %>%
      magick::image_background("transparent") %>%
      magick::image_shadow_mask() %>%
      magick::image_resize("424x552")

    t <- tempfile()

    magick::image_write(marker, t)

    magick::image_write(shadow, t_shadow)

    icon_size <- 30
    ratio <- 512 / 384

    leaflet::makeIcon(
      iconUrl = t,
      iconWidth = icon_size,
      iconHeight = icon_size * ratio,
      iconAnchorX = icon_size / 2,
      iconAnchorY = icon_size * ratio,
      shadowUrl = t_shadow,
      shadowWidth = icon_size * 1.2,
      shadowHeight = icon_size * ratio * 1.1,
      shadowAnchorX = ((icon_size * 1.2) / 2),
      shadowAnchorY = (icon_size * ratio * 1.05),
      popupAnchorX = .Machine$double.eps,
      popupAnchorY = -(icon_size * ratio) * 0.8,
      className = className
    )
  }

  icons <- purrr::pmap(unique_combos, make_fa_icon, .progress = TRUE)

  unique_combos$themarker <- icons

  combinations <-
    dplyr::left_join(combinations,
                     unique_combos,
                     dplyr::join_by(icon, markerColor, iconColor))

  do.call(leaflet::iconList, combinations$themarker)
}


#' Add a Font Awesome legend to a map
#'
#' Manually specify icons, labels, and colours to add a Font Awesome legend to a
#' map. Useful in conjunction with [faIcons()] to communicate the meaning of
#' icons/colours.
#'
#' @param icons Name of the Font Awesome icons, passed to [fontawesome::fa()]. A
#'   full list of available icons can be found using
#'   [fontawesome::fa_metadata()].
#' @param labels Labels for each `icon`; should be the same length as `icons`.
#' @param colors Colours to use for each `icon`. If `length(colors) == 1L` it is
#'   recycled for all icons. Otherwise should be the same length as `icons`.
#' @param title the legend title; optional.
#' @inheritParams leaflet::addControl
#'
#' @return a [leaflet][leaflet::leaflet-package] widget
#' @export
#'
#' @examples
#' library(leaflet)
#' addFaLegend(
#'   map = leaflet(),
#'   icons = c("beer", "school"),
#'   labels = c("Pub", "University"),
#'   colors = c("red", "blue"),
#'   title = "Student Hangout"
#' )
addFaLegend <- function(map,
                        icons,
                        labels,
                        colors = "black",
                        title = NULL,
                        position = c("topright", "bottomright", "bottomleft", "topleft"),
                        layerId = NULL,
                        className = "info legend",
                        data = leaflet::getMapData(map)) {
  position <- match.arg(position)

  # format title
  html_title <- ""
  if (!is.null(title)) {
    html_title <- paste0("<div style='margin-bottom:3px'><strong>",
                         title,
                         "</strong></div>")
  }

  assemble_legend_item <-
    function(icon, label, color) {
      paste0(fontawesome::fa(
        icon,
        width = "1em",
        height = "1em",
        fill = color
      ),
      "   ",
      label)
    }

  html <-
    paste0(html_title,
           paste0(
             purrr::pmap_vec(
               tibble::tibble(
                 icon = icons,
                 label = labels,
                 color = colors
               ),
               assemble_legend_item
             ),
             collapse = "<br>"
           ),

           collapse = "<br>")

  leaflet::addControl(
    map = map,
    html = html,
    position = position,
    layerId = layerId,
    className = className,
    data = data
  )
}
