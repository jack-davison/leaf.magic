#' Create a set of Font Awesome, Bootstrap or Ionicons Markers
#'
#' This function is intended as a more up-to-date implementation of
#' [leaflet::awesomeIcons()]. Key benefits include having access to modern Font
#' Awesome, Bootstrap and Ionicons icons, and allowing any colour to be used the
#' marker and icon.
#'
#' This function uses [magick::image_composite()] to knit together the icon and
#' marker symbol of choice, and saves it to a temporary directory. It is also
#' intelligent enough to not try to recreate a marker that already exists, so
#' you may find the use of [magicIcons()] speeds up the more you use it in a
#' single session.
#'
#' @param icon Name of the Font Awesome icon, passed to [fontawesome::fa()] or
#'   [bsicons::bs_icon()]. A full list of available icons can be found using
#'   [fontawesome::fa_metadata()] or at <https://icons.getbootstrap.com/>.
#' @param markerColor The color of the marker. Not used when `marker = "none"`.
#' @param iconColor The color of the fontawesome icon.
#' @param marker Defaults to `"marker"`, which uses the standard teardrop shaped
#'   marker, similar to [leaflet::addMarkers()]. Other options are `"circle"`,
#'   `"square"`, `"star"`, `"heart"`, and `"diamond"`, which place the icon
#'   inside of the respective shape. Also available is `"none"`, which removes
#'   the marker entirely and places the icon directly on the map.
#' @param markerSize The size of the marker. Defaults to `30`, which is roughly
#'   the same size as [leaflet::addMarkers()].
#' @param library One of `"fontawesome"`, `"bootstrap"`, or `"ionicons"`,
#'   defining the icon library of interest. Defaults to `"fontawesome"`.
#' @param dir The directory in which markers are saved. By default this is
#'   [tempdir()], which is a temporary directory after each session. Providing
#'   an alternative directory will allow markers to persist between R sessions.
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
                       markerColor = awesomePalette$blue,
                       iconColor = awesomePalette$white,
                       marker = c("marker", "circle", "square", "star", "heart", "diamond", "none"),
                       markerSize = 30L,
                       library = "fontawesome",
                       className = NULL,
                       dir = tempdir()) {
  library <- match.arg(library, c("fontawesome", "bootstrap", "ionicons"))

  marker <-
    match.arg(marker,
              c("marker", "circle", "square", "star", "heart", "diamond", "none"))
  marker[marker %in% c("circle", "square", "star", "heart", "diamond")] <-
    paste0("fas fa-", marker)
  marker[marker == "marker"] <- "location-pin"

  combinations <-
    data.frame(
      icon = icon,
      markerColor = markerColor,
      iconColor = iconColor,
      markerSize = markerSize
    )

  combinations$rn <- seq_along(combinations$icon)

  unique_combos <- combinations
  unique_combos$rn <- NULL
  unique_combos <- unique(unique_combos)

  make_fa_icon <- function(icon, markerColor, iconColor, markerSize) {
    url <- paste0(dir,
                  "/leafmagic-",
                  icon,
                  "_",
                  markerColor,
                  "_",
                  iconColor,
                  "_",
                  marker,
                  "_",
                  library,
                  ".png")

    time <- Sys.time() %>% as.numeric()

    if (!file.exists(url)) {
      t_logo <- tempfile(pattern = paste0(time, "logo_"))

      if (library == "fontawesome") {
        fontawesome::fa_png(
          icon,
          file = t_logo,
          fill = iconColor,
          stroke = darken_color(iconColor),
          stroke_width = "5px"
        )
      } else if (library == "bootstrap") {
        icon <- as.character(bsicons::bs_icon(icon, size = "1em"))
        icon <- gsub("currentColor", iconColor, icon)
        rsvg::rsvg_png(charToRaw(icon), file = t_logo)
      } else if (library == "ionicons") {
        ionicon <- read_ionicon(icon, color = iconColor)
        rsvg::rsvg_png(charToRaw(ionicon), file = t_logo)
      }

      logo <-
        magick::image_read(t_logo) %>% magick::image_scale(ifelse(marker %in% c("location-pin", "fas fa-star"), "x200", "x250"))

      if (marker != "none") {
        t_pin <- tempfile(pattern = "pin_")

        fontawesome::fa_png(
          marker,
          file = t_pin,
          fill = markerColor,
          stroke = darken_color(markerColor),
          stroke_width = "5px"
        )

        pin <- magick::image_read(t_pin)

        h_adj <- (magick::image_info(pin)$width - magick::image_info(logo)$width) /
          2

        v_adj <- (magick::image_info(pin)$height - magick::image_info(logo)$height) /
          ifelse(marker == "location-pin", 3.5, ifelse(marker == "fas fa-star", 1.5, 2))

        marker_img <-
          magick::image_composite(pin, logo, offset = paste0("+", h_adj, "+", v_adj))
      } else {
        marker_img <- logo
      }

      magick::image_write(marker_img, url)
    }

    imginfo <- magick::image_info(magick::image_read(url))
    ratio <- imginfo$height / imginfo$width

    leaflet::makeIcon(
      iconUrl = url,
      iconWidth = markerSize,
      iconHeight = markerSize * ratio,
      iconAnchorX = ifelse(marker == "location-pin", markerSize / 2, 0),
      iconAnchorY = ifelse(marker == "location-pin", markerSize * ratio, 0),
      popupAnchorX = ifelse(marker == "location-pin", .Machine$double.eps, 0),
      popupAnchorY = ifelse(marker == "location-pin", -(markerSize * ratio) * 0.8, 0),
      className = className
    )
  }

  icons <- purrr::pmap(unique_combos, make_fa_icon, .progress = TRUE)

  unique_combos$themarker <- icons

  combinations <-
    merge(
      combinations,
      unique_combos,
      sort = TRUE,
      by = c("icon", "markerColor", "iconColor", "markerSize")
    )

  combinations <- combinations[order(combinations$rn),]
  rownames(combinations) <- NULL

  do.call(leaflet::iconList, combinations$themarker)
}

#' Homebrew way to download from ionicons
#' @noRd
read_ionicon <- function(icon, color, dim = "1em") {
  on.exit(close(con))

  con <- url(paste0("https://unpkg.com/ionicons@7.1.0/dist/svg/", icon, ".svg"))

  svg <- readLines(con, warn = FALSE)

  gsub('viewBox=', paste0('style="vertical-align:-0.125em;height:', dim, ';width:', dim, ';fill:', color, ';" viewBox='), svg)
}

#' Darken a colour slightly for icon border
#' @noRd
darken_color <- function(col, factor = 0.8) {
  col <- grDevices::col2rgb(col)
  col <- col * factor
  col <- pmax(pmin(col, 255), 0)
  col <- grDevices::rgb(col[1], col[2], col[3], maxColorValue = 255)
  return(col)
}
