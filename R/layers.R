#' Add an Icon legend to a map
#'
#' Manually specify icons, labels, and colours to add an Icon legend to a map.
#' Useful in conjunction with [magicIcons()] to communicate the meaning of
#' icons/colours.
#'
#' @param icons Name of the icons, from the icon library defined by `library`.
#'   A full list of available icons can be found using
#'   [fontawesome::fa_metadata()], <https://icons.getbootstrap.com/>,
#'   <https://ionic.io/ionicons> or <https://lucide.dev/icons/>.
#' @param labels Labels for each `icon`; should be the same length as `icons`.
#'   Labels are inserted as HTML, so may contain formatting such as
#'   `"PM<sub>2.5</sub>"`.
#' @param colors Colours to use for each `icon`. If `length(colors) == 1L` it is
#'   recycled for all icons. Otherwise should be the same length as `icons`.
#' @param title the legend title; optional.
#' @inheritParams leaflet::addControl
#' @inheritParams magicIcons
#'
#' @return a [leaflet][leaflet::leaflet-package] widget
#' @export
#'
#' @examples
#' library(leaflet)
#' addIconLegend(
#'   map = leaflet(),
#'   icons = c("beer", "school"),
#'   labels = c("Pub", "University"),
#'   colors = c("red", "blue"),
#'   title = "Student Hangout"
#' )
addIconLegend <- function(
  map,
  icons,
  labels,
  colors = "black",
  title = NULL,
  library = "fontawesome",
  position = c("topright", "bottomright", "bottomleft", "topleft"),
  layerId = NULL,
  className = "info legend",
  data = leaflet::getMapData(map)
) {
  library <- check_library(library)
  position <- match.arg(position)

  # format title
  html_title <- ""
  if (!is.null(title)) {
    html_title <- paste0(
      "<div style='margin-bottom:3px'><strong>",
      title,
      "</strong></div>"
    )
  }

  check_lengths(icons = icons, labels = labels, colors = colors)

  assemble_legend_item <- function(icon, label, color) {
    svg <- icon_svg(icon, color, library, width = "1em", height = "1em")
    # HTML collapses plain whitespace, so non-breaking spaces are needed
    paste0(svg, "&nbsp;&nbsp;&nbsp;", label)
  }

  html <-
    paste0(
      html_title,
      paste0(
        purrr::pmap_vec(
          data.frame(
            icon = icons,
            label = labels,
            color = colors
          ),
          assemble_legend_item
        ),
        collapse = "<br>"
      )
    )

  leaflet::addControl(
    map = map,
    html = html,
    position = position,
    layerId = layerId,
    className = className,
    data = data
  )
}

#' Conveniently add Icon Attributions to a Leaflet Map
#'
#' Adds a short character string to the bottom-right of the map widget with the
#' attribution and license of an icon library. `group` and `layerId` can be used
#' to add or remove the attribution - e.g., if the markers using [magicIcons()]
#' are hidden.
#'
#' @inheritParams magicIcons
#' @inheritParams leaflet::addTiles
#'
#' @return a [leaflet][leaflet::leaflet-package] widget
#' @export
#'
#' @examples
#' library(leaflet)
#' leaflet(port_talbot) |>
#'   addTiles() |>
#'   addMarkers(
#'     lng = ~longitude,
#'     lat = ~latitude,
#'     icon = magicIcons("industry"),
#'     group = "Sites"
#'   ) |>
#'   addIconAttribution(group = "Sites") |>
#'   addLayersControl(overlayGroups = "Sites")
addIconAttribution <-
  function(map, library = "fontawesome", layerId = NULL, group = NULL) {
    library <- check_library(library)

    attribution <- switch(
      library,
      fontawesome = 'Icons by <a href="https://fontawesome.com" target="_blank" rel="noopener noreferrer">FontAwesome</a>, licensed under <a href="https://fontawesome.com/license/free" target="_blank" rel="noopener noreferrer">CC BY 4.0</a>',
      bootstrap = 'Icons by <a href="https://icons.getbootstrap.com/" target="_blank" rel="noopener noreferrer">Bootstrap</a>, licensed under <a href="https://github.com/twbs/icons?tab=MIT-1-ov-file" target="_blank" rel="noopener noreferrer">MIT</a>',
      ionicons = 'Icons by <a href="https://ionic.io/ionicons" target="_blank" rel="noopener noreferrer">Ionicons</a>, licensed under <a href="https://github.com/ionic-team/ionicons?tab=MIT-1-ov-file" target="_blank" rel="noopener noreferrer">MIT</a>',
      lucide = 'Icons by <a href="https://lucide.dev" target="_blank" rel="noopener noreferrer">Lucide</a>, licensed under <a href="https://lucide.dev/license" target="_blank" rel="noopener noreferrer">ISC</a>'
    )

    # The attribution is attached to an invisible tile layer so that it can be
    # shown/hidden with `group` and removed with `layerId`. Every tile is a
    # 1x1 transparent PNG data URI, so no network requests are made.
    leaflet::addTiles(
      map,
      urlTemplate = blank_tile,
      attribution = attribution,
      layerId = layerId,
      group = group
    )
  }

#' A 1x1 fully transparent PNG, used as a tile URL by addIconAttribution()
#' @noRd
blank_tile <- "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII="
