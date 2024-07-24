#' Add an Icon legend to a map
#'
#' Manually specify icons, labels, and colours to add an Icon legend to a map.
#' Useful in conjunction with [magicIcons()] to communicate the meaning of
#' icons/colours.
#'
#' @param icons Name of the icons, passed to [fontawesome::fa()] or
#'   [bsicons::bs_icon()]. A full list of available icons can be found using
#'   [fontawesome::fa_metadata()], <https://icons.getbootstrap.com/> or
#'   <https://ionic.io/ionicons>.
#' @param labels Labels for each `icon`; should be the same length as `icons`.
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
addIconLegend <- function(map,
                          icons,
                          labels,
                          colors = "black",
                          title = NULL,
                          library = "fontawesome",
                          position = c("topright", "bottomright", "bottomleft", "topleft"),
                          layerId = NULL,
                          className = "info legend",
                          data = leaflet::getMapData(map)) {
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

  assemble_legend_item <-
    function(icon, label, color) {
      if (library == "fontawesome") {
        paste0(
          fontawesome::fa(
            icon,
            width = "1em",
            height = "1em",
            fill = color
          ),
          "   ",
          label
        )
      } else if (library == "bootstrap") {
        i <- as.character(bsicons::bs_icon(icon, size = "1em"))
        i <- gsub("currentColor", color, i)
        paste0(i, "   ", label)
      } else if (library == "ionicons") {
        i <- read_ionicon(icon, color = color)
        paste0(i, "   ", label)
      }
    }

  html <-
    paste0(html_title,
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
           ),
           collapse = "<br>"
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
#' @export
addIconAttribution <-
  function(map,
           library = "fontawesome",
           layerId = NULL,
           group = NULL) {
    if (library == "fontawesome") {
      attr <-
        'Icons by <a href="https://fontawesome.com" target="_blank" rel="noopener noreferrer">FontAwesome</a>, licensed under <a href="https://fontawesome.com/license/free" target="_blank" rel="noopener noreferrer">CC BY 4.0</a>'
    } else if (library == "bootstrap") {
      attr <-
        'Icons by <a href="https://icons.getbootstrap.com/" target="_blank" rel="noopener noreferrer">Bootstrap</a>, licensed under <a href="https://github.com/twbs/icons?tab=MIT-1-ov-file" target="_blank" rel="noopener noreferrer">MIT</a>'
    } else if (library == "ionicons") {
      attr <-
        'Icons by <a href="https://ionic.io/ionicons" target="_blank" rel="noopener noreferrer">Ionicons</a>, licensed under <a href="https://github.com/ionic-team/ionicons?tab=MIT-1-ov-file" target="_blank" rel="noopener noreferrer">MIT</a>'
    }

    leaflet::addTiles(
      map,
      urlTemplate = " ",
      attribution = attr,
      layerId = layerId,
      group = group
    )
  }
