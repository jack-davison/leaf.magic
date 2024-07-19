#' Add an Icon legend to a map
#'
#' Manually specify icons, labels, and colours to add an Icon legend to a
#' map. Useful in conjunction with [magicIcons()] to communicate the meaning of
#' icons/colours.
#'
#' @param icons Name of the icons, passed to [fontawesome::fa()] or [bsicons::bs_icon()]. A
#'   full list of available icons can be found using
#'   [fontawesome::fa_metadata()] or at <https://icons.getbootstrap.com/>.
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
      }
    }

  html <-
    paste0(html_title,
           paste0(
             purrr::pmap_vec(
               dplyr::tibble(
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
