#' Add an Icon legend to a map
#'
#' Add an Icon legend to a map, either by manually specifying icons, labels,
#' and colours, or by providing an icon scale function (from [iconFactor()],
#' [iconBin()] or [iconQuantile()]) in the same way as [leaflet::addLegend()].
#' Useful in conjunction with [magicIcons()] to communicate the meaning of
#' icons/colours.
#'
#' @section Using icon scales: Like [leaflet::addLegend()], a legend can be
#'   generated automatically by passing an icon scale function to `pal`
#'   alongside the `values` used on the map. The icons and labels are then
#'   determined from the levels or bins of the scale, and labels are formatted
#'   using `labFormat`. For example, an [iconBin()] scale would be shown as one
#'   icon per bin, labelled with the range of that bin.
#'
#'   In this case `colors` may also be a colour scale function, such as one
#'   created using [leaflet::colorBin()] with the same settings as the icon
#'   scale, giving a legend of matched icon-colour pairs.
#'
#'   Legend items are derived exactly as in [leaflet::addLegend()]: factor
#'   scales show the sorted unique `values`, bin scales show one item per bin,
#'   and quantile scales show one item per quantile of `values` (with the
#'   quantile values in a hover tooltip). An icon legend and a colour legend
#'   built from scales with the same settings will therefore always agree.
#'   Unlike [leaflet::addLegend()], `values` may be omitted when the scale has
#'   a fixed `domain`/`levels`, and [iconBin()] scales with no `domain` are
#'   supported. For the latter, the legend shows only the bins that contain
#'   `values`, with the same icons (and colours, if `colors` is a colour scale)
#'   as the map. Like [leaflet::colorBin()], such a scale assigns icons based on
#'   which bins are occupied, so empty bins have no icon to show.
#'
#'   If any `values` are missing, an extra legend item is added using the
#'   `na.icon` of the scale and labelled with `na.label`. Its colour is the
#'   `na.color` of `colors` if it is a colour scale, and otherwise grey
#'   (`"#808080"`, the default `na.color` in `{leaflet}`). As in
#'   [leaflet::addLegend()], this is skipped if `na.label` is already one of the
#'   labels, if `na.icon` is `NA`, or if `colors` is a colour scale with a
#'   transparent `na.color`.
#'
#' @param icons Name of the icons, from the icon library defined by `library`.
#'   A full list of available icons can be found using
#'   [fontawesome::fa_metadata()], <https://icons.getbootstrap.com/>,
#'   <https://ionic.io/ionicons> or <https://lucide.dev/icons/>. Should not be
#'   provided if `pal` is used.
#' @param labels Labels for each `icon`; should be the same length as `icons`.
#'   Labels are inserted as HTML, so may contain formatting such as
#'   `"PM<sub>2.5</sub>"`. If `pal` is used, labels are generated using
#'   `labFormat`, but can be overridden by providing `labels`.
#' @param colors Colours to use for each `icon`. If `length(colors) == 1L` it is
#'   recycled for all icons. Otherwise should be the same length as `icons`. If
#'   `pal` is used, `colors` may also be a colour scale function, such as one
#'   created using [leaflet::colorFactor()], [leaflet::colorBin()] or
#'   [leaflet::colorQuantile()].
#' @param title the legend title; optional. If `pal` is used and `values` is a
#'   formula, the title defaults to the variable name in the formula, as in
#'   [leaflet::addLegend()].
#' @param pal An icon scale function created using [iconFactor()],
#'   [iconBin()] or [iconQuantile()]. If provided, `icons` should not be.
#' @param values The values used to generate icons from `pal`. Either a vector
#'   or a formula to be evaluated against `data` (e.g., `~type`). May be
#'   omitted for scales with a fixed `domain` or `levels`, except for
#'   [iconQuantile()] without a `domain`.
#' @param na.label The legend label for missing `values`.
#' @param labFormat A function to format the labels derived from `pal` and
#'   `values`. See [leaflet::labelFormat()].
#' @param opacity The opacity of the legend icons, between `0` and `1`. Note
#'   that this defaults to `1`, rather than the `0.5` of
#'   [leaflet::addLegend()], so that icons are shown in their true colours.
#' @param group The group of a layer (e.g., the markers created using
#'   [magicIcons()]). If provided, the legend is shown/hidden alongside that
#'   group, e.g., using [leaflet::addLayersControl()]. In Shiny, using `group`
#'   with [leaflet::leafletProxy()] requires the original map to already
#'   contain a grouped icon legend.
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
#'
#' # use icon scales, as with leaflet::addLegend()
#' icons <- c("temperature-low", "temperature-half", "temperature-high")
#' iconPal <- iconBin(icons, domain = 0:30, bins = 3, pretty = FALSE)
#' colorPal <- colorBin("RdYlBu", domain = 0:30, bins = 3, pretty = FALSE,
#'                      reverse = TRUE)
#'
#' leaflet() |>
#'   addIconLegend(
#'     pal = iconPal,
#'     values = c(2, 15, 28),
#'     colors = colorPal,
#'     title = "Temperature",
#'     labFormat = labelFormat(suffix = "&deg;C")
#'   )
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
  data = leaflet::getMapData(map),
  pal,
  values,
  na.label = "NA",
  labFormat = leaflet::labelFormat(),
  opacity = 1,
  group = NULL
) {
  library <- check_library(library)
  position <- match.arg(position)

  if (!missing(pal)) {
    if (!missing(icons)) {
      rlang::abort("Provide either `pal` or `icons`, not both.")
    }
    if (missing(values)) {
      values <- NULL
    } else if (inherits(values, "formula")) {
      if (missing(title)) {
        title <- deparse(values[[2]])
      }
      values <- leaflet::evalFormula(values, data)
    }
    legend <- icon_legend_items(pal, values, colors, labFormat, na.label)
    icons <- legend$icons
    colors <- legend$colors
    if (missing(labels)) {
      labels <- legend$labels
    }
  } else {
    if (is.function(colors)) {
      rlang::abort("`colors` can only be a function if `pal` is provided.")
    }
    if (missing(icons) || missing(labels)) {
      rlang::abort(c(
        "Both `icons` and `labels` must be provided when `pal` is not used.",
        i = "Alternatively, use `pal` and `values` to build the legend from an icon scale."
      ))
    }
  }

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

  if (!is.numeric(opacity) || length(opacity) != 1L || is.na(opacity) ||
      opacity < 0 || opacity > 1) {
    rlang::abort("`opacity` must be a single number between 0 and 1.")
  }

  assemble_legend_item <- function(icon, label, color) {
    svg <- icon_svg(icon, color, library, width = "1em", height = "1em")
    if (opacity < 1) {
      svg <- paste0("<span style='opacity:", opacity, "'>", svg, "</span>")
    }
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

  if (is.null(group)) {
    return(
      leaflet::addControl(
        map = map,
        html = html,
        position = position,
        layerId = layerId,
        className = className,
        data = data
      )
    )
  }

  # leaflet::addControl() has no `group` argument, so a small JS method which
  # mirrors leaflet's own addLegend() group handling is used instead
  map$dependencies <- c(map$dependencies, list(icon_legend_dependency()))
  leaflet::invokeMethod(
    map,
    data,
    "addIconLegend",
    list(
      html = html,
      position = position,
      layerId = layerId,
      className = className,
      group = group
    )
  )
}

#' Derive legend icons, labels and colours from an icon scale
#'
#' Mirrors the logic of [leaflet::addLegend()] for `colorType`, using the
#' `iconType` and `iconArgs` attributes set by `with_icon_attr()`. Where
#' leaflet would fail (e.g., no `values`, or a bin scale with no domain), a
#' sensible fallback is used instead.
#' @noRd
icon_legend_items <- function(pal, values, colors, labFormat, na.label) {
  type <- attr(pal, "iconType", exact = TRUE)
  args <- attr(pal, "iconArgs", exact = TRUE)
  if (!is.function(pal) || is.null(type)) {
    rlang::abort(
      "`pal` must be created by `iconFactor()`, `iconBin()` or `iconQuantile()`."
    )
  }

  has_values <- !is.null(values) && !all(is.na(values))
  color_index <- NULL
  needs_values <- function() {
    if (!has_values) {
      rlang::abort(
        "`values` must be provided when `pal` has no fixed `domain`."
      )
    }
  }

  if (type == "factor") {
    if (has_values) {
      # as leaflet::addLegend()
      v <- sort(unique(stats::na.omit(values)))
    } else {
      if (is.null(args$levels)) {
        needs_values()
      }
      v <- args$levels
    }
    icons <- pal(v)
    color_values <- v
    labels <- labFormat(type = "factor", v)
  } else if (type == "bin") {
    cuts <- args$bins
    autobin <- length(cuts) == 1
    if (autobin) {
      # leaflet::addLegend() draws an empty legend for
      # colorBin(domain = NULL, bins = n), so there is nothing to mirror.
      # Instead, show exactly what the map shows: the scale only assigns icons
      # (and colours) to occupied bins, and which icon a bin gets depends on
      # which other bins are occupied, so evaluate `pal()` on `values` itself
      # and take one representative value per occupied bin. Calling `pal()` on
      # bin midpoints would recalculate the bins from the midpoints.
      needs_values()
      cuts <- get_bins(NULL, values, cuts, args$pretty)
      ints <- cut(
        values,
        cuts,
        labels = FALSE,
        include.lowest = TRUE,
        right = isTRUE(args$right)
      )
      occupied <- sort(unique(stats::na.omit(ints)))
      first <- match(occupied, ints)
      icons <- pal(values)[first]
      color_values <- values
      color_index <- first
      labels <- labFormat(type = "bin", cuts)[occupied]
    } else {
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n]) / 2
      icons <- pal(mids)
      color_values <- mids
      labels <- labFormat(type = "bin", cuts)
    }
  } else if (type == "quantile") {
    p <- args$probs
    n <- length(p)
    if (has_values) {
      # as leaflet::addLegend()
      cuts <- stats::quantile(values, probs = p, na.rm = TRUE)
      mids <- stats::quantile(values, probs = (p[-1] + p[-n]) / 2, na.rm = TRUE)
    } else {
      if (is.null(args$bins)) {
        needs_values()
      }
      cuts <- args$bins
      mids <- (cuts[-1] + cuts[-n]) / 2
    }
    icons <- pal(mids)
    color_values <- mids
    labels <- labFormat(type = "quantile", cuts, p)
  } else {
    rlang::abort("Icon scale function not supported.")
  }

  # as leaflet::addLegend(), an NA item is only shown if there are missing
  # values, the NA icon/colour isn't "invisible", and `na.label` isn't already
  # one of the labels
  na.color <- "#808080"
  if (is.function(colors)) {
    na.color <- attr(colors, "colorArgs", exact = TRUE)$na.color
  }
  show_na <- has_values &&
    any(is.na(values)) &&
    !is_invisible(args$na.icon) &&
    !is_invisible_color(na.color) &&
    !na.label %in% labels

  if (is.function(colors)) {
    colors <- colors(color_values)
    if (!is.null(color_index)) colors <- colors[color_index]
    if (show_na) colors <- c(colors, na.color)
  } else if (show_na) {
    # as leaflet::addLegend(), the NA item always uses the NA colour, even
    # when a single colour is used for every other item
    colors <- c(rep_len(colors, length(icons)), na.color)
  }

  if (show_na) {
    icons <- c(icons, args$na.icon)
    labels <- c(labels, na.label)
  }

  list(
    icons = unname(icons),
    labels = unname(labels),
    colors = unname(colors)
  )
}

#' Is an NA icon missing?
#' @noRd
is_invisible <- function(x) {
  is.null(x) || length(x) == 0 || is.na(x[1])
}

#' Is a colour missing or fully transparent? (as leaflet::addLegend())
#' @noRd
is_invisible_color <- function(x) {
  is.null(x) || is.na(x[1]) || grDevices::col2rgb(x, alpha = TRUE)[[4]] == 0
}

#' HTML dependency for the addIconLegend JS method, used for `group` support
#' @noRd
icon_legend_dependency <- function() {
  htmltools::htmlDependency(
    name = "leaf-magic-legend",
    version = as.character(utils::packageVersion("leaf.magic")),
    src = "htmlwidgets",
    script = "iconLegend.js",
    package = "leaf.magic",
    all_files = FALSE
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
