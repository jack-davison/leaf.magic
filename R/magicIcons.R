#' Create a set of Font Awesome, Bootstrap, Ionicons or Lucide Markers
#'
#' This function is intended as a more up-to-date implementation of
#' [leaflet::awesomeIcons()]. Key benefits include having access to modern Font
#' Awesome, Bootstrap, Ionicons and Lucide icons, and allowing any colour to be
#' used for the marker and icon.
#'
#' This function uses [magick::image_composite()] to knit together the icon and
#' marker symbol of choice, and saves it to `dir`. It is also intelligent enough
#' to not try to recreate a marker that already exists, so you may find the use
#' of [magicIcons()] speeds up the more you use it in a single session.
#'
#' Ionicons are downloaded from <https://unpkg.com> the first time each icon is
#' used in a session, so require an internet connection.
#'
#' @param icon Name of the icon. A full list of available icons can be found
#'   using [fontawesome::fa_metadata()], <https://icons.getbootstrap.com/>,
#'   <https://ionic.io/ionicons> or <https://lucide.dev/icons/>, depending on
#'   `library`.
#'
#' @param markerColor The color of the marker. Not used when `marker = "none"`.
#'
#' @param iconColor The color of the icon.
#'
#' @param marker Defaults to `"marker"`, which uses the standard teardrop shaped
#'   marker, similar to [leaflet::addMarkers()]. Other options are `"circle"`,
#'   `"square"`, `"star"`, `"heart"`, and `"diamond"`, which place the icon
#'   inside of the respective shape. Also available is `"none"`, which removes
#'   the marker entirely and places the icon directly on the map.
#'
#' @param markerSize The size of the marker. Defaults to `30`, which is roughly
#'   the same size as [leaflet::addMarkers()].
#'
#' @param library One of `"fontawesome"`, `"bootstrap"`, `"ionicons"`, or
#'   `"lucide"` defining the icon library of interest. Defaults to
#'   `"fontawesome"`.
#'
#' @param dir The directory in which markers are saved. By default this is
#'   [tempdir()], which is deleted at the end of each R session. Providing an
#'   alternative directory will allow markers to persist between R sessions.
#'   `dir` is created if it does not already exist.
#'
#' @inheritParams leaflet::makeIcon
#'
#' @return a [leaflet::iconList()], to be passed to the `icon` argument of
#'   [leaflet::addMarkers()]
#'
#' @export
#'
#' @seealso [addIconLegend()], [iconFactor()], [iconBin()], [iconQuantile()]
#'
#' @author Jack Davison
#'
#' @examples
#' # create map
#' library(leaflet)
#' port_talbot |>
#'   dplyr::mutate(
#'     icon = dplyr::recode_values(
#'       site_type,
#'       "Urban Industrial" ~ "industry",
#'       "Urban Traffic" ~ "car",
#'       "Urban Background" ~ "city",
#'       default = "x"
#'     ),
#'     color = dplyr::recode_values(
#'       site_type,
#'       "Urban Industrial" ~ "#12436D",
#'       "Urban Traffic" ~ "#801650",
#'       "Urban Background" ~ "#28A197",
#'       default = "#3D3D3DFF"
#'     )
#'   ) |>
#'   leaflet() |>
#'   addProviderTiles("CartoDB.Voyager") |>
#'   addMarkers(
#'     icon = ~ magicIcons(icon, color, "white"),
#'     lat = ~ latitude,
#'     lng = ~ longitude,
#'     popup = ~site
#'   )
magicIcons <- function(
  icon = "circle",
  markerColor = leaf.magic::awesomePalette$blue,
  iconColor = leaf.magic::awesomePalette$white,
  marker = "marker",
  markerSize = 30L,
  library = "fontawesome",
  className = NULL,
  dir = tempdir()
) {
  library <- check_library(library)

  # user-facing marker names -> Font Awesome icon used to draw the marker
  marker_shapes <- c(
    marker = "location-pin",
    circle = "fas fa-circle",
    square = "fas fa-square",
    star = "fas fa-star",
    heart = "fas fa-heart",
    diamond = "fas fa-diamond",
    none = "none"
  )
  marker <- match.arg(marker, names(marker_shapes), several.ok = TRUE)
  marker <- unname(marker_shapes[marker])

  check_lengths(
    icon = icon,
    markerColor = markerColor,
    iconColor = iconColor,
    marker = marker,
    markerSize = markerSize
  )

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  combinations <-
    data.frame(
      icon = icon,
      markerColor = markerColor,
      iconColor = iconColor,
      marker = marker,
      markerSize = markerSize
    )

  # only render each unique combination once
  key <- do.call(paste, c(combinations, sep = "\x1f"))
  is_first <- !duplicated(key)
  unique_combos <- combinations[is_first, , drop = FALSE]

  make_fa_icon <- function(icon, markerColor, iconColor, marker, markerSize) {
    # the package version is part of the cache key so that markers cached in a
    # persistent `dir` are re-rendered when the rendering code changes. The
    # rest of the key is hashed so that any icon name is a valid file name,
    # and colours are normalised so that, e.g., "red", "#ff0000" and "#FF0000"
    # share a single cached marker.
    url <- file.path(
      dir,
      paste0(
        "leafmagic-",
        getNamespaceVersion("leaf.magic"),
        "-",
        rlang::hash(list(
          icon,
          normalise_color(markerColor),
          normalise_color(iconColor),
          marker,
          library
        )),
        ".png"
      )
    )

    if (!file.exists(url)) {
      t_logo <- tempfile(pattern = "logo_")
      t_pin <- tempfile(pattern = "pin_")
      on.exit(unlink(c(t_logo, t_pin)), add = TRUE)

      svg_to_png(
        icon_svg(
          icon,
          iconColor,
          library,
          stroke = darken_color(iconColor),
          stroke_width = "5px"
        ),
        file = t_logo
      )

      logo <-
        magick::image_scale(
          magick::image_read(t_logo),
          ifelse(
            marker %in% c("location-pin", "fas fa-star"),
            "x200",
            "x250"
          )
        )

      if (marker != "none") {
        svg_to_png(
          icon_svg(
            marker,
            markerColor,
            "fontawesome",
            stroke = darken_color(markerColor),
            stroke_width = "5px"
          ),
          file = t_pin
        )

        pin <- magick::image_read(t_pin)

        h_adj <- (magick::image_info(pin)$width -
          magick::image_info(logo)$width) /
          2

        v_adj <- (magick::image_info(pin)$height -
          magick::image_info(logo)$height) /
          ifelse(
            marker == "location-pin",
            3.5,
            ifelse(marker == "fas fa-star", 1.5, 2)
          )

        marker_img <-
          magick::image_composite(
            pin,
            logo,
            offset = paste0("+", h_adj, "+", v_adj)
          )
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
      popupAnchorY = ifelse(
        marker == "location-pin",
        -(markerSize * ratio) * 0.8,
        0
      ),
      className = className
    )
  }

  icons <- purrr::pmap(unique_combos, make_fa_icon, .progress = TRUE)

  do.call(leaflet::iconList, icons[match(key, key[is_first])])
}

#' Build the SVG markup for an icon from any supported library
#'
#' @param icon,color Icon name and fill colour.
#' @param library A library name, already validated by `check_library()`.
#' @param ... Passed to [fontawesome::fa()] only (e.g., `stroke`, `height`).
#' @return A single character string of SVG markup.
#' @noRd
icon_svg <- function(icon, color, library, ...) {
  svg <- switch(
    library,
    fontawesome = fontawesome::fa(icon, fill = color, ...),
    bootstrap = gsub(
      "currentColor",
      color,
      as.character(bsicons::bs_icon(icon, size = "1em"))
    ),
    ionicons = read_ionicon(icon, color = color),
    lucide = lucidr::lucide(icon, size = 16, color = color)
  )
  paste(as.character(svg), collapse = "\n")
}

#' Rasterise SVG markup to a PNG file
#'
#' SVGs are rendered at a fixed height so that every icon library has enough
#' resolution to be scaled down cleanly; without a height, `{rsvg}` uses the
#' SVG's own size, which is only 16-24px for Bootstrap and Lucide icons.
#' @noRd
svg_to_png <- function(svg, file, height = 512) {
  rsvg::rsvg_png(charToRaw(svg), file = file, height = height)
}

#' Homebrew way to download from ionicons
#' @noRd
read_ionicon <- function(icon, color, dim = "1em") {
  svg <- ionicon_cache[[icon]]

  if (is.null(svg)) {
    svg_url <- paste0(
      "https://unpkg.com/ionicons@7.1.0/dist/svg/",
      icon,
      ".svg"
    )
    svg <- tryCatch(
      paste(readLines(svg_url, warn = FALSE), collapse = "\n"),
      error = function(cnd) {
        rlang::abort(
          c(
            paste0("Could not download the Ionicons icon \"", icon, "\"."),
            i = "Check the icon name at <https://ionic.io/ionicons>.",
            i = "Ionicons are downloaded on demand, so an internet connection is required."
          ),
          parent = cnd,
          call = NULL
        )
      }
    )
    ionicon_cache[[icon]] <- svg
  }

  gsub(
    'viewBox=',
    paste0(
      'style="vertical-align:-0.125em;height:',
      dim,
      ';width:',
      dim,
      ';fill:',
      color,
      ';" viewBox='
    ),
    svg
  )
}

#' Session cache of downloaded Ionicons SVGs, keyed by icon name
#' @noRd
ionicon_cache <- new.env(parent = emptyenv())

#' Check vector arguments are length 1 or a common length
#' @noRd
check_lengths <- function(..., call = rlang::caller_env()) {
  args <- list(...)
  lens <- lengths(args)
  n <- max(lens)
  bad <- lens == 0L | !lens %in% c(1L, n)
  if (any(bad)) {
    rlang::abort(
      c(
        "Arguments must be length 1 or all the same length.",
        stats::setNames(
          paste0("`", names(args), "` has length ", lens, ".")[bad],
          rep("x", sum(bad))
        ),
        i = paste0("The longest argument has length ", n, ".")
      ),
      call = call
    )
  }
  invisible(n)
}

#' Darken a colour slightly for icon border
#' @noRd
darken_color <- function(col, factor = 0.8) {
  col <- grDevices::col2rgb(col)
  col <- col * factor
  grDevices::rgb(col[1], col[2], col[3], maxColorValue = 255)
}

#' Normalise a colour to an uppercase "#RRGGBBAA" hex code
#' @noRd
normalise_color <- function(col) {
  rgba <- grDevices::col2rgb(col, alpha = TRUE)
  grDevices::rgb(
    rgba[1],
    rgba[2],
    rgba[3],
    rgba[4],
    maxColorValue = 255
  )
}

#' Check library is valid
#' @noRd
check_library <- function(library) {
  library <- rlang::arg_match(
    library,
    c("fontawesome", "bootstrap", "ionicons", "lucide")
  )

  if (library == "bootstrap") {
    rlang::check_installed("bsicons")
  }

  if (library == "lucide") {
    rlang::check_installed("lucidr")
  }

  library
}
