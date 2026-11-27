#' Create a set of Font Awesome, Bootstrap, Ionicons or Lucide Markers
#'
#' This function is intended as a more up-to-date implementation of
#' [leaflet::awesomeIcons()]. Key benefits include having access to modern Font
#' Awesome, Bootstrap, Ionicons and Lucide icons, and allowing any colour to be
#' used for the marker and icon.
#'
#' Each marker is built as a single SVG image, with the icon nested inside of
#' the marker shape, and saved to `dir`. As markers are vector images, they
#' stay sharp at any `markerSize` and on high resolution screens. Markers are
#' only built once for each combination of icon, colours, marker shape and
#' library, so you may find the use of [magicIcons()] speeds up the more you use
#' it in a single session.
#'
#' Each icon library is provided by a separate package which must be installed
#' to use it: `{fontawesome}`, `{bsicons}` or `{lucidr}`. Ionicons are
#' downloaded from <https://unpkg.com> the first time each icon is used in a
#' session, so require an internet connection.
#'
#' @section Using formulas: As with [leaflet::awesomeIcons()], `icon`,
#'   `markerColor`, `iconColor`, `marker`, `markerSize`, `outlineColor` and
#'   `outlineWidth` may be one-sided
#'   formulas (e.g., `~site_type`), which are evaluated against the data of the
#'   layer the markers are added to. Markers are then built when the layer is
#'   added, so `magicIcons()` must be passed directly to the `icon` argument of
#'   [leaflet::addMarkers()]. Formulas and constants can be mixed freely, and
#'   formulas may call other functions, such as an [iconFactor()] scale or a
#'   [leaflet::colorFactor()] palette (e.g., `markerColor = ~pal(site_type)`).
#'
#' @param icon Name of the icon. A full list of available icons can be found
#'   using [fontawesome::fa_metadata()], <https://icons.getbootstrap.com/>,
#'   <https://ionic.io/ionicons> or <https://lucide.dev/icons/>, depending on
#'   `library`. When `library = "text"`, the text (or numbers) to show in each
#'   marker instead, and when `library = "svg"`, a string of SVG markup. Use
#'   `NA` for a marker with no icon, e.g., when the marker shape alone conveys
#'   the meaning.
#'
#' @param markerColor The color of the marker. Not used when `marker = "none"`.
#'
#' @param iconColor The color of the icon. `"auto"` chooses black or white for
#'   each marker, whichever has the greater contrast with `markerColor`. When
#'   `marker = "none"`, `"auto"` instead gives the icon the colour
#'   `markerColor`, as there is no marker for it to contrast with.
#'
#' @param marker The shape of the marker. Defaults to `"marker"`, which uses
#'   the standard teardrop shaped pin, similar to [leaflet::addMarkers()]. The
#'   other built-in shapes are:
#'
#'   - Pointed shapes, which sit on their location with their tip:
#'   `"marker-square"` (a pin with a square head) and `"callout"` (a speech
#'   bubble).
#'
#'   - Shapes centred on their location: `"circle"`, `"square"`, `"diamond"`,
#'   `"triangle"`, `"hexagon"`, `"star"` and `"heart"`.
#'
#'   - `"none"`, which removes the marker entirely and places the icon
#'   directly on the map.
#'
#'   Custom shapes can be created using [markerShape()]. `marker` may be a
#'   character vector of shape names, a single [markerShape()], or a list of
#'   [markerShape()]s and/or shape names to use a different shape for each
#'   marker. Missing (`NA`) shapes are drawn as `"none"`.
#'
#' @param markerSize The width of the marker in pixels. Defaults to `30`, which
#'   is roughly the same size as [leaflet::addMarkers()].
#'
#' @param library One of `"fontawesome"`, `"bootstrap"`, `"ionicons"`, or
#'   `"lucide"` defining the icon library of interest. Defaults to
#'   `"fontawesome"`. Alternatively, `"text"` shows `icon` as text, which is
#'   useful for numbered or lettered markers (e.g., `icon = 1:10`). Text is
#'   bold and shrinks to fit, so is best kept to around three characters.
#'   `"svg"` uses `icon` as raw SVG markup (e.g., `'<svg viewBox="0 0 24
#'   24">...</svg>'`), so any icon can be used. Parts of the SVG which use
#'   `currentColor` or have no fill of their own take on `iconColor`. Only use
#'   SVG markup you trust, as it is inserted directly into legends.
#'
#' @param dir The directory in which markers are saved. By default this is
#'   [tempdir()], which is deleted at the end of each R session. Providing an
#'   alternative directory will allow markers to persist between R sessions.
#'   `dir` is created if it does not already exist.
#'
#' @param shadow Should markers have a shadow? Defaults to `TRUE`. As with the
#'   default [leaflet::addMarkers()] marker, pointed shapes such as `"marker"`
#'   cast a shadow up and to the right, while other shapes have a soft shadow
#'   just beneath them. `"none"` never has a shadow. The shadow of a custom
#'   shape is set using the `shadow` argument of [markerShape()].
#'
#' @param outlineColor The colour of the outline around each marker. Defaults
#'   to a slightly darker shade of `markerColor`. A contrasting colour (e.g.,
#'   `"white"`) helps markers stand out against similarly coloured basemaps.
#'
#' @param outlineWidth The width of the outline in pixels. Defaults to a
#'   subtle outline which scales with `markerSize` (around half a pixel at the
#'   default size). Markers are not shrunk to make room for wider outlines, so
#'   they grow by the extra width.
#'
#' @inheritParams leaflet::makeIcon
#'
#' @return a [leaflet::iconList()], to be passed to the `icon` argument of
#'   [leaflet::addMarkers()]. If any argument is a formula, a formula which
#'   builds that [leaflet::iconList()] from the layer's data is returned
#'   instead.
#'
#' @export
#'
#' @seealso [markerShape()], [addIconLegend()], [iconFactor()], [iconBin()],
#'   [iconQuantile()]
#'
#' @author Jack Davison
#'
#' @examplesIf rlang::is_installed(c("fontawesome", "dplyr"))
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
#'   addTiles() |>
#'   addMarkers(
#'     icon = magicIcons(icon = ~icon, markerColor = ~color, iconColor = "white"),
#'     lat = ~latitude,
#'     lng = ~longitude,
#'     popup = ~site
#'   )
magicIcons <- function(
  icon = "circle",
  markerColor = leaf.magic::awesomePalette$blue,
  iconColor = "auto",
  marker = "marker",
  markerSize = 30L,
  outlineColor = NULL,
  outlineWidth = NULL,
  library = "fontawesome",
  className = NULL,
  dir = tempdir(),
  shadow = TRUE
) {
  library <- check_library(library)
  if (!rlang::is_bool(shadow)) {
    rlang::abort("`shadow` must be `TRUE` or `FALSE`.")
  }

  data_args <- list(
    icon = icon,
    markerColor = markerColor,
    iconColor = iconColor,
    marker = marker,
    markerSize = markerSize,
    outlineColor = if (is.null(outlineColor)) NA_character_ else outlineColor,
    outlineWidth = if (is.null(outlineWidth)) NA_real_ else outlineWidth
  )
  if (any(vapply(data_args, rlang::is_formula, logical(1)))) {
    return(defer_magic_icons(
      data_args,
      list(library = library, className = className, dir = dir, shadow = shadow)
    ))
  }

  shapes <- as_marker_shapes(marker)

  if (is.null(outlineColor)) {
    outlineColor <- NA_character_
  }
  if (is.null(outlineWidth)) {
    outlineWidth <- NA_real_
  }
  if (!is.numeric(outlineWidth) && !all(is.na(outlineWidth))) {
    rlang::abort("`outlineWidth` must be a number of pixels.")
  }

  check_lengths(
    icon = icon,
    markerColor = markerColor,
    iconColor = iconColor,
    marker = shapes,
    markerSize = markerSize,
    outlineColor = outlineColor,
    outlineWidth = outlineWidth
  )

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  # shapes are identified by a hash of their definition, so that identical
  # shapes (built-in or custom) share cached markers
  shape_keys <- vapply(shapes, function(s) rlang::hash(unclass(s)), "")
  shapes <- shapes[!duplicated(shape_keys)]
  names(shapes) <- unique(shape_keys)

  combinations <-
    data.frame(
      icon = icon,
      markerColor = markerColor,
      iconColor = iconColor,
      marker = shape_keys,
      markerSize = markerSize,
      outlineColor = outlineColor,
      outlineWidth = outlineWidth
    )

  combinations$iconColor <- auto_icon_color(
    combinations$iconColor,
    combinations$markerColor,
    shapes[combinations$marker]
  )

  # only build each unique combination once
  key <- do.call(paste, c(combinations, sep = "\x1f"))
  is_first <- !duplicated(key)
  unique_combos <- combinations[is_first, , drop = FALSE]

  make_icon <- function(
    icon,
    markerColor,
    iconColor,
    marker,
    markerSize,
    outlineColor,
    outlineWidth
  ) {
    shape <- shapes[[marker]]
    scale <- markerSize / shape$width

    # outlines default to a slightly darker shade of the marker, 2 shape units
    # wide. Built-in shapes leave a 2 unit margin, so wider outlines would be
    # clipped; instead, the canvas is padded by the extra width.
    if (is.na(outlineColor)) {
      outlineColor <- darken_color(markerColor)
    }
    outline_units <- if (is.na(outlineWidth)) 2 else outlineWidth / scale
    pad <- if (is.null(shape$body)) 0 else max(0, (outline_units - 2) / 2)

    # the package version is part of the cache key so that markers cached in a
    # persistent `dir` are rebuilt when the marker code changes. The rest of
    # the key is hashed so that any icon name is a valid file name, and
    # colours are normalised so that, e.g., "red", "#ff0000" and "#FF0000"
    # share a single cached marker.
    url <- file.path(
      dir,
      paste0(
        "leafmagic-",
        getNamespaceVersion("leaf.magic"),
        "-",
        rlang::hash(list(
          icon,
          if (!is.null(shape$body)) normalise_color(markerColor),
          normalise_color(iconColor),
          if (!is.null(shape$body)) normalise_color(outlineColor),
          if (!is.null(shape$body)) round(outline_units, 4),
          marker,
          library
        )),
        ".svg"
      )
    )

    if (!file.exists(url)) {
      svg <- marker_svg(
        shape,
        icon,
        markerColor,
        iconColor,
        library,
        outlineColor = outlineColor,
        outlineWidth = outline_units,
        pad = pad
      )
      writeLines(enc2utf8(svg), url, useBytes = TRUE)
    }

    # shape units -> pixels
    anchor <- (shape$anchor + pad) * scale
    popup <- (shape$popup - shape$anchor) * scale

    # shadows don't depend on colours or icons, so there's one per shape
    shadow_url <- shadow_width <- shadow_height <- NULL
    shadow_anchor <- NULL
    if (shadow && shape$shadow != "none") {
      shadow_def <- shadow_svg(shape)
      shadow_url <- file.path(
        dir,
        paste0(
          "leafmagic-",
          getNamespaceVersion("leaf.magic"),
          "-shadow-",
          marker,
          ".svg"
        )
      )
      if (!file.exists(shadow_url)) {
        writeLines(enc2utf8(shadow_def$svg), shadow_url, useBytes = TRUE)
      }
      shadow_width <- shadow_def$width * scale
      shadow_height <- shadow_def$height * scale
      # the anchor is the same point of the shape, measured from the corner
      # of the (larger) shadow canvas
      shadow_anchor <- (shape$anchor - c(shadow_def$x, shadow_def$y)) * scale
    }

    leaflet::makeIcon(
      iconUrl = url,
      iconWidth = (shape$width + 2 * pad) * scale,
      iconHeight = (shape$height + 2 * pad) * scale,
      # leaflet ignores anchors whose x is exactly 0, hence the tiny offsets
      iconAnchorX = nonzero(anchor[["x"]]),
      iconAnchorY = anchor[["y"]],
      shadowUrl = shadow_url,
      shadowWidth = shadow_width,
      shadowHeight = shadow_height,
      shadowAnchorX = if (!is.null(shadow_url)) nonzero(shadow_anchor[[1]]),
      shadowAnchorY = if (!is.null(shadow_url)) shadow_anchor[[2]],
      popupAnchorX = nonzero(popup[["x"]]),
      popupAnchorY = popup[["y"]],
      className = className
    )
  }

  icons <- purrr::pmap(unique_combos, make_icon, .progress = TRUE)

  do.call(leaflet::iconList, icons[match(key, key[is_first])])
}

#' Defer building markers until a layer's data is known
#'
#' {leaflet} evaluates any formula passed to `addMarkers(icon = )` with the
#' layer's data as a data mask, before it processes the icons. This returns a
#' single formula which, when evaluated like that, captures the data mask with
#' `environment()`, resolves each formula argument against it in that
#' formula's own environment (as {leaflet} does), and then builds the markers.
#' The helpers live in the formula's environment under dotted names so data
#' columns are unlikely to mask them.
#' @noRd
defer_magic_icons <- function(data_args, other_args) {
  for (nm in names(data_args)) {
    f <- data_args[[nm]]
    if (rlang::is_formula(f) && !rlang::is_formula(f, lhs = FALSE)) {
      rlang::abort(c(
        paste0("`", nm, "` must be a one-sided formula, e.g., `~column`."),
        x = paste0("`", nm, "` is ", deparse1(f), ".")
      ))
    }
  }

  env <- rlang::env(
    rlang::ns_env("leaf.magic"),
    .leafmagic_args = data_args,
    .leafmagic_other = other_args,
    .leafmagic_resolve = resolve_magic_icons
  )
  f <- stats::as.formula(
    quote(
      ~ .leafmagic_resolve(
        .leafmagic_args,
        .leafmagic_other,
        environment()
      )
    ),
    env = env
  )
  structure(f, class = c("leafmagic_icons_formula", class(f)))
}

#' Resolve deferred formula arguments against a data mask, then build markers
#' @noRd
resolve_magic_icons <- function(data_args, other_args, mask) {
  data <- as.list(mask, all.names = TRUE)
  resolved <- lapply(data_args, function(x) {
    if (rlang::is_formula(x)) eval(x[[2]], data, environment(x)) else x
  })
  do.call(magicIcons, c(resolved, other_args))
}

#' @export
print.leafmagic_icons_formula <- function(x, ...) {
  args <- environment(x)$.leafmagic_args
  is_f <- vapply(args, rlang::is_formula, logical(1))
  cat(
    "<magicIcons() to be built from layer data>\n",
    paste0(
      "  ",
      names(args)[is_f],
      ": ",
      vapply(args[is_f], deparse1, ""),
      "\n",
      collapse = ""
    ),
    "Pass to the `icon` argument of `leaflet::addMarkers()`.\n",
    sep = ""
  )
  invisible(x)
}

#' Replace an exact 0 with a tiny number
#'
#' leaflet's JavaScript only uses an icon or popup anchor if its x-coordinate
#' is truthy, so an x of exactly 0 would be silently ignored.
#' @noRd
nonzero <- function(x) {
  if (x == 0) .Machine$double.eps else x
}

#' Build the SVG markup of a whole marker
#'
#' The marker shape is drawn with an outline (by default, a slightly darker
#' shade of `markerColor`), and the icon's own SVG is nested inside it. `pad`
#' extends the canvas on every side so that wide outlines aren't clipped. A
#' missing (`NA`) icon draws the shape alone.
#'
#' By default the SVG's size is its size in shape units (markers are scaled by
#' leaflet's `iconSize`). `width` instead sets its width as a CSS length in
#' `units` (e.g., `"em"`, for legends), with the height following the shape's
#' aspect ratio.
#' @noRd
marker_svg <- function(
  shape,
  icon,
  markerColor,
  iconColor,
  library,
  outlineColor = darken_color(markerColor),
  outlineWidth = 2,
  pad = 0,
  width = NULL,
  units = "em"
) {
  markerColor <- css_color(markerColor)
  outlineColor <- css_color(outlineColor)
  iconColor <- css_color(iconColor)

  body <- NULL
  if (!is.null(shape$body)) {
    body <- paste0(
      '<g fill="',
      html_attr(markerColor),
      '" stroke="',
      html_attr(outlineColor),
      '" stroke-width="',
      format(round(outlineWidth, 4)),
      '" stroke-linejoin="round">',
      shape$body,
      "</g>"
    )
  }

  nested <- NULL
  if (!is.na(icon)) {
    size <- shape$icon[["size"]]
    nested <- nest_svg(
      icon_svg(icon, iconColor, library),
      x = shape$icon[["x"]] - size / 2,
      y = shape$icon[["y"]] - size / 2,
      size = size,
      color = iconColor,
      # Font Awesome icons are given a subtle outline, as in previous versions
      outline = if (library == "fontawesome") darken_color(iconColor)
    )
  }

  canvas_w <- shape$width + 2 * pad
  canvas_h <- shape$height + 2 * pad
  if (is.null(width)) {
    svg_w <- format(canvas_w)
    svg_h <- format(canvas_h)
  } else {
    svg_w <- paste0(fmt_num(width), units)
    svg_h <- paste0(fmt_num(width * canvas_h / canvas_w), units)
  }

  paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="',
    format(-pad),
    " ",
    format(-pad),
    " ",
    format(canvas_w),
    " ",
    format(canvas_h),
    '" width="',
    svg_w,
    '" height="',
    svg_h,
    '">',
    body,
    nested,
    "</svg>"
  )
}

#' Place an icon's SVG at a given position and size inside another SVG
#'
#' The icon's root `<svg>` tag is rewritten: its CSS styles and sizes (e.g.,
#' `height:1em`) are removed, as these would override the position, and `x`,
#' `y`, `width` and `height` attributes are set instead. The icon's `viewBox`
#' is kept so that it is scaled to fit. `color` sets `currentColor`, which is
#' how most libraries colour their icons, and is also used as the fill of icons
#' with no fill of their own.
#' @noRd
nest_svg <- function(svg, x, y, size, color, outline = NULL) {
  # drop anything before the root tag, e.g., an XML declaration or comment
  svg <- sub("^.*?(<svg\\b)", "\\1", svg, perl = TRUE)
  root <- regmatches(svg, regexpr("^<svg\\b[^>]*>", svg, perl = TRUE))
  if (length(root) == 0L) {
    rlang::abort("Icon is not valid SVG markup.", .internal = TRUE)
  }

  attrs <- sub(
    "^<svg\\b",
    "",
    sub("\\s*/?>$", "", root, perl = TRUE),
    perl = TRUE
  )
  attrs <- gsub(
    "\\s(style|width|height|x|y|color)\\s*=\\s*(\"[^\"]*\"|'[^']*')",
    "",
    attrs,
    perl = TRUE
  )
  has_fill <- grepl("\\sfill\\s*=", attrs, perl = TRUE)

  color <- html_attr(color)
  new_attrs <- paste0(
    sprintf(
      ' x="%s" y="%s" width="%s" height="%s" color="%s"',
      format(x),
      format(y),
      format(size),
      format(size),
      color
    ),
    if (!has_fill) sprintf(' fill="%s"', color),
    if (!is.null(outline)) {
      sprintf(
        ' stroke="%s" stroke-width="%s"',
        outline,
        format(svg_view_box_height(attrs) / 100)
      )
    }
  )

  paste0("<svg", attrs, new_attrs, ">", substring(svg, nchar(root) + 1L))
}

#' Height of an SVG viewBox, from the root tag's attributes
#' @noRd
svg_view_box_height <- function(attrs) {
  vb <- regmatches(
    attrs,
    regexec("viewBox\\s*=\\s*[\"']([^\"']*)[\"']", attrs, perl = TRUE)
  )[[1]]
  if (length(vb) < 2L) {
    return(100)
  }
  as.numeric(strsplit(trimws(vb[2]), "[\\s,]+", perl = TRUE)[[1]][4])
}

#' Escape a value for use in an XML attribute
#' @noRd
html_attr <- function(x) {
  as.character(htmltools::htmlEscape(x, attribute = TRUE))
}

#' Build the SVG markup for an icon from any supported library
#'
#' @param icon,color Icon name and fill colour.
#' @param library A library name, already validated by `check_library()`.
#' @param ... Passed to [fontawesome::fa()] only (e.g., `stroke`, `height`).
#' @return A single character string of SVG markup.
#' @noRd
icon_svg <- function(icon, color, library, ...) {
  color <- css_color(color)
  svg <- switch(
    library,
    fontawesome = fontawesome::fa(icon, fill = color, ...),
    bootstrap = gsub(
      "currentColor",
      color,
      as.character(bsicons::bs_icon(icon, size = "1em"))
    ),
    ionicons = read_ionicon(icon, color = color),
    lucide = lucidr::lucide(icon, size = 16, color = color),
    text = text_svg(icon, color),
    svg = user_svg(icon, color)
  )
  paste(as.character(svg), collapse = "\n")
}

#' Build an SVG showing a short piece of text, in the same form as an icon
#'
#' The text is bold, centred, and shrinks to fit the (square) icon area as it
#' gets longer. Markers are shown as images, which can't load web fonts, so a
#' common system font stack is used.
#' @noRd
text_svg <- function(text, color) {
  text <- as.character(text)
  # bold digits and capitals are roughly 0.6em wide; allow text to overhang the
  # icon area slightly, as marker shapes have some room around it
  n <- max(nchar(text, type = "width"), 1L)
  font_size <- min(90, 120 / (0.6 * n))
  paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100" ',
    'overflow="visible" style="vertical-align:-0.125em;height:1em;width:1em">',
    '<text x="50" y="50" dy="0.35em" text-anchor="middle" ',
    'font-family="Helvetica, Arial, sans-serif" font-weight="bold" ',
    'font-size="',
    format(round(font_size, 2)),
    '" fill="',
    html_attr(color),
    '">',
    htmltools::htmlEscape(text),
    "</text></svg>"
  )
}

#' Prepare user-supplied SVG markup to be used as an icon
#'
#' The markup is sized to 1em (like the other libraries' icons, for legends)
#' and coloured with `color` where it uses `currentColor` or has no fill of
#' its own. An SVG with `width` and `height` but no `viewBox` is given one, so
#' that it scales to fit rather than being cropped.
#' @noRd
user_svg <- function(svg, color) {
  svg <- paste(as.character(svg), collapse = "\n")
  root <- regmatches(svg, regexpr("<svg\\b[^>]*>", svg, perl = TRUE))
  if (length(root) == 0L) {
    rlang::abort(
      c(
        "`icon` must be SVG markup when `library = \"svg\"`.",
        x = paste0("Can't find an `<svg>` tag in \"", substr(svg, 1, 40), "\".")
      ),
      call = NULL
    )
  }

  if (!grepl("\\sviewBox\\s*=", root, perl = TRUE)) {
    dim <- function(attr) {
      m <- regmatches(
        root,
        regexec(
          paste0("\\s", attr, "\\s*=\\s*[\"']\\s*([0-9.]+)(px)?\\s*[\"']"),
          root,
          perl = TRUE
        )
      )[[1]]
      if (length(m) < 2L) NA_real_ else as.numeric(m[2])
    }
    w <- dim("width")
    h <- dim("height")
    if (!is.na(w) && !is.na(h)) {
      svg <- sub(
        "<svg\\b",
        sprintf('<svg viewBox="0 0 %s %s"', format(w), format(h)),
        svg,
        perl = TRUE
      )
    }
  }

  svg <- nest_svg(svg, x = 0, y = 0, size = "1em", color = color)
  sub("<svg\\b", '<svg style="vertical-align:-0.125em"', svg, perl = TRUE)
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

#' Resolve `"auto"` icon colours
#'
#' `"auto"` icon colours are black or white, whichever contrasts more with the
#' marker. Shapes with no body (i.e., `"none"`) have no marker to contrast
#' with, so the icon takes on `markerColor` instead. `shapes` is a list of
#' shapes, one per element of `iconColor`.
#' @noRd
auto_icon_color <- function(iconColor, markerColor, shapes) {
  is_auto <- iconColor %in% "auto"
  if (!any(is_auto)) {
    return(iconColor)
  }
  markerColor <- rep_len(markerColor, length(iconColor))[is_auto]
  has_body <- vapply(shapes[is_auto], function(s) !is.null(s$body), logical(1))
  iconColor[is_auto] <- ifelse(
    has_body,
    contrast_color(markerColor),
    markerColor
  )
  iconColor
}

#' Pick black or white, whichever has the higher contrast against `col`
#'
#' Uses the WCAG 2 definitions of relative luminance and contrast ratio. Ties
#' go to white, as in the default `{leaflet}` awesome markers.
#' @noRd
contrast_color <- function(col) {
  rgb <- grDevices::col2rgb(col) / 255
  linear <- ifelse(rgb <= 0.04045, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  lum <- colSums(linear * c(0.2126, 0.7152, 0.0722))
  black_contrast <- (lum + 0.05) / 0.05
  white_contrast <- 1.05 / (lum + 0.05)
  ifelse(black_contrast > white_contrast, "#000000", "#FFFFFF")
}

#' Normalise a colour to an uppercase "#RRGGBBAA" hex code
#' @noRd
normalise_color <- function(col) {
  rgba <- grDevices::col2rgb(col, alpha = TRUE)
  grDevices::rgb(
    rgba[1, ],
    rgba[2, ],
    rgba[3, ],
    rgba[4, ],
    maxColorValue = 255
  )
}

#' Convert R colours to CSS colours
#'
#' R accepts colour names that CSS doesn't (e.g., `"grey40"`, `"navyblue"`),
#' which browsers would draw as black, so colours are converted to hex codes.
#' Transparency is kept as an "#RRGGBBAA" code.
#' @noRd
css_color <- function(col) {
  hex <- normalise_color(col)
  ifelse(endsWith(hex, "FF"), substr(hex, 1, 7), hex)
}

#' Check library is valid
#'
#' @param check_installed Check that the package providing the library is
#'   installed? Not needed if no icons are drawn (e.g., for attributions).
#' @noRd
check_library <- function(library, check_installed = TRUE) {
  library <- rlang::arg_match(
    library,
    c("fontawesome", "bootstrap", "ionicons", "lucide", "text", "svg")
  )

  if (!check_installed) {
    return(library)
  }

  pkg <- switch(
    library,
    fontawesome = "fontawesome",
    bootstrap = "bsicons",
    lucide = "lucidr"
  )
  if (!is.null(pkg)) {
    rlang::check_installed(
      pkg,
      reason = paste0("to use `library = \"", library, "\"`.")
    )
  }

  library
}
