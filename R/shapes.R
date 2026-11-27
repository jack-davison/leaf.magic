#' Define a custom marker shape
#'
#' Creates a marker shape for use with the `marker` argument of [magicIcons()].
#' A shape is drawn using SVG in its own coordinate system, `width` units wide
#' and `height` units tall. The icon is then placed in a square within the
#' shape, and the whole marker is scaled so that it is `markerSize` pixels wide
#' on the map. All of the built-in shapes (`"marker"`, `"circle"`, etc.) are
#' defined in this way.
#'
#' @section Drawing shapes: The shape is filled with `markerColor` and outlined
#'   in a slightly darker colour with a stroke width of `2` units, so `path`
#'   should not usually set its own `fill` or `stroke`. As the outline is
#'   centred on the edge of the shape, leave a margin of at least `1` unit
#'   between the shape and the edges of the coordinate system so that it isn't
#'   clipped.
#'
#' @param path The shape to draw. Either the `d` attribute of an SVG `<path>`
#'   (e.g., `"M50 2 98 98 2 98z"` for a triangle), or SVG markup of one or more
#'   elements (e.g., `'<circle cx="50" cy="50" r="48"/>'`). Use `NULL` for no
#'   shape, in which case only the icon is drawn.
#' @param width,height The size of the coordinate system the shape is drawn in.
#'   Their ratio defines the aspect ratio of the marker.
#' @param icon Where to draw the icon, as a numeric vector of the `x` and `y`
#'   coordinates of its centre and its `size` (the side of the square the icon
#'   is fitted into). Unnamed vectors are taken to be in the order `x`, `y`,
#'   `size`.
#' @param anchor The coordinates of the point of the shape which sits on the
#'   marker's location on the map, as a numeric vector of `x` and `y`. Defaults
#'   to the centre of the shape. For pointed shapes, such as `"marker"`, this
#'   should be the tip.
#' @param popup The coordinates from which popups open, as a numeric vector of
#'   `x` and `y`. Defaults to `anchor`.
#' @param shadow The style of shadow drawn beneath the marker when
#'   `magicIcons(shadow = TRUE)`. One of:
#'   - `"drop"` (the default): a soft shadow just below the shape, as if it
#'   were lying flat on the map. Suits shapes centred on their location.
#'   - `"cast"`: a shadow cast from `anchor` up and to the right, as if the
#'   shape were standing upright on its tip, like the default
#'   [leaflet::addMarkers()] marker. Suits pointed shapes.
#'   - `"none"`: no shadow.
#'
#' @section Shadows: As in `{leaflet}`, shadows are drawn as separate images in
#'   the map's shadow pane, so a marker's shadow never covers another marker.
#'   Shadows are generated from the shape itself, so work for any shape. Shapes
#'   with no `path` never have a shadow.
#'
#' @return A `leafmagic_marker_shape` object.
#' @export
#'
#' @seealso [magicIcons()]
#'
#' @examples
#' # a downwards-pointing triangle, anchored at its tip
#' arrow <- markerShape(
#'   "M2 2h96L50 98z",
#'   icon = c(x = 50, y = 34, size = 36),
#'   anchor = c(x = 50, y = 98),
#'   popup = c(x = 50, y = 10),
#'   shadow = "cast"
#' )
#' arrow
#'
#' @examplesIf rlang::is_installed("fontawesome")
#' # use it with magicIcons()
#' library(leaflet)
#' leaflet(port_talbot) |>
#'   addTiles() |>
#'   addMarkers(
#'     lng = ~longitude,
#'     lat = ~latitude,
#'     icon = magicIcons("industry", marker = arrow)
#'   )
markerShape <- function(
  path,
  width = 100,
  height = 100,
  icon = c(x = width / 2, y = height / 2, size = min(width, height) / 2),
  anchor = c(x = width / 2, y = height / 2),
  popup = anchor,
  shadow = c("drop", "cast", "none")
) {
  shadow <- rlang::arg_match(shadow)
  check_number(width, "width")
  check_number(height, "height")

  if (!is.null(path)) {
    if (!is.character(path) || length(path) != 1L || is.na(path)) {
      rlang::abort("`path` must be a single string or `NULL`.")
    }
    path <- trimws(path)
    if (!startsWith(path, "<")) {
      path <- paste0('<path d="', html_attr(path), '"/>')
    }
  }

  icon <- check_coords(icon, c("x", "y", "size"), "icon")
  if (icon[["size"]] <= 0) {
    rlang::abort("The `size` of `icon` must be positive.")
  }
  anchor <- check_coords(anchor, c("x", "y"), "anchor")
  popup <- check_coords(popup, c("x", "y"), "popup")

  structure(
    list(
      body = path,
      width = width,
      height = height,
      icon = icon,
      anchor = anchor,
      popup = popup,
      shadow = if (is.null(path)) "none" else shadow
    ),
    class = "leafmagic_marker_shape"
  )
}

#' @export
print.leafmagic_marker_shape <- function(x, ...) {
  cat(
    "<markerShape> ",
    format(x$width),
    " x ",
    format(x$height),
    "\n",
    "* icon: size ",
    format(x$icon[["size"]]),
    " centred at (",
    format(x$icon[["x"]]),
    ", ",
    format(x$icon[["y"]]),
    ")\n",
    "* anchor: (",
    format(x$anchor[["x"]]),
    ", ",
    format(x$anchor[["y"]]),
    ")\n",
    "* popup: (",
    format(x$popup[["x"]]),
    ", ",
    format(x$popup[["y"]]),
    ")\n",
    "* shadow: ",
    x$shadow,
    "\n",
    if (is.null(x$body)) "* no shape; icon only\n",
    sep = ""
  )
  invisible(x)
}

#' Check a single positive, finite number
#' @noRd
check_number <- function(x, arg, call = rlang::caller_env()) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0) {
    rlang::abort(
      paste0("`", arg, "` must be a single positive number."),
      call = call
    )
  }
}

#' Check and name a vector of coordinates
#' @noRd
check_coords <- function(x, names, arg, call = rlang::caller_env()) {
  if (!is.numeric(x) || length(x) != length(names) || !all(is.finite(x))) {
    rlang::abort(
      paste0(
        "`",
        arg,
        "` must be a numeric vector of ",
        paste0("`", names, "`", collapse = ", "),
        "."
      ),
      call = call
    )
  }
  if (is.null(names(x))) {
    names(x) <- names
  } else if (!setequal(names(x), names)) {
    rlang::abort(
      paste0(
        "`",
        arg,
        "` must be named ",
        paste0("`", names, "`", collapse = ", "),
        ", or unnamed."
      ),
      call = call
    )
  }
  x[names]
}

#' Points of a regular polygon or star, as an SVG `points` attribute
#'
#' Vertices alternate between each of `radii` (one radius gives a regular
#' polygon; two give a star), starting at `start` radians.
#' @noRd
polygon_points <- function(cx, cy, n, radii, start = -pi / 2) {
  n_points <- n * length(radii)
  angle <- start + (seq_len(n_points) - 1) * 2 * pi / n_points
  radius <- rep_len(radii, n_points)
  paste(
    sprintf(
      "%.2f,%.2f",
      cx + radius * cos(angle),
      cy + radius * sin(angle)
    ),
    collapse = " "
  )
}

#' The built-in marker shapes
#' @noRd
marker_shapes <- list(
  marker = markerShape(
    "M50 130C38 114 2 80 2 50a48 48 0 0 1 96 0c0 30-36 64-48 80z",
    width = 100,
    height = 132,
    icon = c(50, 50, 52),
    anchor = c(50, 130),
    popup = c(50, 24),
    shadow = "cast"
  ),
  `marker-square` = markerShape(
    "M16 2h68a14 14 0 0 1 14 14v68a14 14 0 0 1-14 14H64l-14 24-14-24H16A14 14 0 0 1 2 84V16A14 14 0 0 1 16 2z",
    width = 100,
    height = 124,
    icon = c(50, 50, 54),
    anchor = c(50, 122),
    popup = c(50, 22),
    shadow = "cast"
  ),
  circle = markerShape(
    '<circle cx="50" cy="50" r="48"/>',
    icon = c(50, 50, 52)
  ),
  square = markerShape(
    '<rect x="2" y="2" width="96" height="96" rx="14"/>',
    icon = c(50, 50, 54)
  ),
  diamond = markerShape(
    "M50 2 98 50 50 98 2 50z",
    icon = c(50, 50, 42)
  ),
  triangle = markerShape(
    paste0('<polygon points="', polygon_points(50, 57, 3, 55), '"/>'),
    width = 100,
    height = 87,
    icon = c(50, 61, 38),
    anchor = c(50, 57)
  ),
  hexagon = markerShape(
    paste0('<polygon points="', polygon_points(50, 44, 6, 48, 0), '"/>'),
    width = 100,
    height = 88,
    icon = c(50, 44, 50)
  ),
  star = markerShape(
    paste0('<polygon points="', polygon_points(50, 52, 5, c(48, 27)), '"/>'),
    width = 100,
    height = 95,
    icon = c(50, 53, 32),
    anchor = c(50, 52)
  ),
  heart = markerShape(
    "M50 87C44 83 2 57 2 30C2 14 14 2 28 2c10 0 18 6 22 14C54 8 62 2 72 2c14 0 26 12 26 28 0 27-42 53-48 57z",
    width = 100,
    height = 90,
    icon = c(50, 39, 40)
  ),
  callout = markerShape(
    "M14 2h72a12 12 0 0 1 12 12v52a12 12 0 0 1-12 12H38L14 96l6-18h-6A12 12 0 0 1 2 66V14A12 12 0 0 1 14 2z",
    width = 100,
    height = 98,
    icon = c(50, 40, 50),
    anchor = c(14, 96),
    popup = c(50, 4),
    shadow = "cast"
  ),
  none = markerShape(NULL, icon = c(50, 50, 96))
)

#' Resolve the `marker` argument of magicIcons() into a list of shapes
#'
#' `marker` may be a character vector of built-in shape names (which may be
#' abbreviated), a single shape from markerShape(), or a list of shapes and/or
#' names. Missing (`NA`) shapes are `"none"`, i.e., the icon alone, so that a
#' shape scale's `na.icon = NA` behaves like a colour scale's transparent
#' `na.color`.
#' @noRd
as_marker_shapes <- function(marker, call = rlang::caller_env()) {
  if (inherits(marker, "leafmagic_marker_shape")) {
    return(list(marker))
  }
  if (is.factor(marker) || is.logical(marker)) {
    marker <- as.character(marker)
  }
  if (is.character(marker)) {
    marker <- as.list(marker)
  }
  if (is.list(marker)) {
    is_na <- vapply(
      marker,
      function(x) is.atomic(x) && length(x) == 1L && is.na(x),
      logical(1)
    )
    marker[is_na] <- list("none")
  }
  if (!is.list(marker)) {
    rlang::abort(
      "`marker` must be a character vector of shape names, or shapes created by `markerShape()`.",
      call = call
    )
  }

  is_shape <- vapply(marker, inherits, logical(1), "leafmagic_marker_shape")
  is_name <- vapply(
    marker,
    function(x) is.character(x) && length(x) == 1L && !is.na(x),
    logical(1)
  )
  if (!all(is_shape | is_name)) {
    rlang::abort(
      "Each `marker` must be a shape name or a shape created by `markerShape()`.",
      call = call
    )
  }

  names <- unlist(marker[is_name], use.names = FALSE)
  idx <- pmatch(names, names(marker_shapes), duplicates.ok = TRUE)
  if (anyNA(idx)) {
    bad <- unique(names[is.na(idx)])
    rlang::abort(
      c(
        paste0(
          "Unknown `marker` shape: ",
          paste0("\"", bad, "\"", collapse = ", "),
          "."
        ),
        i = paste0(
          "Built-in shapes are ",
          paste0("\"", names(marker_shapes), "\"", collapse = ", "),
          "."
        ),
        i = "Create your own shapes using `markerShape()`."
      ),
      call = call
    )
  }
  marker[is_name] <- marker_shapes[idx]
  unname(marker)
}

#' Build the SVG of a shape's shadow
#'
#' Shadows are drawn in the shape's own coordinate system, but on a canvas
#' large enough to contain the (transformed, blurred) shadow. The shape is
#' turned into a translucent black silhouette using a colour matrix, so any
#' fills set by a custom shape are ignored, and then blurred.
#'
#' - `"cast"` shadows are squashed vertically and sheared to the right about
#'   the anchor point, as though the shape were standing on its tip with the
#'   light behind and to the left, mimicking leaflet's default marker shadow.
#' - `"drop"` shadows are offset slightly downwards.
#'
#' @return A list of the SVG markup and the canvas `x`, `y`, `width` and
#'   `height` in shape units.
#' @noRd
shadow_svg <- function(shape) {
  w <- shape$width
  h <- shape$height
  corners_x <- c(0, w, 0, w)
  corners_y <- c(0, 0, h, h)

  if (shape$shadow == "cast") {
    ay <- shape$anchor[["y"]]
    squash <- 0.5
    blur <- 2.5
    opacity <- 0.35
    # x' = x - squash * (y - ay); y' = ay + squash * (y - ay)
    transform <- sprintf(
      "matrix(1 0 %s %s %s %s)",
      fmt_num(-squash),
      fmt_num(squash),
      fmt_num(squash * ay),
      fmt_num((1 - squash) * ay)
    )
    tx <- corners_x - squash * (corners_y - ay)
    ty <- ay + squash * (corners_y - ay)
  } else {
    offset <- c(0, 0.04 * h)
    blur <- 3
    opacity <- 0.3
    transform <- sprintf(
      "translate(%s %s)",
      fmt_num(offset[1]),
      fmt_num(offset[2])
    )
    tx <- corners_x + offset[1]
    ty <- corners_y + offset[2]
  }

  # leave room for the blur, which extends about 3 standard deviations
  margin <- 3 * blur
  x <- floor(min(tx) - margin)
  y <- floor(min(ty) - margin)
  width <- ceiling(max(tx) + margin) - x
  height <- ceiling(max(ty) + margin) - y
  box <- paste(fmt_num(c(x, y, width, height)), collapse = " ")

  svg <- paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="',
    box,
    '" width="',
    fmt_num(width),
    '" height="',
    fmt_num(height),
    '">',
    '<defs><filter id="shadow" filterUnits="userSpaceOnUse" x="',
    fmt_num(x),
    '" y="',
    fmt_num(y),
    '" width="',
    fmt_num(width),
    '" height="',
    fmt_num(height),
    '">',
    '<feColorMatrix type="matrix" values="0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ',
    fmt_num(opacity),
    ' 0"/>',
    '<feGaussianBlur stdDeviation="',
    fmt_num(blur),
    '"/></filter></defs>',
    '<g filter="url(#shadow)"><g transform="',
    transform,
    '" fill="#000" stroke="#000" stroke-width="2" stroke-linejoin="round">',
    shape$body,
    "</g></g></svg>"
  )

  list(svg = svg, x = x, y = y, width = width, height = height)
}

#' Format numbers for SVG attributes, without scientific notation
#' @noRd
fmt_num <- function(x) {
  vapply(
    round(x, 4),
    format,
    character(1),
    scientific = FALSE,
    trim = TRUE,
    drop0trailing = TRUE
  )
}
