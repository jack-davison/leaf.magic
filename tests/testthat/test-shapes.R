test_that("markerShape() wraps path data and validates its inputs", {
  shape <- markerShape("M2 2h96L50 98z")
  expect_s3_class(shape, "leafmagic_marker_shape")
  expect_equal(shape$body, '<path d="M2 2h96L50 98z"/>')
  # markup is used as-is
  expect_equal(markerShape("<circle r='1'/>")$body, "<circle r='1'/>")
  expect_null(markerShape(NULL)$body)

  # defaults: icon and anchor centred, popup at the anchor
  shape <- markerShape("M0 0", width = 100, height = 60)
  expect_equal(shape$icon, c(x = 50, y = 30, size = 30))
  expect_equal(shape$anchor, c(x = 50, y = 30))
  expect_equal(shape$popup, shape$anchor)

  # unnamed coordinates are named, named ones are reordered
  shape <- markerShape("M0 0", icon = c(1, 2, 3), anchor = c(y = 5, x = 4))
  expect_equal(shape$icon, c(x = 1, y = 2, size = 3))
  expect_equal(shape$anchor, c(x = 4, y = 5))

  expect_error(markerShape(1), "`path` must be a single string")
  expect_error(markerShape("M0 0", width = -1), "`width` must be")
  expect_error(markerShape("M0 0", icon = c(1, 2)), "`icon` must be")
  expect_error(markerShape("M0 0", icon = c(1, 2, 0)), "must be positive")
  expect_error(markerShape("M0 0", anchor = c(a = 1, b = 2)), "must be named")
  expect_output(print(markerShape("M0 0")), "<markerShape> 100 x 100")
})

test_that("built-in shapes fit inside their coordinate systems", {
  for (name in names(marker_shapes)) {
    shape <- marker_shapes[[name]]
    half <- shape$icon[["size"]] / 2
    expect_true(shape$icon[["x"]] - half >= 0, label = name)
    expect_true(shape$icon[["x"]] + half <= shape$width, label = name)
    expect_true(shape$icon[["y"]] - half >= 0, label = name)
    expect_true(shape$icon[["y"]] + half <= shape$height, label = name)
    expect_true(all(shape$anchor >= 0), label = name)
    expect_true(shape$anchor[["y"]] <= shape$height, label = name)
  }
})

test_that("`marker` accepts names, shapes and lists of both", {
  arrow <- markerShape("M2 2h96L50 98z", anchor = c(50, 98))

  expect_equal(as_marker_shapes("circle"), list(marker_shapes$circle))
  # abbreviations are allowed, as with match.arg()
  expect_equal(as_marker_shapes("hex"), list(marker_shapes$hexagon))
  expect_equal(as_marker_shapes(arrow), list(arrow))
  expect_equal(
    as_marker_shapes(list(arrow, "circle")),
    list(arrow, marker_shapes$circle)
  )
  expect_equal(
    as_marker_shapes(factor(c("star", "star"))),
    list(marker_shapes$star, marker_shapes$star)
  )

  expect_error(as_marker_shapes("blob"), "Unknown `marker` shape: \"blob\"")
  expect_error(as_marker_shapes(NA_character_), "shape name")
  expect_error(as_marker_shapes(1), "`marker` must be")
  expect_error(as_marker_shapes(list(1)), "shape name")
})

test_that("custom shapes are used by magicIcons()", {
  skip_if_not_installed("fontawesome")
  skip_if_not_installed("xml2")
  dir <- tempfile("leafmagic-test-")
  on.exit(unlink(dir, recursive = TRUE))

  arrow <- markerShape(
    "M2 2h96L50 98z",
    width = 100,
    height = 100,
    icon = c(50, 34, 36),
    anchor = c(50, 98),
    popup = c(50, 10)
  )
  icons <- magicIcons(
    "car",
    marker = list(arrow, "circle", arrow),
    markerSize = 50,
    dir = dir
  )
  expect_length(icons, 3)
  # identical shapes share a cached marker
  expect_equal(icons[[1]]$iconUrl, icons[[3]]$iconUrl)
  expect_length(grep("-shadow-", list.files(dir), invert = TRUE, value = TRUE), 2)

  svg <- xml2::read_xml(icons[[1]]$iconUrl)
  expect_equal(
    xml2::xml_attr(xml2::xml_find_first(svg, "//*[local-name()='path']"), "d"),
    "M2 2h96L50 98z"
  )

  # anchors and popups are scaled from shape units to pixels
  expect_equal(icons[[1]]$iconAnchorX, 25)
  expect_equal(icons[[1]]$iconAnchorY, 49)
  expect_equal(icons[[1]]$popupAnchorY, -44)

  # a custom shape identical to a built-in one shares its cached marker
  circle <- markerShape(
    '<circle cx="50" cy="50" r="48"/>',
    icon = c(50, 50, 52)
  )
  expect_equal(
    magicIcons("car", marker = circle, dir = dir)[[1]]$iconUrl,
    icons[[2]]$iconUrl
  )
})

test_that("anchors at x = 0 aren't ignored by leaflet", {
  skip_if_not_installed("fontawesome")
  dir <- tempfile("leafmagic-test-")
  on.exit(unlink(dir, recursive = TRUE))

  corner <- markerShape("M0 0h100v100z", anchor = c(0, 100), popup = c(0, 0))
  icon <- magicIcons("car", marker = corner, dir = dir)[[1]]
  expect_gt(icon$iconAnchorX, 0)
  expect_gt(icon$popupAnchorX, 0)
})

test_that("markerShape() validates `shadow`", {
  expect_equal(markerShape("M0 0")$shadow, "drop")
  expect_equal(markerShape("M0 0", shadow = "cast")$shadow, "cast")
  # shapes with no path have nothing to cast a shadow
  expect_equal(markerShape(NULL, shadow = "cast")$shadow, "none")
  expect_error(markerShape("M0 0", shadow = "blob"), "`shadow` must be one of")
  expect_equal(marker_shapes$marker$shadow, "cast")
  expect_equal(marker_shapes$circle$shadow, "drop")
  expect_equal(marker_shapes$none$shadow, "none")
})

test_that("shadow SVGs are well-formed and contain the whole shadow", {
  skip_if_not_installed("xml2")
  for (name in setdiff(names(marker_shapes), "none")) {
    shape <- marker_shapes[[name]]
    shadow <- shadow_svg(shape)
    svg <- xml2::read_xml(shadow$svg)
    expect_equal(
      xml2::xml_attr(svg, "viewBox"),
      paste(shadow$x, shadow$y, shadow$width, shadow$height),
      label = name
    )
    # the canvas always contains the shape's anchor
    expect_true(shape$anchor[["x"]] > shadow$x, label = name)
    expect_true(shape$anchor[["y"]] > shadow$y, label = name)
  }

  # cast shadows lean to the right of the shape and are squashed
  pin <- shadow_svg(marker_shapes$marker)
  expect_gt(pin$x + pin$width, marker_shapes$marker$width + 30)
  expect_lt(pin$height, marker_shapes$marker$height)
})

test_that("magicIcons() adds shadows, positioned by the anchor", {
  skip_if_not_installed("fontawesome")
  dir <- tempfile("leafmagic-test-")
  on.exit(unlink(dir, recursive = TRUE))

  icons <- magicIcons(
    "car",
    c("red", "blue"),
    marker = c("marker", "marker"),
    markerSize = 50,
    dir = dir
  )
  pin <- icons[[1]]
  expect_match(pin$shadowUrl, "-shadow-.*[.]svg$")
  # one shadow per shape, whatever the colours
  expect_equal(icons[[2]]$shadowUrl, pin$shadowUrl)

  # the shadow canvas, in pixels, is placed so the shape's anchor lines up
  shadow <- shadow_svg(marker_shapes$marker)
  expect_equal(pin$shadowWidth, shadow$width / 2)
  expect_equal(pin$shadowHeight, shadow$height / 2)
  expect_equal(pin$shadowAnchorX, (50 - shadow$x) / 2)
  expect_equal(pin$shadowAnchorY, (130 - shadow$y) / 2)

  expect_null(magicIcons("car", marker = "none", dir = dir)[[1]]$shadowUrl)
  expect_null(magicIcons("car", shadow = FALSE, dir = dir)[[1]]$shadowUrl)
  expect_error(magicIcons("car", shadow = NA, dir = dir), "`shadow` must be")
})
