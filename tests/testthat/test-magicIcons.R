test_that("Constants work", {
  skip_if_not_installed("fontawesome")
  testthat::expect_no_error(
    magicIcons("circle", "blue", "white")
  )
})

test_that("Constants work (bootstrap)", {
  skip_if_not_installed("bsicons")
  testthat::expect_no_error(
    magicIcons("circle", "blue", "white", library = "bootstrap")
  )
})

test_that("Vectors work", {
  skip_if_not_installed("fontawesome")
  testthat::expect_no_error(
    magicIcons(c("circle", "x"), "blue", "white")
  )
  testthat::expect_no_error(
    magicIcons(c("circle", "x"), c("blue", "red"), "white")
  )
  testthat::expect_no_error(
    magicIcons(c("circle", "x"), c("blue", "red"), c("white", "black"))
  )
})

test_that("Vectors work (bootstrap)", {
  skip_if_not_installed("bsicons")
  testthat::expect_no_error(
    magicIcons(c("circle", "x"), "blue", "white", library = "bootstrap")
  )
  testthat::expect_no_error(
    magicIcons(
      c("circle", "x"),
      c("blue", "red"),
      "white",
      library = "bootstrap"
    )
  )
  testthat::expect_no_error(
    magicIcons(
      c("circle", "x"),
      c("blue", "red"),
      c("white", "black"),
      library = "bootstrap"
    )
  )
})

test_that("Unequal vectors fail", {
  skip_if_not_installed("fontawesome")
  testthat::expect_error(
    magicIcons(c("circle", "x"), c("red", "blue", "green"), "black"),
    "`icon` has length 2"
  )
  # lengths that divide evenly must not be silently recycled
  testthat::expect_error(
    magicIcons(c("circle", "x"), c("red", "blue", "green", "pink"), "black"),
    "`icon` has length 2"
  )
  testthat::expect_error(
    magicIcons(character(), "red", "black"),
    "`icon` has length 0"
  )
})

test_that("Markers are cached by package version and temp files are removed", {
  skip_if_not_installed("fontawesome")
  dir <- tempfile("leafmagic-test-")
  on.exit(unlink(dir, recursive = TRUE))

  # a missing `dir` is created
  before <- list.files(tempdir())
  icons <- magicIcons("car", "red", "white", dir = dir, shadow = FALSE)
  expect_true(dir.exists(dir))

  files <- list.files(dir)
  expect_length(files, 1)
  expect_match(files, paste0("^leafmagic-", getNamespaceVersion("leaf.magic")))
  expect_equal(icons[[1]]$iconUrl, file.path(dir, files))

  # intermediate logo/pin PNGs are cleaned up
  new_tmp <- setdiff(list.files(tempdir()), before)
  expect_false(any(grepl("^(logo|pin)_", new_tmp)))

  # equivalent colours share a single cached marker
  magicIcons(
    "car",
    c("#FF0000", "#ff0000", "red"),
    "WHITE",
    dir = dir,
    shadow = FALSE
  )
  expect_length(list.files(dir), 1)

  # cache file names are always valid, whatever the icon name
  expect_match(files, "^leafmagic-[0-9.]+-[0-9a-f]+\\.svg$")
})

test_that("Ionicons download failures give an informative error", {
  testthat::local_mocked_bindings(
    readLines = function(...) stop("cannot open URL"),
    .package = "base"
  )
  expect_error(
    read_ionicon("definitely-not-an-icon-xyz", "red"),
    "Could not download the Ionicons icon"
  )
})

test_that("Ionicons are cached within a session", {
  ionicon_cache[["cached-test-icon"]] <- '<svg viewBox="0 0 1 1"></svg>'
  on.exit(rm("cached-test-icon", envir = ionicon_cache))
  out <- read_ionicon("cached-test-icon", "red")
  expect_match(out, "fill:red")
})

test_that("addIconLegend uses non-breaking spaces and checks lengths", {
  skip_if_not_installed("fontawesome")
  map <- addIconLegend(
    leaflet::leaflet(),
    icons = c("car", "house"),
    labels = c("Car", "House"),
    colors = "red"
  )
  html <- map$x$calls[[1]]$args[[1]]
  expect_match(html, "&nbsp;&nbsp;&nbsp;Car", fixed = TRUE)
  expect_error(
    addIconLegend(
      leaflet::leaflet(),
      icons = c("car", "house"),
      labels = c("a", "b", "c", "d")
    ),
    "`icons` has length 2"
  )
  expect_error(
    addIconLegend(leaflet::leaflet(), icons = c("car", "house")),
    "Both `icons` and `labels` must be provided"
  )
  expect_error(
    addIconLegend(leaflet::leaflet(), labels = c("Car", "House")),
    "Both `icons` and `labels` must be provided"
  )
})

test_that("addIconAttribution doesn't request any tiles", {
  map <- addIconAttribution(leaflet::leaflet(), group = "g")
  call <- map$x$calls[[1]]
  expect_equal(call$method, "addTiles")
  expect_match(call$args[[1]], "^data:image/png;base64,")
  expect_match(call$args[[4]]$attribution, "FontAwesome")
})

test_that("icon_svg returns a single SVG string for every library", {
  skip_if_not_installed("fontawesome")
  expect_length(icon_svg("car", "red", "fontawesome"), 1)
  skip_if_not_installed("bsicons")
  expect_length(icon_svg("car-front", "red", "bootstrap"), 1)
  skip_if_not_installed("lucidr")
  expect_length(icon_svg("car", "red", "lucide"), 1)
})

test_that("missing icon packages give an informative error", {
  local_mocked_bindings(
    check_installed = function(pkg, reason, ...) stop(pkg, " ", reason),
    .package = "rlang"
  )
  expect_error(check_library("fontawesome"), "fontawesome.*library")
  expect_error(check_library("bootstrap"), "bsicons")
  expect_equal(check_library("ionicons"), "ionicons")
  # attributions don't draw icons, so don't need the package
  expect_no_error(addIconAttribution(leaflet::leaflet()))
})

test_that("nest_svg positions icons and strips conflicting styles", {
  skip_if_not_installed("xml2")
  svg <- paste0(
    '<?xml version="1.0"?><svg xmlns="http://www.w3.org/2000/svg" ',
    'width="16" height="16" style="height:1em;fill:red" viewBox="0 0 24 24">',
    '<path d="M0 0h24v24z" stroke="currentColor"/></svg>'
  )
  out <- nest_svg(svg, x = 10, y = 20, size = 30, color = "#123456")
  root <- xml2::read_xml(out)
  attrs <- xml2::xml_attrs(root)

  expect_false("style" %in% names(attrs))
  expect_equal(
    unname(attrs[c("x", "y", "width", "height", "viewBox")]),
    c("10", "20", "30", "30", "0 0 24 24")
  )
  # `color` sets currentColor; icons with no fill of their own are filled
  expect_equal(unname(attrs[c("color", "fill")]), c("#123456", "#123456"))
  expect_length(xml2::xml_children(root), 1)

  # an existing fill (e.g., Lucide's fill="none") is kept
  out <- nest_svg(
    '<svg viewBox="0 0 24 24" fill="none"><path d="M0 0"/></svg>',
    0,
    0,
    10,
    "red"
  )
  expect_equal(xml2::xml_attr(xml2::read_xml(out), "fill"), "none")

  # outlines scale with the icon's viewBox
  out <- nest_svg('<svg viewBox="0 0 512 512"></svg>', 0, 0, 10, "red", "black")
  expect_equal(xml2::xml_attr(xml2::read_xml(out), "stroke-width"), "5.12")
})

test_that("markers are well-formed SVGs for every shape and library", {
  skip_if_not_installed("xml2")
  libraries <- list(
    fontawesome = "car",
    bootstrap = "car-front",
    lucide = "car",
    ionicons = "leafmagic-test-icon"
  )
  dir <- tempfile("leafmagic-test-")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  for (lib in names(libraries)) {
    pkg <- c(
      fontawesome = "fontawesome",
      bootstrap = "bsicons",
      lucide = "lucidr"
    )[lib]
    if (!is.na(pkg) && !rlang::is_installed(pkg)) {
      next
    }
    if (lib == "ionicons") {
      ionicon_cache[["leafmagic-test-icon"]] <- paste0(
        '<svg xmlns="http://www.w3.org/2000/svg" class="ionicon" viewBox="0 0 512 512">',
        '<path d="M80 224h352" fill="none" stroke="currentColor"/></svg>'
      )
      on.exit(rm("leafmagic-test-icon", envir = ionicon_cache), add = TRUE)
    }

    for (marker in names(marker_shapes)) {
      icon <- magicIcons(
        libraries[[lib]],
        "red",
        "white",
        marker = marker,
        library = lib,
        dir = dir
      )[[1]]
      expect_match(icon$iconUrl, "\\.svg$")
      svg <- xml2::read_xml(icon$iconUrl)
      shape <- marker_shapes[[marker]]
      expect_equal(
        xml2::xml_attr(svg, "viewBox"),
        paste("0 0", shape$width, shape$height)
      )
      # the icon is always the last element, and is a nested SVG
      nested <- xml2::xml_children(svg)
      expect_equal(xml2::xml_name(nested[[length(nested)]]), "svg")
    }
  }
})

test_that("pins are anchored at their tip, other markers at their centre", {
  skip_if_not_installed("fontawesome")
  dir <- tempfile("leafmagic-test-")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  pin <- magicIcons("car", marker = "marker", markerSize = 50, dir = dir)[[1]]
  expect_equal(pin$iconWidth, 50)
  expect_equal(pin$iconHeight, 66)
  expect_equal(pin$iconAnchorX, 25)
  expect_equal(pin$iconAnchorY, 65)

  circle <- magicIcons("car", marker = "circle", markerSize = 50, dir = dir)[[
    1
  ]]
  expect_equal(circle$iconHeight, 50)
  expect_equal(c(circle$iconAnchorX, circle$iconAnchorY), c(25, 25))
})

test_that("Formulas are evaluated against the layer's data", {
  skip_if_not_installed("fontawesome")
  df <- data.frame(
    lng = c(0, 1, 2),
    lat = c(0, 1, 2),
    type = c("car", "city", "car"),
    col = c("red", "blue", "red"),
    size = c(20, 30, 40)
  )

  deferred <- magicIcons(~type, ~col, "white", markerSize = ~size)
  expect_true(rlang::is_formula(deferred))
  expect_output(print(deferred), "icon: ~type")

  map <- leaflet::leaflet(df) |>
    leaflet::addMarkers(lng = ~lng, lat = ~lat, icon = deferred)
  eager <- leaflet::leaflet(df) |>
    leaflet::addMarkers(
      lng = ~lng,
      lat = ~lat,
      icon = magicIcons(df$type, df$col, "white", markerSize = df$size)
    )
  expect_identical(map$x$calls, eager$x$calls)

  # data passed to the layer takes precedence over the map's data
  sub <- df[2, ]
  map <- leaflet::leaflet(df) |>
    leaflet::addMarkers(data = sub, lng = ~lng, lat = ~lat, icon = deferred)
  eager <- leaflet::leaflet(df) |>
    leaflet::addMarkers(
      data = sub,
      lng = ~lng,
      lat = ~lat,
      icon = magicIcons("city", "blue", "white", markerSize = 30)
    )
  expect_identical(map$x$calls, eager$x$calls)
})

test_that("Formulas can use functions and variables from their environment", {
  skip_if_not_installed("fontawesome")
  df <- data.frame(lng = 0:1, lat = 0:1, type = c("a", "b"))
  pal <- leaflet::colorFactor(c("red", "blue"), df$type)
  iconPal <- iconFactor(c("car", "city"), df$type)

  map <- leaflet::leaflet(df) |>
    leaflet::addMarkers(
      icon = magicIcons(~ iconPal(type), ~ pal(type)),
      lng = ~lng,
      lat = ~lat
    )
  eager <- leaflet::leaflet(df) |>
    leaflet::addMarkers(
      icon = magicIcons(c("car", "city"), c("#FF0000", "#0000FF")),
      lng = ~lng,
      lat = ~lat
    )
  expect_identical(map$x$calls, eager$x$calls)
})

test_that("Two-sided formulas error", {
  expect_error(magicIcons(type ~ col), "one-sided formula")
})

test_that("iconColor = 'auto' picks the higher contrast of black or white", {
  skip_if_not_installed("fontawesome")
  expect_identical(
    contrast_color(c("#000000", "#FFFFFF", "navy", "yellow")),
    c("#FFFFFF", "#000000", "#FFFFFF", "#000000")
  )

  icon_urls <- function(...) {
    vapply(magicIcons(..., shadow = FALSE), function(i) i$iconUrl, "")
  }
  expect_identical(
    icon_urls("car", c("navy", "yellow"), "auto"),
    icon_urls("car", c("navy", "yellow"), c("#FFFFFF", "#000000"))
  )
  # "auto" can be mixed with other colours
  expect_identical(
    icon_urls("car", c("navy", "yellow"), c("auto", "red")),
    icon_urls("car", c("navy", "yellow"), c("#FFFFFF", "red"))
  )
  # with no marker, the icon takes the marker colour
  expect_identical(
    icon_urls("car", c("navy", "yellow"), "auto", marker = c("none", "circle")),
    icon_urls(
      "car",
      c("navy", "yellow"),
      c("navy", "#000000"),
      marker = c("none", "circle")
    )
  )
})

test_that("library = 'text' shows text in markers", {
  icons <- magicIcons(
    c(1, 10, "A&B"),
    "navy",
    "white",
    library = "text",
    shadow = FALSE
  )
  svgs <- vapply(
    icons,
    function(i) paste(readLines(i$iconUrl), collapse = ""),
    ""
  )
  expect_match(svgs[1], ">1</text>", fixed = TRUE)
  expect_match(svgs[2], ">10</text>", fixed = TRUE)
  # text is escaped
  expect_match(svgs[3], ">A&amp;B</text>", fixed = TRUE)
  # text is allowed to overhang the icon area rather than being clipped
  expect_match(svgs[1], 'overflow="visible"', fixed = TRUE)

  # longer text is smaller
  sizes <- as.numeric(sub('.*font-size="([0-9.]+)".*', "\\1", svgs))
  expect_true(sizes[2] <= sizes[1] && sizes[3] < sizes[2])

  # text legends work, but text needs no attribution
  expect_no_error(
    leaflet::leaflet() |>
      addIconLegend(icons = 1:2, labels = c("a", "b"), library = "text")
  )
  expect_error(
    leaflet::leaflet() |> addIconAttribution("text"),
    "no attribution"
  )
})

read_marker <- function(icon) paste(readLines(icon$iconUrl), collapse = "")

test_that("outlineColor and outlineWidth style the marker outline", {
  skip_if_not_installed("fontawesome")
  default <- magicIcons("car", "red", shadow = FALSE)[[1]]
  expect_match(
    read_marker(default),
    'stroke="#CC0000" stroke-width="2"',
    fixed = TRUE
  )

  white <- magicIcons("car", "red", outlineColor = "white", shadow = FALSE)[[1]]
  expect_match(
    read_marker(white),
    'stroke="#FFFFFF" stroke-width="2"',
    fixed = TRUE
  )
  expect_equal(white$iconWidth, default$iconWidth)

  # a 3px outline on a 30px marker is 10 shape units, so the canvas is padded
  # by 4 units (1.2px) on each side and the anchor moves with it
  wide <- magicIcons("car", "red", outlineWidth = 3, shadow = FALSE)[[1]]
  expect_match(read_marker(wide), 'viewBox="-4 -4 108 140"', fixed = TRUE)
  expect_match(read_marker(wide), 'stroke-width="10"', fixed = TRUE)
  expect_equal(wide$iconWidth, default$iconWidth + 2.4)
  expect_equal(wide$iconHeight, default$iconHeight + 2.4)
  expect_equal(wide$iconAnchorX, default$iconAnchorX + 1.2)
  expect_equal(wide$iconAnchorY, default$iconAnchorY + 1.2)

  # outlines can vary by marker, including via formulas
  icons <- magicIcons(
    "car",
    "red",
    outlineColor = c("white", "black"),
    shadow = FALSE
  )
  expect_false(identical(icons[[1]]$iconUrl, icons[[2]]$iconUrl))
  expect_s3_class(
    magicIcons("car", outlineColor = ~col),
    "leafmagic_icons_formula"
  )

  expect_error(magicIcons("car", outlineWidth = "thick"), "number of pixels")
})

test_that("library = 'svg' uses raw SVG markup as the icon", {
  with_viewbox <- '<svg viewBox="0 0 24 24"><circle cx="12" cy="12" r="10" fill="gold"/><circle cx="12" cy="12" r="5"/></svg>'
  no_viewbox <- '<svg xmlns="http://www.w3.org/2000/svg" width="24px" height="16"><rect width="24" height="16"/></svg>'

  svg <- read_marker(magicIcons(
    with_viewbox,
    "navy",
    "white",
    library = "svg",
    shadow = FALSE
  )[[1]])
  # hard-coded colours are kept, and unfilled parts take on iconColor
  expect_match(svg, 'fill="gold"', fixed = TRUE)
  expect_match(svg, 'viewBox="0 0 24 24"[^>]*fill="#FFFFFF"', perl = TRUE)

  # an SVG with only a width and height is given a viewBox so it scales
  svg <- read_marker(magicIcons(
    no_viewbox,
    "navy",
    library = "svg",
    shadow = FALSE
  )[[1]])
  expect_match(svg, 'viewBox="0 0 24 16"', fixed = TRUE)
  expect_no_match(svg, 'width="24px"', fixed = TRUE)

  expect_error(
    magicIcons("car", library = "svg"),
    "must be SVG markup"
  )

  # svg legends work, but svg icons need no attribution
  expect_no_error(
    leaflet::leaflet() |>
      addIconLegend(icons = with_viewbox, labels = "a", library = "svg")
  )
  expect_error(
    leaflet::leaflet() |> addIconAttribution("svg"),
    "no attribution"
  )
})

test_that("NA icons draw the marker shape alone", {
  shape <- marker_shapes$circle
  svg <- marker_svg(shape, NA, "#FF0000", "#FFFFFF", "fontawesome")
  expect_match(svg, "<circle", fixed = TRUE)
  expect_equal(lengths(regmatches(svg, gregexpr("<svg", svg))), 1L)

  icons <- magicIcons(NA, marker = "square", library = "text")
  svg <- paste(readLines(icons[[1]]$iconUrl), collapse = "")
  expect_match(svg, "<rect", fixed = TRUE)
  expect_no_match(svg, "<text", fixed = TRUE)
})

test_that("R colour names that aren't CSS colours are converted", {
  # browsers draw unknown colours (e.g., "grey40") as black
  icons <- magicIcons(
    "1",
    markerColor = "grey40",
    iconColor = "navyblue",
    library = "text",
    shadow = FALSE
  )
  svg <- paste(readLines(icons[[1]]$iconUrl), collapse = "")
  expect_match(svg, 'fill="#666666"', fixed = TRUE)
  expect_match(svg, 'fill="#000080">1</text>', fixed = TRUE)
  expect_no_match(svg, "grey40|navyblue")

  # transparency is kept
  expect_equal(css_color(c("red", "#FF000080")), c("#FF0000", "#FF000080"))
})

test_that("the \"auto\" icon is a question mark in every library", {
  expect_equal(missing_icon("fontawesome"), "question")
  expect_equal(missing_icon("text"), "?")

  for (library in c("fontawesome", "bootstrap", "lucide")) {
    skip_if_not_installed(switch(
      library,
      fontawesome = "fontawesome",
      bootstrap = "bsicons",
      lucide = "lucidr"
    ))
    expect_equal(
      icon_svg("auto", "red", library),
      icon_svg(missing_icon(library), "red", library)
    )
  }
  expect_match(icon_svg("auto", "red", "text"), ">?</text>", fixed = TRUE)
  expect_match(icon_svg("auto", "red", "svg"), ">?</text>", fixed = TRUE)
})
