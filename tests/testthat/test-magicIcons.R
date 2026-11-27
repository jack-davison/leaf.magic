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
  icons <- magicIcons("car", "red", "white", dir = dir)
  expect_true(dir.exists(dir))

  files <- list.files(dir)
  expect_length(files, 1)
  expect_match(files, paste0("^leafmagic-", getNamespaceVersion("leaf.magic")))
  expect_equal(icons[[1]]$iconUrl, file.path(dir, files))

  # intermediate logo/pin PNGs are cleaned up
  new_tmp <- setdiff(list.files(tempdir()), before)
  expect_false(any(grepl("^(logo|pin)_", new_tmp)))

  # equivalent colours share a single cached marker
  magicIcons("car", c("#FF0000", "#ff0000", "red"), "WHITE", dir = dir)
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
