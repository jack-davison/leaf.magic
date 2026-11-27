test_that("Constants work", {
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
  expect_match(files, "^leafmagic-[0-9.]+-[0-9a-f]+\\.png$")
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
  expect_length(icon_svg("car", "red", "fontawesome"), 1)
  skip_if_not_installed("bsicons")
  expect_length(icon_svg("car-front", "red", "bootstrap"), 1)
  skip_if_not_installed("lucidr")
  expect_length(icon_svg("car", "red", "lucide"), 1)
})

test_that("icons from small-viewBox libraries are rendered at high resolution", {
  skip_if_not_installed("bsicons")
  skip_if_not_installed("lucidr")
  for (lib in c("bootstrap", "lucide")) {
    icon <- if (lib == "bootstrap") "car-front" else "car"
    file <- tempfile(fileext = ".png")
    svg_to_png(icon_svg(icon, "red", lib), file)
    expect_equal(magick::image_info(magick::image_read(file))$height, 512)
  }
})
