cols <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF")
icns <- c("a", "b", "c", "d", "e")

legend_items <- function(
  pal,
  values,
  colors = "black",
  labFormat = leaflet::labelFormat(),
  na.label = "NA"
) {
  icon_legend_items(pal, values, colors, labFormat, na.label)
}

# The legend leaflet::addLegend() would draw for a colour scale
leaflet_legend <- function(pal, values, ...) {
  map <- leaflet::addLegend(leaflet::leaflet(), pal = pal, values = values, ...)
  args <- map$x$calls[[1]]$args[[1]]
  colors <- as.character(args$colors)
  labels <- as.character(args$labels)
  if (!is.null(args$na_color) && !args$na_label %in% labels) {
    colors <- c(colors, args$na_color)
    labels <- c(labels, args$na_label)
  }
  list(colors = colors, labels = labels)
}

# Check that icon & colour legends agree, and that icon-colour pairs are
# consistent (i.e., the nth icon is always paired with the nth colour)
expect_legend_parity <- function(ipal, cpal, values, n, ...) {
  ours <- legend_items(ipal, values, colors = cpal, ...)
  theirs <- leaflet_legend(cpal, values, ...)
  expect_equal(ours$labels, theirs$labels)
  expect_equal(ours$colors, theirs$colors)
  is_na <- !ours$colors %in% cols
  expect_equal(
    ours$icons[!is_na],
    icns[match(ours$colors[!is_na], cols[seq_len(n)])]
  )
}

test_that("icon scales carry iconType and iconArgs attributes", {
  expect_equal(attr(iconFactor(icns[1:2], c("x", "y")), "iconType"), "factor")
  expect_equal(attr(iconBin(icns[1:2], 1:10), "iconType"), "bin")
  expect_equal(attr(iconQuantile(icns[1:2], 1:10), "iconType"), "quantile")
  expect_equal(attr(iconQuantile(icns[1:2], NULL), "iconType"), "quantile")
  expect_equal(
    attr(iconBin(icns[1:2], 0:10, pretty = FALSE), "iconArgs")$bins,
    c(0, 5, 10)
  )
})

test_that("factor legends match leaflet::addLegend()", {
  domain <- c("pear", "apple", "fig")
  values <- c("pear", "fig", "pear")

  expect_legend_parity(
    iconFactor(icns[1:3], domain),
    leaflet::colorFactor(cols[1:3], domain),
    values,
    3
  )
  expect_legend_parity(
    iconFactor(icns[1:3], domain, ordered = TRUE),
    leaflet::colorFactor(cols[1:3], domain, ordered = TRUE),
    values,
    3
  )
  expect_legend_parity(
    iconFactor(icns[1:3], levels = c("fig", "pear", "apple")),
    leaflet::colorFactor(cols[1:3], levels = c("fig", "pear", "apple")),
    values,
    3
  )
  expect_legend_parity(
    iconFactor(icns[1:2], NULL, reverse = TRUE),
    leaflet::colorFactor(cols[1:2], NULL, reverse = TRUE),
    values,
    2
  )
  fvalues <- factor(values, levels = c("pear", "fig"))
  expect_legend_parity(
    iconFactor(icns[1:2], fvalues),
    leaflet::colorFactor(cols[1:2], fvalues),
    fvalues,
    2
  )
  # with missing values
  expect_legend_parity(
    iconFactor(icns[1:3], domain),
    leaflet::colorFactor(cols[1:3], domain),
    c(values, NA),
    3
  )
  # labFormat
  expect_legend_parity(
    iconFactor(icns[1:3], domain),
    leaflet::colorFactor(cols[1:3], domain),
    values,
    3,
    labFormat = leaflet::labelFormat(prefix = "<", suffix = ">")
  )
})

test_that("bin legends match leaflet::addLegend()", {
  values <- c(1, 7, 13, 22, 29)

  expect_legend_parity(
    iconBin(icns[1:3], 0:30, bins = 3, pretty = FALSE),
    leaflet::colorBin(cols[1:3], 0:30, bins = 3, pretty = FALSE),
    values,
    3
  )
  # pretty = TRUE turns 4 requested bins into 0, 10, 20, 30
  expect_legend_parity(
    iconBin(icns[1:3], 0:30, bins = 4),
    leaflet::colorBin(cols[1:3], 0:30, bins = 4),
    values,
    3
  )
  expect_legend_parity(
    iconBin(icns[1:3], NULL, bins = c(0, 10, 25, 30), reverse = TRUE),
    leaflet::colorBin(cols[1:3], NULL, bins = c(0, 10, 25, 30), reverse = TRUE),
    c(values, NA),
    3
  )
  expect_legend_parity(
    iconBin(icns[1:3], 0:30, bins = 3, pretty = FALSE),
    leaflet::colorBin(cols[1:3], 0:30, bins = 3, pretty = FALSE),
    values,
    3,
    labFormat = leaflet::labelFormat(suffix = "&deg;C", between = " to ")
  )
})

test_that("quantile legends match leaflet::addLegend()", {
  set.seed(1)
  values <- stats::rexp(50)

  expect_legend_parity(
    iconQuantile(icns[1:4], NULL),
    leaflet::colorQuantile(cols[1:4], NULL),
    values,
    4
  )
  expect_legend_parity(
    iconQuantile(icns[1:3], values, probs = c(0, 0.2, 0.8, 1)),
    leaflet::colorQuantile(cols[1:3], values, probs = c(0, 0.2, 0.8, 1)),
    c(values, NA),
    3
  )
  expect_legend_parity(
    iconQuantile(icns[1:3], NULL, n = 3, reverse = TRUE),
    leaflet::colorQuantile(cols[1:3], NULL, n = 3, reverse = TRUE),
    values,
    3
  )
})

test_that("NA legend items follow leaflet::addLegend() rules", {
  ipal <- iconFactor(icns[1:2], c("x", "y"), na.icon = "nope")
  cpal <- leaflet::colorFactor(cols[1:2], c("x", "y"), na.color = "pink")

  out <- legend_items(ipal, c("x", "y", NA), cpal)
  expect_equal(out$icons, c("a", "b", "nope"))
  expect_equal(out$labels, c("x", "y", "NA"))
  expect_equal(out$colors[3], "pink")

  # no NAs in values -> no NA item
  expect_length(legend_items(ipal, c("x", "y"), cpal)$icons, 2)
  # transparent na.color -> no NA item
  cpal_t <- leaflet::colorFactor(cols[1:2], c("x", "y"), na.color = "#00000000")
  expect_length(legend_items(ipal, c("x", NA), cpal_t)$icons, 1)
  # NA na.icon -> no NA item
  ipal_na <- iconFactor(icns[1:2], c("x", "y"), na.icon = NA)
  expect_length(legend_items(ipal_na, c("x", NA), "black")$icons, 1)
  # na.label already in labels -> no NA item
  expect_length(legend_items(ipal, c("x", NA), na.label = "x")$icons, 1)
  # a single colour is recycled, but the NA item is grey, as in leaflet
  out <- legend_items(ipal, c("x", "y", NA), "red")
  expect_equal(out$colors, c("red", "red", "#808080"))
})

test_that("values can be omitted for scales with a fixed domain", {
  out <- legend_items(iconFactor(icns[1:3], c("b", "c", "a")), NULL)
  expect_equal(out$labels, c("a", "b", "c"))
  expect_equal(out$icons, c("a", "b", "c"))

  out <- legend_items(iconBin(icns[1:2], 0:10, 2, pretty = FALSE), NULL)
  expect_equal(out$labels, c("0 &ndash; 5", "5 &ndash; 10"))
  expect_equal(out$icons, c("a", "b"))

  out <- legend_items(iconQuantile(icns[1:2], 1:10), NULL)
  expect_equal(out$icons, c("a", "b"))
  expect_match(out$labels[1], "0% &ndash; 50%", fixed = TRUE)

  expect_error(legend_items(iconFactor(icns[1:2], NULL), NULL), "`values`")
  expect_error(legend_items(iconQuantile(icns[1:2], NULL), NULL), "`values`")
})

test_that("bin scales with no domain are supported", {
  # leaflet::addLegend() draws an empty legend here, so check the legend
  # matches the map instead
  ipal <- iconBin(icns[1:2], NULL, bins = 2, pretty = FALSE)
  out <- legend_items(ipal, c(0, 10))
  expect_equal(out$labels, c("0 &ndash; 5", "5 &ndash; 10"))
  expect_equal(out$icons, ipal(c(0, 10)))
  expect_error(legend_items(ipal, NULL), "`values` must be provided")
})

test_that("bin legends with no domain match the map when bins are empty", {
  ipal <- iconBin(icns[1:4], NULL, bins = 4, pretty = FALSE)
  cpal <- leaflet::colorBin(cols[1:4], NULL, bins = 4, pretty = FALSE)
  # cuts are 3, 4.75, 6.5, 8.25, 10, so bin 3 is empty
  values <- c(3, 4, 6, 10)
  out <- legend_items(ipal, values, cpal)

  expect_equal(
    out$labels,
    c("3.00 &ndash; 4.75", "4.75 &ndash; 6.50", "8.25 &ndash; 10.00")
  )
  # each item shows the icon and colour that the map uses for that bin
  expect_equal(out$icons, unique(ipal(values)))
  expect_equal(out$colors, unique(cpal(values)))

  # `right` is respected when assigning values to bins
  ipal_r <- iconBin(icns[1:2], NULL, bins = 2, pretty = FALSE, right = TRUE)
  out_r <- legend_items(ipal_r, c(0, 5, 5, 10))
  expect_equal(out_r$icons, unique(ipal_r(c(0, 5, 5, 10))))
})

test_that("addIconLegend accepts pal and values", {
  skip_if_not_installed("fontawesome")
  df <- data.frame(type = c("Pub", "University", "Pub"))
  ipal <- iconFactor(c("beer", "school"), df$type)
  map <- addIconLegend(
    leaflet::leaflet(df),
    pal = ipal,
    values = ~type,
    colors = c("red", "blue")
  )
  html <- map$x$calls[[1]]$args[[1]]
  expect_match(html, "<strong>type</strong>", fixed = TRUE)
  expect_match(html, "&nbsp;Pub", fixed = TRUE)
  expect_match(html, "&nbsp;University", fixed = TRUE)

  # labels can be overridden
  map <- addIconLegend(
    leaflet::leaflet(),
    pal = ipal,
    labels = c("A pub", "A uni")
  )
  expect_match(map$x$calls[[1]]$args[[1]], "&nbsp;A uni", fixed = TRUE)

  expect_error(
    addIconLegend(leaflet::leaflet(), icons = "beer", pal = ipal),
    "either `pal` or `icons`"
  )
  expect_error(
    addIconLegend(leaflet::leaflet(), pal = function(x) x),
    "must be created by"
  )
  expect_error(
    addIconLegend(
      leaflet::leaflet(),
      icons = "beer",
      labels = "Pub",
      colors = leaflet::colorFactor("red", "Pub")
    ),
    "only be a function"
  )
})

test_that("addIconLegend supports opacity and group", {
  skip_if_not_installed("fontawesome")
  map <- addIconLegend(leaflet::leaflet(), "beer", "Pub", opacity = 0.5)
  expect_match(map$x$calls[[1]]$args[[1]], "opacity:0.5", fixed = TRUE)
  map <- addIconLegend(leaflet::leaflet(), "beer", "Pub")
  expect_no_match(map$x$calls[[1]]$args[[1]], "opacity", fixed = TRUE)
  expect_error(
    addIconLegend(leaflet::leaflet(), "beer", "Pub", opacity = 2),
    "`opacity`"
  )

  # no group -> a plain control, as before
  expect_equal(map$x$calls[[1]]$method, "addControl")

  map <- addIconLegend(leaflet::leaflet(), "beer", "Pub", group = "Pubs")
  call <- map$x$calls[[1]]
  expect_equal(call$method, "addIconLegend")
  expect_equal(call$args[[1]]$group, "Pubs")
  expect_match(call$args[[1]]$html, "&nbsp;Pub", fixed = TRUE)
  deps <- vapply(map$dependencies, `[[`, "", "name")
  expect_true("leaf-magic-legend" %in% deps)
  expect_true(file.exists(
    system.file("htmlwidgets", "iconLegend.js", package = "leaf.magic")
  ))
})
