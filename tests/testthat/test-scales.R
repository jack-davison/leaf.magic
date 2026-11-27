# Helper: map leaflet colour output back to the matching icon, so icon scales
# can be compared directly against leaflet's colour scales.
cols <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF")
icns <- c("a", "b", "c", "d", "e")

as_icons <- function(colors, n, na.icon = "auto") {
  out <- icns[match(colors, cols[seq_len(n)])]
  out[is.na(out)] <- na.icon
  out
}

# iconFactor --------------------------------------------------------------

test_that("iconFactor matches colorFactor for character domains", {
  domain <- c("pear", "apple", "fig", "apple")
  x <- c("fig", "pear", "apple", NA)
  cpal <- leaflet::colorFactor(cols[1:3], domain)
  ipal <- iconFactor(icns[1:3], domain)
  expect_equal(ipal(x), as_icons(cpal(x), 3))
  # levels are sorted, as in leaflet
  expect_equal(ipal(c("apple", "fig", "pear")), c("a", "b", "c"))
})

test_that("iconFactor matches colorFactor for ordered, factor and levels", {
  domain <- c("pear", "apple", "fig")

  cpal <- leaflet::colorFactor(cols[1:3], domain, ordered = TRUE)
  ipal <- iconFactor(icns[1:3], domain, ordered = TRUE)
  expect_equal(ipal(domain), as_icons(cpal(domain), 3))
  expect_equal(ipal(domain), c("a", "b", "c"))

  fdomain <- factor(domain, levels = c("fig", "pear", "apple"))
  cpal <- leaflet::colorFactor(cols[1:3], fdomain)
  ipal <- iconFactor(icns[1:3], fdomain)
  expect_equal(ipal(domain), as_icons(cpal(domain), 3))
  expect_equal(ipal(c("fig", "pear", "apple")), c("a", "b", "c"))

  cpal <- leaflet::colorFactor(cols[1:3], levels = c("fig", "apple", "pear"))
  ipal <- iconFactor(icns[1:3], levels = c("fig", "apple", "pear"))
  expect_equal(ipal(domain), as_icons(cpal(domain), 3))
})

test_that("iconFactor matches by label when given a factor", {
  ipal <- iconFactor(c("A-icon", "B-icon"), c("b", "a"))
  expect_equal(ipal(factor(c("a", "b"))), c("A-icon", "B-icon"))
  expect_equal(ipal(factor(c("b", "a"))), c("B-icon", "A-icon"))
})

test_that("iconFactor allows repeated icons", {
  ipal <- iconFactor(c("car", "car", "tree"), c("a", "b", "c"))
  expect_equal(ipal(c("a", "b", "c")), c("car", "car", "tree"))
})

test_that("iconFactor reverse matches colorFactor", {
  domain <- c("x", "y", "z")
  cpal <- leaflet::colorFactor(cols[1:3], domain, reverse = TRUE)
  ipal <- iconFactor(icns[1:3], domain, reverse = TRUE)
  expect_equal(ipal(domain), as_icons(cpal(domain), 3))
  expect_equal(ipal(domain), c("c", "b", "a"))
})

test_that("iconFactor handles NA and out-of-domain values", {
  ipal <- iconFactor(c("a", "b"), c("x", "y"), na.icon = "nope")
  expect_equal(ipal(c(NA, NA)), c("nope", "nope"))
  expect_equal(ipal(character()), character())
  expect_warning(out <- ipal(c("x", "zzz")), "outside the icon scale")
  expect_equal(out, c("a", "nope"))
})

test_that("iconFactor warns when icons and levels differ in length", {
  expect_warning(iconFactor(c("a", "b"), c("x", "y", "z")), "length 2")
})

test_that("iconFactor works with domain = NULL", {
  ipal <- iconFactor(c("a", "b"), NULL)
  expect_equal(ipal(c("y", "x")), c("b", "a"))
})

# iconBin -----------------------------------------------------------------

test_that("iconBin matches colorBin regardless of domain order", {
  domain <- c(5, 1, 10, 7, 3)
  x <- c(1, 5, 10, 2.5, NA)
  cpal <- leaflet::colorBin(cols[1:3], domain, bins = 3, pretty = FALSE)
  ipal <- iconBin(icns[1:3], domain, pretty = FALSE)
  expect_equal(ipal(x), as_icons(cpal(x), 3))
  expect_equal(ipal(c(1, 5, 10)), c("a", "b", "c"))
})

test_that("iconBin defaults match colorBin defaults", {
  domain <- c(0, 7, 13, 22, 30)
  x <- c(0, 5, 10, 15, 25, 30)
  cpal <- leaflet::colorBin(cols[1:3], domain, bins = 3)
  ipal <- iconBin(icns[1:3], domain)
  expect_equal(ipal(x), as_icons(cpal(x), 3))
})

test_that("iconBin warns when pretty bins don't match the number of icons", {
  expect_warning(iconBin(icns[1:3], c(1, 2, 3, 100)), "levels/bins")
})

test_that("iconBin assigns the right icon when a bin is empty", {
  ipal <- iconBin(
    c("lo", "mid", "hi"),
    domain = c(1, 2, 3, 100),
    pretty = FALSE
  )
  expect_equal(ipal(c(1, 50, 100)), c("lo", "mid", "hi"))
})

test_that("iconBin matches colorBin with explicit breaks", {
  breaks <- c(0, 5, 10, 20)
  x <- c(1, 6, 15, 20)
  cpal <- leaflet::colorBin(cols[1:3], domain = 0:20, bins = breaks)
  ipal <- iconBin(icns[1:3], domain = 0:20, bins = breaks)
  expect_equal(ipal(x), as_icons(cpal(x), 3))

  # domain can be omitted when bins are given
  ipal <- iconBin(icns[1:3], bins = breaks)
  cpal <- leaflet::colorBin(cols[1:3], bins = breaks)
  expect_equal(ipal(x), as_icons(cpal(x), 3))
})

test_that("iconBin matches colorBin with pretty, right and reverse", {
  domain <- c(0.3, 4.2, 9.7, 13.1, 19.9)
  x <- c(0.3, 5, 10, 15, 19.9)
  cpal <- leaflet::colorBin(cols[1:4], domain, bins = 4, pretty = TRUE)
  n <- length(attr(cpal, "colorArgs")$bins) - 1
  ipal <- suppressWarnings(iconBin(icns[1:n], domain, bins = 4, pretty = TRUE))
  cpal <- leaflet::colorBin(cols[1:n], domain, bins = 4, pretty = TRUE)
  expect_equal(ipal(x), as_icons(cpal(x), n))

  cpal <- leaflet::colorBin(
    cols[1:3],
    1:9,
    bins = c(1, 3, 6, 9),
    right = TRUE,
    reverse = TRUE
  )
  ipal <- iconBin(
    icns[1:3],
    1:9,
    bins = c(1, 3, 6, 9),
    right = TRUE,
    reverse = TRUE
  )
  expect_equal(ipal(1:9), as_icons(cpal(1:9), 3))
})

test_that("iconBin handles NA and out-of-range values", {
  ipal <- iconBin(c("a", "b"), domain = 0:10)
  expect_equal(ipal(c(NA_real_, NA_real_)), c("auto", "auto"))
  expect_warning(out <- ipal(c(5, 100)), "outside the icon scale")
  expect_equal(out, c("b", "auto"))
})

# iconQuantile ------------------------------------------------------------

test_that("iconQuantile matches colorQuantile regardless of domain order", {
  domain <- c(9, 8, 1, 2, 5, 3, 7)
  cpal <- leaflet::colorQuantile(cols[1:2], domain, n = 2)
  ipal <- iconQuantile(icns[1:2], domain)
  expect_equal(ipal(domain), as_icons(cpal(domain), 2))
  expect_equal(ipal(c(1, 9)), c("a", "b"))
})

test_that("iconQuantile matches colorQuantile with probs and reverse", {
  set.seed(1)
  domain <- stats::rnorm(50)
  probs <- c(0, 0.1, 0.5, 0.9, 1)
  cpal <- leaflet::colorQuantile(
    cols[1:4],
    domain,
    probs = probs,
    reverse = TRUE
  )
  ipal <- iconQuantile(icns[1:4], domain, probs = probs, reverse = TRUE)
  expect_equal(ipal(domain), as_icons(cpal(domain), 4))
})

test_that("iconQuantile mirrors colorQuantile's handling of `right`", {
  # colorQuantile() ignores `right` when domain is supplied; values on a break
  # must still get the icon that pairs with their colour
  domain <- 1:9
  cpal <- leaflet::colorQuantile(cols[1:2], domain, n = 2, right = TRUE)
  ipal <- iconQuantile(icns[1:2], domain, right = TRUE)
  expect_equal(ipal(domain), as_icons(cpal(domain), 2))

  cpal <- leaflet::colorQuantile(cols[1:2], NULL, n = 2, right = TRUE)
  ipal <- iconQuantile(icns[1:2], NULL, right = TRUE)
  expect_equal(ipal(domain), as_icons(cpal(domain), 2))
})

test_that("iconQuantile works with domain = NULL", {
  x <- c(10, 1, 4, 7)
  cpal <- leaflet::colorQuantile(cols[1:2], NULL, n = 2)
  ipal <- iconQuantile(icns[1:2], NULL)
  expect_equal(ipal(x), as_icons(cpal(x), 2))
})

test_that("icon scales default to the \"auto\" missing icon", {
  expect_equal(iconFactor("a", "x")(NA), "auto")
  expect_equal(iconBin("a", 0:1, bins = 1:2)(NA), "auto")
  expect_equal(iconQuantile(c("a", "b"), 0:10)(NA), "auto")
})
