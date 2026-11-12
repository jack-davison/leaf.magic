test_that("Constants work", {
  testthat::expect_no_error(
    magicIcons("circle", "blue", "white")
  )
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
    magicIcons(c("circle", "x"), c("red", "blue", "green"), "black")
  )
  testthat::expect_error(
    magicIcons(
      c("circle", "x"),
      c("red", "blue", "green"),
      "black",
      library = "bootstrap"
    )
  )
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
