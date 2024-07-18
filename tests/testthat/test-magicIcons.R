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
    magicIcons(c("circle", "x"), c("blue", "red"), "white", library = "bootstrap")
  )
  testthat::expect_no_error(
    magicIcons(c("circle", "x"), c("blue", "red"), c("white", "black"), library = "bootstrap")
  )
})

test_that("Unequal vectors fail", {
  testthat::expect_error(
    magicIcons(c("circle", "x"), c("red", "blue", "green"), "black")
  )
  testthat::expect_error(
    magicIcons(c("circle", "x"), c("red", "blue", "green"), "black", library = "bootstrap")
  )
})
