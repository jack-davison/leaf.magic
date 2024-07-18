test_that("Constants work", {
  testthat::expect_no_error(
    faIcons("circle", "blue", "white")
  )
})

test_that("Vectors work", {
  testthat::expect_no_error(
    faIcons(c("circle", "x"), "blue", "white")
  )
  testthat::expect_no_error(
    faIcons(c("circle", "x"), c("blue", "red"), "white")
  )
  testthat::expect_no_error(
    faIcons(c("circle", "x"), c("blue", "red"), c("white", "black"))
  )
})

test_that("Unequal vectors fail", {
  testthat::expect_error(
    faIcons(c("circle", "x"), c("red", "blue", "green"), "black")
  )
})
