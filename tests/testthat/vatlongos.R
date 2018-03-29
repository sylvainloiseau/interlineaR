context("Vatlongos")

test_that("Vatlongos corpus", {
  path <- system.file("exampleData", "vatlongos.xml", package="interlineaR")
  corpus <- read.emeld(path, vernacular="vtk", analysis="en")
  expect_equal(length(corpus), 4)

  expect_equal(nrow(corpus$texts), 10)
  expect_equal(nrow(corpus$sentences), 468)
  expect_equal(nrow(corpus$words), 5922)
  expect_equal(nrow(corpus$morphems), 6034)
})
