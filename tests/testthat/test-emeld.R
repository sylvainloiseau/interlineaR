context("emeld")

test_that("Emeld files: nb slots", {
  path <- system.file("exampleData", "tuwariInterlinear.xml", package="interlineaR")
  corpus <- read.emeld(path, vernacular="tww", analysis="en")
  expect_equal(length(corpus), 4)
  
  corpus <- read.emeld(path, vernacular="tww", analysis="en", get.morphems=FALSE, get.words=FALSE, get.sentences=FALSE, get.texts=TRUE)
  expect_equal(length(corpus), 1)
})

test_that("Emeld files: slots names", {
  path <- system.file("exampleData", "tuwariInterlinear.xml", package="interlineaR")

  corpus <- read.emeld(path, vernacular="tww", analysis="en")
  expect_equal(names(corpus), c("texts", "sentences", "words", "morphems"));

  corpus <- read.emeld(path, vernacular="tww", analysis="en", get.morphems=FALSE, get.words=FALSE, get.sentences=FALSE, get.texts=TRUE)
  expect_equal(names(corpus), c("texts"));
})

test_that("Emeld files: texts slot", {
  path <- system.file("exampleData", "tuwariInterlinear.xml", package="interlineaR")
  
  corpus <- read.emeld(path, vernacular="tww", analysis="en")
  expect_equal(nrow(corpus$texts), 9);
})

#xmlstarlet sel -t -v "count(//morph)" inst/exampleData/tuwariInterlinear.xml
test_that("Emeld files: morphems slot", {
  path <- system.file("exampleData", "tuwariInterlinear.xml", package="interlineaR")
  
  corpus <- read.emeld(path, vernacular="tww", analysis="en")

  expect_equal(nrow(corpus$morphems), 212);
  expect_equal(ncol(corpus$morphems), 10);
})

# xmlstarlet sel -t -v "count(//words/word)" inst/exampleData/tuwariInterlinear.xml
test_that("Emeld files: words slot", {
  path <- system.file("exampleData", "tuwariInterlinear.xml", package="interlineaR")

  corpus <- read.emeld(path, vernacular="tww", analysis="en")

  expect_equal(nrow(corpus$words), 125);
  expect_equal(ncol(corpus$words), 6);
})

test_that("Emeld files: no morphems.vernacular.fields", {
  path <- system.file("exampleData", "tuwariInterlinear.xml", package="interlineaR")
  corpus <- read.emeld(path, vernacular="tww", analysis="en", morphems.vernacular.fields=c())
  expect_equal(ncol(corpus$morphems), 8);
})

test_that("Emeld files: no morphems.analysis.fields", {
  path <- system.file("exampleData", "tuwariInterlinear.xml", package="interlineaR")
  corpus <- read.emeld(path, vernacular="tww", analysis="en", morphems.analysis.fields=c())
  expect_equal(ncol(corpus$morphems), 7);
})

test_that("Emeld files: no morphems.analysis.fields nor morphems.vernacular.fields", {
  path <- system.file("exampleData", "tuwariInterlinear.xml", package="interlineaR")
  corpus <- read.emeld(path, vernacular="tww", analysis="en", morphems.analysis.fields=c(), morphems.vernacular.fields=c())
  expect_equal(ncol(corpus$morphems), 5);
})

test_that("Emeld files: vernacular languages argument", {
  path <- system.file("exampleData", "tuwariInterlinear.xml", package="interlineaR")
  expect_error(read.emeld(path, analysis="en"));
  expect_error(read.emeld(path, vernacular=NULL, analysis="en"));
  expect_error(read.emeld(path, vernacular="foobar", analysis="en"));
})

test_that("Emeld files: analysis languages argument", {
  path <- system.file("exampleData", "tuwariInterlinear.xml", package="interlineaR")
  expect_error(read.emeld(path, vernacular="tww", analysis=NULL));
  expect_error(read.emeld(path, vernacular="tww", analysis="foobar"));
})

test_that("Emeld files: get languages function", {
  path <- system.file("exampleData", "tuwariInterlinear.xml", package="interlineaR")
  corpusdoc <- read_xml(path);
  expect_equal(get.languages(corpusdoc, type="vernacular"), sort(c("tpi", "tww")));
  expect_equal(get.languages(corpusdoc, type="analysis"), sort(c("en")));
})


