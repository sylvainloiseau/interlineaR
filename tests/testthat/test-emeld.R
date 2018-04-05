context("emeld")

test_that("Emeld files: nb slots", {
  path <- system.file("exampleData", "tuwariInterlinear.xml", package="interlineaR")
  corpus <- read.emeld(path, vernacular="tww", analysis="en")
  expect_equal(length(corpus), 4)
  
  corpus <- read.emeld(path, vernacular="tww", analysis="en", get.morphemes=FALSE, get.words=FALSE, get.sentences=FALSE, get.texts=TRUE)
  expect_equal(length(corpus), 1)
})

test_that("Emeld files: slots names", {
  path <- system.file("exampleData", "tuwariInterlinear.xml", package="interlineaR")

  corpus <- read.emeld(path, vernacular="tww", analysis="en")
  expect_equal(names(corpus), c("texts", "sentences", "words", "morphemes"));

  corpus <- read.emeld(path, vernacular="tww", analysis="en", get.morphemes=FALSE, get.words=FALSE, get.sentences=FALSE, get.texts=TRUE)
  expect_equal(names(corpus), c("texts"));
})

test_that("Emeld files: texts slot", {
  path <- system.file("exampleData", "tuwariInterlinear.xml", package="interlineaR")
  
  corpus <- read.emeld(path, vernacular="tww", analysis="en")
  expect_equal(nrow(corpus$texts), 9);
})

#xmlstarlet sel -t -v "count(//morph)" inst/exampleData/tuwariInterlinear.xml
test_that("Emeld files: morphemes slot", {
  path <- system.file("exampleData", "tuwariInterlinear.xml", package="interlineaR")
  
  corpus <- read.emeld(path, vernacular="tww", analysis="en")

  expect_equal(nrow(corpus$morphemes), 212);
  expect_equal(ncol(corpus$morphemes), 10);
})

# xmlstarlet sel -t -v "count(//words/word)" inst/exampleData/tuwariInterlinear.xml
test_that("Emeld files: words slot", {
  path <- system.file("exampleData", "tuwariInterlinear.xml", package="interlineaR")

  corpus <- read.emeld(path, vernacular="tww", analysis="en")

  expect_equal(nrow(corpus$words), 125);
  expect_equal(ncol(corpus$words), 6);
})

test_that("Emeld files: no morphemes.vernacular.fields", {
  path <- system.file("exampleData", "tuwariInterlinear.xml", package="interlineaR")
  corpus <- read.emeld(path, vernacular="tww", analysis="en", morphemes.vernacular.fields=c())
  expect_equal(ncol(corpus$morphemes), 8);
})

test_that("Emeld files: no morphemes.analysis.fields", {
  path <- system.file("exampleData", "tuwariInterlinear.xml", package="interlineaR")
  corpus <- read.emeld(path, vernacular="tww", analysis="en", morphemes.analysis.fields=c())
  expect_equal(ncol(corpus$morphemes), 7);
})

test_that("Emeld files: no morphemes.analysis.fields nor morphemes.vernacular.fields", {
  path <- system.file("exampleData", "tuwariInterlinear.xml", package="interlineaR")
  corpus <- read.emeld(path, vernacular="tww", analysis="en", morphemes.analysis.fields=c(), morphemes.vernacular.fields=c())
  expect_equal(ncol(corpus$morphemes), 5);
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


