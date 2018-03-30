context("toolbox")

test_that("fields", {
  corpuspath <- system.file("exampleData", "kakabe.txt", package="interlineaR")
  lines <- readLines(corpuspath)
  fields <- interlineaR:::line2field(lines);
  fields <- fields[grepl(pattern = "^\\\\", fields)]
  fields <- fields[1:6]

  field.end   <- regexpr("\\\\\\b\\w+\\b", fields, perl = TRUE)
  field.end    <- attr(field.end,"match.length");

  field_name  <- substr(fields, start=2, stop=field.end) # trimws();
  field_value <- substr(fields, start=field.end+2, stop=nchar(fields))# trimws();

  expect_equal(field_name, c("_sh", "id", "genre", "ref", "tag", "tx"))
})

test_that("get.morphems.boundaries", {
  mb <- c("h p *- ux -*", "a");
  breaks <- interlineaR:::get.tokens.boundaries(mb)
  start <- breaks$index_start
  end <- breaks$index_end
  
  # two strings in mb
  expect_equal(length(start), 2)
  expect_equal(length(end), 2)
  
  # the first string contains 5 tokens, the second 1
  expect_equal(lengths(start), c(5, 1))
  expect_equal(lengths(end), c(5, 1))
  
  # the start and end boundaries of the first string
  expect_equal(start[[1]], c(1, 3, 5, 8,  11))
  expect_equal(end[[1]],   c(2, 4, 7, 10, 10000))

  # the start and end boundaries of the second string
  expect_equal(start[[2]], c(1))
  expect_equal(end[[2]],   c(10000))
})

test_that("get.morphems.boundaries : multiple white space", {

  # caveat not to turn white space into tab here !
  mb <- c("h     p", "a");
  breaks <- interlineaR:::get.tokens.boundaries(mb)
  start <- breaks$index_start
  end <- breaks$index_end
  
  # two strings in mb
  expect_equal(length(start), 2)
  expect_equal(length(end), 2)
  
  # the first string contains 2 tokens, the second 1
  expect_equal(lengths(start), c(2, 1))
  expect_equal(lengths(end), c(2, 1))

  # the start and end boundaries of the first string
  expect_equal(start[[1]], c(1, 7))
  expect_equal(end[[1]],   c(6, 10000))

  # the start and end boundaries of the second string
  expect_equal(start[[2]], c(1))
  expect_equal(end[[2]],   c(10000))
})

test_that("read.toolbox: tuwari tiny corpus", {
		corpuspath <- system.file("exampleData", "tuwariToolbox.txt", package="interlineaR")
		corpus <- read.toolbox(corpuspath)
		expect_equal(dim(corpus$texts), c(1, 2))	
		expect_equal(dim(corpus$sentences), c(7, 6))	
		expect_equal(dim(corpus$words), c(33, 5))	
		expect_equal(dim(corpus$morphems), c(59, 7))	
})
