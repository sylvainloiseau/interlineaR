context("cldf")

removedir <- function(dir) {
	fs <- list.files(dir, full.names=TRUE)
	res <- file.remove(fs)
	if (!all(res)) {
		stop(paste0("Unable to remove some files: ", paste(fs, collapse=" ")));
	}
	res <- file.remove(dir)
	if (!res) {
		stop(paste0("Unable to remove the directory: ", dir));
	}
}

test_that("Read file set with metadata", {
	path <- system.file("exampleData/cldf/Dictionary_1", package="interlineaR")
	l <- read_CLDF(path, "Dictionary-metadata.json")
	expect_equal(length(l), 2)
	expect_equal(nrow(l[[1]]), 6)
	expect_equal(nrow(l[[2]]), 8)
	expect_equal(names(l),  c("EntryTable", "SenseTable"))
})

test_that("Read file set without metadata", {
	path <- system.file("exampleData/cldf/Dictionary_2", package="interlineaR")
	l <- read_CLDF(path, NULL)
	expect_equal(length(l), 2)
	expect_equal(nrow(l[[1]]), 6)
	expect_equal(nrow(l[[2]]), 8)
	expect_equal(names(l),  c("EntryTable", "SenseTable"))
})

test_that("Read file set without metadata and wrong filenames", {
	path <- system.file("exampleData/cldf/Dictionary_4_wrong_filenames", package="interlineaR")
	expect_error(read_CLDF(path, NULL), regexp="No Module specification matching the urls")
})

test_that("Write, guessing the module with table names", {
	path <- system.file("exampleData", "tuwariDictionary.lift", package="interlineaR")
	dictionary <- read.lift(path, vernacular.languages="tww", simplify=TRUE)
	
	# By default, the Dictionary module has only the entries and senses tables
	dictionary[[3]] <- NULL

	path <- "~/test_interlineaR"
	dir <- "CLDF_1"
	write_CLDF(dictionary, dir=path, cldf_dir=dir)
	
	fs <- list.files(paste0(path, "/", dir), full.names=TRUE)
	expect_equal(length(fs), 3)

	removedir(paste0(path, "/", dir))
})

test_that("Write: incomplete metadata (no file names)", {
	path <- system.file("exampleData", "tuwariDictionary.lift", package="interlineaR")
	dictionary <- read.lift(path, vernacular.languages="tww", simplify=TRUE)
	path <- "~/test_interlineaR"
	expect_error(write_CLDF(dictionary, dir=path, meta=list(`dc:conformsTo`= "http://cldf.clld.org/v1.0/terms.rdf#Dictionary")))
})

test_that("Write: no metadata", {
	path <- system.file("exampleData", "tuwariDictionary.lift", package="interlineaR")
	dictionary <- read.lift(path, vernacular.languages="tww", simplify=TRUE)
	path <- "~/test_interlineaR"
	dir <- "CLDF_2"
	write_CLDF(dictionary, dir=path, cldf_dir=dir, meta=NULL)
	
	fs <- list.files(paste0(path, "/", dir), full.names=TRUE)
	expect_equal(length(fs), 3)
	
	removedir(paste0(path, "/", dir))
})

