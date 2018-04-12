context("cldf")

test_that("Read", {
	path <- system.file("exampleData/cldf", package="interlineaR")
	l <- read.CLDF(path, "Dictionary-metadata.json")
	expect_equal(length(l), 2)
	expect_equal(nrow(l[[1]]), 6)
	expect_equal(nrow(l[[2]]), 8)
	expect_equal(names(l),  c("EntryTable", "SenseTable"))
})

test_that("Write", {
	path <- system.file("exampleData", "tuwariDictionary.lift", package="interlineaR")
	dictionary <- read.lift(path, vernacular.languages="tww", simplify=TRUE)
	path <- "~/test_interlineaR"
	write_CLDF(dictionary, dir=path)
	
	fs <- list.files(paste0(path, "/CLDF"), full.names=TRUE)
	expect_equal(length(fs), 4)

	res <- file.remove(fs)
	if (!all(res)) {
		stop(paste0("Unable to remove some files: ", paste(fs, collapse=" ")));
	}
	res <- file.remove(paste0(path, "/CLDF"))
	if (!res) {
		stop(paste0("Unable to remove the directory: ", paste0(path, "/CLDF")));
	}
})

