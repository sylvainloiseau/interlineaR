context("common")

test_that("simplify", {
	df <- data.frame(1:10, rep(NA, 10), c(rep(NA, 9),3) )
	df2 <- data.frame(rep(NA, 10), rep(NA, 10) )
	l <- list(df, df2)
	newl <- simplify_table_list(l)
	expect_equal(length(newl), 2)
	expect_equal(ncol(newl[[2]]), 0)
	expect_equal(ncol(newl[[1]]), 2)
})
