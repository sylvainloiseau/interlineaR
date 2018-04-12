#' Remove columns containing only NA in all data frame of a list of data frame.
#'
#' @param l: a list of data frame
#'
#' @return a list of data frame
#'
#' @noRd
simplify_table_list <- function(l) {
	res <- lapply(l,
								function(table) {
									table[!unlist(vapply(table, function(col)
										all(is.na(col)), logical(1)))]
								})
	return(res)
}