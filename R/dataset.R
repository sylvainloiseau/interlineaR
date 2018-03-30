#' A corpus of 10 texts of the Vatlongos (vtk) language
#'
#' The corpus is produced with the read.emeld() function. It is a list of 4 slots representing four 
#' units: "texts" "sentences" "words" "morphems". Each slot contains a data frame, and each row
#' in the data.frame describe one occurrences of the corresponding unit.
#' 
#' \itemize{
#'   \item texts : a data frame of 95 units and 5 columns ("text_id", "title.en", "title.abbreviation.en", "source.en", "comment.en")
#'   \item sentenes : a data frame of 3967 units and 6 columns ("text_id", "sentence_id", "segnum.en", "gls.en", "lit.en", "note.en")
#'   \item words : a data frame of 52983 units and 6 columns ("text_id" "sentence_id" "word_id" "txt.tvk" "gls.en" "pos.en")
#'   \item mophems numeric : a data frame of 56354 units and 10 columns ("text_id" "sentence_id" "word_id" "morphem_id" "type" "txt.tvk" "cf.tvk" "gls.en" "msa.en" "hn.en" )
#' }
#'
#' See the vignette vatlongos for Case study based on this corpus.
#'
#' @format A list with 4 slots
#' @references Eleanor Ridge <Eleanor_Ridge@soas.ac.uk> 
"vatlongos"

# #' A corpus of 8 texts of the Kakabe language
# #'
# #' The corpus is produced with the read.toolbox() function. It is a list of 4 slots representing four 
# #' units: "texts" "sentences" "words" "morphems". Each slot contains a data frame, and each row
# #' in the data.frame describe one occurrences of the corresponding unit.
# #' 
# #' \itemize{
# #'   \item texts : a data frame of 8 units and 2 columns
# #'   \item sentenes : a data frame of 552 units and 5 columns
# #'   \item words : a data frame of 7381 units and 7 columns
# #'   \item mophems numeric : a data frame of 8659 units and 9 columns
# #' }
# #'
# #'
# #' @format A list with 4 slots
# #' @references Data by Alexandra Vydrina <alexandra.vydrina@gmail.com>
# "kakabe"
