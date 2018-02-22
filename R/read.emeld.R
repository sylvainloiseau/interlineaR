#' Create a data frame importing an interlinearised corpus in ELMED XML.
#'
#' The EMELD XML vocabulary has been proposed for the encoding of interlinear glosses.
#' It is used by the FieldWorks software (SIL FLEX) as an export format.
#'
#' @param file the uri of the XML document
#'
#' @return a data frame
#' @export
#' @references Baden Hughes, Steven Bird and Catherine Bow \emph{Encoding and Presenting Interlinear Text Using XML Technologies}, http://www.aclweb.org/anthology/U03-1008
#' @seealso read.lift
#' @examples
#' path <- system.file("exampleData", "tuwariInterlinear.xml", package="RFlex")
#' corpus <- read.emeld(path)
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_attr
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_find_first
#' @importFrom xml2 xml_find_num
#' @importFrom xml2 xml_text

read.emeld <- function(file) {
  corpusdoc <- read_xml(file)
  morphs <- xml_find_all(corpusdoc, "/document/interlinear-text//morphemes/morph")
  corpus <- data.frame(
    txt=xml_text(xml_find_first(morphs, "(./item[@type='txt'] | text())")),
    gloss=xml_text(xml_find_first(morphs, "(./item[@type='gls'] | text())")),
    morph_msa=xml_text(xml_find_first(morphs, "./item[@type='msa']")),
    cf=xml_text(xml_find_first(morphs, "./item[@type='cf']")),
    hn=xml_text(xml_find_first(morphs, "./item[@type='hn']")),
    type=xml_attr(morphs, "type")
  );

  words <- xml_find_all(corpusdoc, "/document/interlinear-text//phrases/word/words/word")
  wordsdf <- data.frame(
    word_txt=xml_text(xml_find_first(words, "item[@type='txt']")),
    word_pos=xml_text(xml_find_first(words, "item[@type='pos']")),
    word_gls=xml_text(xml_find_first(words, "item[@type='gls']"))
    # ,
    # word_id=xml_text(xml_find_first(words, "position()"))
  )
  # number of morphems by word
  morphs.by.word <- xml_find_num(words, "count(morphemes/morph)")
  
  corpus$word_txt = rep(wordsdf$word_txt, times=morphs.by.word)
  corpus$word_pos = rep(wordsdf$word_pos, times=morphs.by.word)
  corpus$word_gloss = rep(wordsdf$word_gls, times=morphs.by.word)
  corpus$word_id = rep(1:length(words), times=morphs.by.word)
  
  # number of morphems by texts
  texts <- xml_find_all(corpusdoc, "/document/interlinear-text")
  morph.by.texts <- xml_find_num(texts, "count(.//morph)")
  corpus$text_id <- rep(1:length(texts), times=morph.by.texts)
  
  # number of morphems by paragraphs
  paragraphs <- xml_find_all(corpusdoc, "/document/interlinear-text/paragraphs/paragraph")
  morph.by.paragraphs <- xml_find_num(paragraphs, "count(.//morph)")
  corpus$paragraph_id <- rep(1:length(paragraphs), times=morph.by.paragraphs)

  # number of morphems by sentences
  phrases <- xml_find_all(corpusdoc, "/document/interlinear-text/paragraphs/paragraph/phrases/word")
  morph.by.phrases <- xml_find_num(phrases, "count(.//morph)")
  corpus$phrases_id <- rep(1:length(phrases), times=morph.by.phrases)
  return(corpus)
}
