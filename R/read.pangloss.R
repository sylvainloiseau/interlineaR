#' Read a file in the format used in the pangloss collection
#'
#' The pangloss collection (http://lacito.vjf.cnrs.fr/pangloss/index_en.html) is a large collection of interlinearized texts.
#'
#' @param url a length one character vector with the url of the document to be imported
#' @param DOI an unique identifier
#' @param get.texts should the 'texts' data.frame be included in the result ?
#' @param get.sentences should the 'sentences' data.frame be included in the result ?
#' @param get.words should the 'words' data.frame be included in the result ?
#' @param get.morphems should the 'morphems' data.frame be included in the result ?
#'
#' @export
#' @return a list with up to 5 slots corresponding to different units and named "texts", "sentences", "words", "morphems".
#' Each slot contains a data frame where each line describe an occurrence of the corresponding unit.
#' 
#' @references http://lacito.vjf.cnrs.fr/pangloss/index_en.html
#'
#' @examples
#' path <- system.file("exampleData", "FOURMI.xml", package="interlineaR")
#' corpus <- read.pangloss(path)
#' head(corpus$morphems)
read.pangloss <- function(url,
                          DOI = NULL,
                          get.texts = TRUE,
                          get.sentences = TRUE,
                          get.words = TRUE,
                          get.morphems = TRUE)
{
  corpusdoc <- read_xml(url);
  
  interlinearized <- list();
  
  texts <- xml_find_all(corpusdoc, "/TEXT")
  if (get.texts) {
    textsdf <- data.frame(text_id = xml_text(xml_find_first(texts, "@id")),
                          audio = xml_text(xml_find_first(texts, "HEADER/SOURDFILE/@href")));
    interlinearized$texts <- textsdf;
  }
  
  sentences <- xml_find_all(corpusdoc, "/TEXT/S");
  sentence_id <- xml_text(xml_find_first(sentences, "./@id"));
  if (get.sentences) {
    sentence.by.texts <- xml_find_num(texts, "count(./S)");
    
    sentencesdf <- data.frame(
      sentence_id = sentence_id,
      text_id     = rep(1:length(texts), times = sentence.by.texts),
      audio_start = xml_text(xml_find_first(texts, "/TEXT/S/AUDIO/@start")),
      audio_end   = xml_text(xml_find_first(texts, "/TEXT/S/AUDIO/@end")),
      form = xml_text(xml_find_first(texts, "/TEXT/S/FORM")),
      translation = xml_text(xml_find_first(texts, "/TEXT/S/TRANSL"))
    );
    interlinearized$sentences <- sentencesdf;
  }
  
  words <- xml_find_all(corpusdoc, "/TEXT/S/W");
  if (get.words) {
    word.by.texts <- xml_find_num(texts, "count(./S/W)");
    word.by.sentences <- xml_find_num(sentences, "count(./W)");
    wordsdf <- data.frame(
      word_id = 1:length(words),
      text_id     = rep(1:length(texts), times = word.by.texts),
      sentence_id = rep(sentencesdf$sentence_id, times = word.by.sentences)
    );
    interlinearized$words <- wordsdf;
  }

  morphems <- xml_find_all(corpusdoc, "/TEXT/S/W/M");
  if (get.morphems) {
    morph.by.texts <- xml_find_num(texts, "count(./S/W/M)");
    morph.by.sentences <- xml_find_num(sentences, "count(./W/M)");
    morph.by.words <- xml_find_num(words, "count(./M)");
    morphemsdf <- data.frame(
      morphem_id   = 1:length(morphems),
      text_id      = rep(1:length(texts), times = morph.by.texts),
      sentence_id  = rep(sentencesdf$sentence_id, times = morph.by.sentences),
      word_id      = rep(1:length(words), times = morph.by.words),
      token        = xml_text(xml_find_first(morphems, "./FORM")),
      gloss        = xml_text(xml_find_first(morphems, "./TRANSL"))
    );
    interlinearized$morphems <- morphemsdf;
  }
  return(interlinearized);
}
