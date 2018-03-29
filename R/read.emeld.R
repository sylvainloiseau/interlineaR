#' Read an EMELD XML document containing an interlinearized corpus.
#'
#' The EMELD XML vocabulary has been proposed for the encoding of interlinear glosses.
#' It is used by the FieldWorks software (SIL FLEX) as an export format.
#' 
#' Please note that if several 'note' fields in the same language are present in one sentence,
#' the first one only will be kept.
#'
#' @param file the path (or url) to a document in ELMED vocabulary
#' @param vernacular.languages a character vector of one or more codes of languages analysed in the document.
#' @param analysis.languages a character vector of one or more codes of languages used for the analyses (in glosses, translations, notes) in the document.
#' @param get.morphems logical should the returned list include a slot for the description of morphems?
#' @param get.words logical should the returned list include a slot for the description of words?
#' @param get.sentences logical should the returned list include a slot for the description of sentences?
#' @param get.texts logical should the returned list include a slot for the description of texts?
#' @param text.fields information to be extracted for the texts (and turned into corresponding column in the data.frame describing texts)
#' @param sentence.fields information to be extracted for the sentences (and turned into corresponding column in the data.frame describing sentences)
#' @param words.vernacular.fields information (in vernacular language(s)) to be extracted for the words (and turned into corresponding columns in the data.frame describing words)
#' @param words.analysis.fields information (in analysis language(s)) to be extracted for the words (and turned into corresponding columns in the data.frame describing words)
#' @param morphems.vernacular.fields information (in vernacular language(s)) to be extracted for the morphems (and turned into corresponding columns in the data.frame describing morphems). May be null or empty.
#' @param morphems.analysis.fields information (in analysis language(s)) to be extracted for the morphems (and turned into corresponding columns in the data.frame describing morphems). May be null or empty.
#'
#' @return a list with slots named "morphems", "words", "sentences", "texts" 
#' (some slot may have been excluded throuth the "get.*" arguments, see above).
#' Each slot is a data.frame containing the information on the corresponding unit.
#' In each data.frame, each row describe an occurrence (the first row of the result$morphems data.frame describe the first morphem of the corpus).
#' In each data.frame, the first columns give ids refering to the line in other data.frame (so that we can link the first morphem to the text, the sentence or the word it belongs to).
#' The following columns give information about the corresponding occurrence of the unit. Which information are extracted from the document and included in the data frame depends upton the *.fields parameters (see above).
#' Columns made are coined using the field name and the language code. For instance, if read.emeld is called with the parameters vernacular.languages="tww" and morphems.vernacular.fields=c("txt", "cf"), then the column txt.tww and cf.tww will be created in the morphems slot data frame.
#' 
#' @export
#' 
#' @references Baden Hughes, Steven Bird and Catherine Bow \emph{Encoding and Presenting Interlinear Text Using XML Technologies}, http://www.aclweb.org/anthology/U03-1008
#' @references SIL FieldWorks: https://software.sil.org/fieldworks/
#'
#' @examples
#' path <- system.file("exampleData", "tuwariInterlinear.xml", package="interlineaR")
#' corpus <- read.emeld(path, vernacular="tww", analysis="en")
#' head(corpus$morphems)
#' 
#' # In some cases, one may have to combine information coming from various data.frame.
#' # Lets imagine one needs to have in the same data.frame the morphems data 
#' # plus the "note" field attached to sentences:
#' # - The easy way is to combine all the columns of the two data frame 'morphems' and 'sentence' :
#' combined <- merge(corpus$morphems, corpus$sentences, by.x="sentence_id", by.y="sentence_id")
#' head(combined)
#' 
#' # - Alternatively, one may use vector extraction in order to add only the desired column
#' # to the morphems data frame:
#' corpus$morphems$note = corpus$sentences$note.en[ corpus$morphems$sentence_id ]
#' head(corpus$morphems)
read.emeld <- function(file,
                        vernacular.languages,
                        analysis.languages="en", 
                        get.morphems=TRUE, get.words=TRUE, get.sentences=TRUE, get.texts=TRUE,
                        text.fields=c("title", "title-abbreviation", "source", "comment"),
                        sentence.fields=c("segnum", "gls", "lit", "note"),
                        words.vernacular.fields="txt",
                        words.analysis.fields=c("gls", "pos"),
                        morphems.vernacular.fields=c("txt", "cf"),
                        morphems.analysis.fields=c("gls", "msa", "hn")
                        ) {

  corpusdoc <- read_xml(file);

  available.vernacular.languages <- get.languages(corpusdoc, type="vernacular");
  if (is.null(vernacular.languages) || is.na(vernacular.languages)) {
    stop(paste0("No vernacular language argument. The following vernacular languages are found in the document: ", paste(available.vernacular.languages, collapse=", "), "."));
  } else {
    non.available <- vernacular.languages[! vernacular.languages %in% available.vernacular.languages]
    if (length(non.available) > 0) {
      stop(paste0("The following vernacular languages are not available in the document: ", paste(non.available, collapse=" ")));
    }
  }
  
  available.analysis.languages <- get.languages(corpusdoc, type="analysis");
  if (is.null(analysis.languages) || is.na(analysis.languages)) {
    stop(paste0("No analysis language argument. The following analysis languages are found in the document: ", paste(available.analysis.languages, collapse=", "), "."));
  } else {
    non.available <- analysis.languages[! analysis.languages %in% available.analysis.languages]
    if (length(non.available) > 0) {
      stop(paste0("The following analysis languages are not available in the document: ", paste(non.available, collapse=" ")));
    }
  }

  interlinearized <- list();

  texts <- xml_find_all(corpusdoc, "/document/interlinear-text")
  if (get.texts) {
    textsdf <- data.frame(
      text_id = 1:length(texts),
      stringsAsFactors=FALSE
      );
    if (! is.null(text.fields) & length(text.fields) > 0) {
      items <- items.in.element(texts, text.fields, analysis.languages)
      textsdf <- data.frame(textsdf,
                            items,
                            stringsAsFactors = FALSE);
    }
    interlinearized$texts <- textsdf;
  }

  #paragraphs <- xml_find_all(corpusdoc, "/document/interlinear-text/paragraphs/paragraph")

  sentences <- xml_find_all(corpusdoc, "/document/interlinear-text/paragraphs/paragraph/phrases/word")
  if (get.sentences) {
    sentence.by.texts <- xml_find_num(texts, "count(./paragraphs/paragraph/phrases/word)")
    sentencesdf <- data.frame(
      text_id     = rep(1:length(texts), times=sentence.by.texts),
      sentence_id = 1:length(sentences),
      stringsAsFactors=FALSE
    );
    if (! is.null(sentence.fields) & length(sentence.fields) > 0) {
      items <- items.in.element(sentences, sentence.fields, analysis.languages)
      sentencesdf <- data.frame(
        sentencesdf, items,
        stringsAsFactors = FALSE
      );
    }
    interlinearized$sentences <- sentencesdf;
  }

  words <- xml_find_all(corpusdoc, "/document/interlinear-text/paragraphs/paragraph/phrases/word/words/word")
  if (get.words) {
    word.by.texts     <- xml_find_num(texts,     "count(./paragraphs/paragraph/phrases/word/words/word)");
    word.by.sentences <- xml_find_num(sentences, "count(./words/word)");
    wordsdf <- data.frame(
      text_id     = rep(1:length(texts), times=word.by.texts),
      sentence_id = rep(1:length(sentences), times=word.by.sentences),
      word_id     = 1:length(words),
      stringsAsFactors=FALSE
    );
    if (! is.null(words.vernacular.fields) & length(words.vernacular.fields) > 0) {
      items <- items.in.element(words, words.vernacular.fields, vernacular.languages)
      wordsdf <- data.frame(wordsdf,
                            items,
                            stringsAsFactors = FALSE
                            );
    }
    if (! is.null(words.analysis.fields) & length(words.analysis.fields) > 0) {
      items <- items.in.element(words, words.analysis.fields, analysis.languages)
      wordsdf <- data.frame(wordsdf,
                            items,
                            stringsAsFactors = FALSE);
    }
    interlinearized$words <- wordsdf;
  }

  if (get.morphems) {
    morph.by.texts      <- xml_find_num(texts, "count(./paragraphs/paragraph/phrases/word/words/word/morphemes/morph)");
    #morph.by.paragraphs <- xml_find_num(paragraphs, "count(./phrases/word/words/word/morphemes/morph)");
    morph.by.sentences  <- xml_find_num(sentences, "count(./words/word/morphemes/morph)");
    morphs.by.word      <- xml_find_num(words, "count(./morphemes/morph)");
    morphs              <- xml_find_all(corpusdoc, "/document/interlinear-text/paragraphs/paragraph/phrases/word/words/word/morphemes/morph");
    
    morphemsdf <- data.frame(
      text_id      = rep(1:length(texts), times=morph.by.texts),
      #paragraph_id = rep(1:length(paragraphs), times=morph.by.paragraphs),
      sentence_id  = rep(1:length(sentences), times=morph.by.sentences),
      word_id      = rep(1:length(words), times=morphs.by.word),
      morphem_id   = 1:length(morphs),
      type         = xml_attr(morphs, "type"),
      stringsAsFactors=FALSE
    );

    if (! is.null(morphems.vernacular.fields) & length(morphems.vernacular.fields) > 0) {
      items <- items.in.element(morphs, morphems.vernacular.fields, vernacular.languages)
      morphemsdf <- data.frame(morphemsdf, items, stringsAsFactors = FALSE);
    }

    if (! is.null(morphems.analysis.fields) & length(morphems.analysis.fields) > 0) {
      items <- items.in.element(morphs, morphems.analysis.fields, analysis.languages)
      morphemsdf <- data.frame(morphemsdf, items, stringsAsFactors = FALSE);
    }

    interlinearized$morphems <- morphemsdf; 
  }
  return(interlinearized)
}

#' Search the "item" elements in the nodes of an unit
#'
#' In EMELD vocabulary, most of the information is in "item" elements child of text,
#' word (=sentence), word or morphs elements.
#' These items have two attribute: @type for indicating the field
#' (part of speech, gloss, free translation...)
#' and @lang for indicating the language (either under scrutiny or used in the analysis).
#'
#' @param elements a set of node of the same unit (morphems, sentences...)
#' @param fields a character vector of field names.
#' @param languages a character vector of language names
#'
#' @return a data frame with as many columns as fields.
items.in.element <-function(elements, fields, languages) {
  itemsl <- vector(mode="list", length=length(fields) * length(languages))
  names(itemsl) <- paste(fields, rep(languages, each=length(fields)), sep="-")
  for (field in fields) {
    for (language in languages) {
      itemsl[[paste(field, language, sep="-")]] = xml_text(xml_find_first(elements, paste0("(./item[@type='", field,"' and @lang='", language, "'] | text())")));
    }
  }
  return(as.data.frame(itemsl, stringsAsFactors=FALSE))
}

#' Extract the vernacular of analysis language of a EMELD document
#'
#' @param corpusdoc the XML document (xml2 library)
#' @param type length-1 character vector, "vernacular" or "analysis"
#'
#' @return a character vector of language code.
#'
get.languages <- function(corpusdoc, type) {
  languages <- "";
  if (type=="vernacular") {
    languages <- unique(xml_text(xml_find_all(corpusdoc, "/document/interlinear-text/languages/language[@vernacular='true']/@lang")))
  } else if (type=="analysis") {
    languages <- unique(xml_text(xml_find_all(corpusdoc, "/document/interlinear-text/languages/language[not(@vernacular='true')]/@lang")))
  } else {
    stop("Unknown type of language")
  }
  return(sort(languages))
}
