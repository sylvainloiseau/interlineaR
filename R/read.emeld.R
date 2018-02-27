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
#' path <- system.file("exampleData", "tuwariInterlinear.xml", package="interlineaR")
#' corpus <- read.emeld.old(path)
read.emeld.old <- function(file) {
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
  # number of morphems by word
  morphs.by.word <- xml_find_num(words, "count(morphemes/morph)")
  wordsdf <- data.frame(
    word_txt=xml_text(xml_find_first(words, "item[@type='txt']")),
    word_pos=xml_text(xml_find_first(words, "item[@type='pos']")),
    word_gls=xml_text(xml_find_first(words, "item[@type='gls']"))
    # ,
    # word_id=xml_text(xml_find_first(words, "position()"))
  );

  notes <- xml_find_all(corpusdoc, "/document/interlinear-text//phrases/word")
  phrasesdf <- data.frame(
    note=xml_text(xml_find_first(notes, "item[@type='note']"))
    #<item type="gls" lang="en">
    #<item type="gls" lang="tpi" />
    #<item type="segnum" lang="en">1.1</item>
  );
  
  corpus$word_txt = rep(wordsdf$word_txt, times=morphs.by.word)
  corpus$word_pos = rep(wordsdf$word_pos, times=morphs.by.word)
  corpus$word_gloss = rep(wordsdf$word_gls, times=morphs.by.word)
  corpus$word_id = rep(1:length(words), times=morphs.by.word)
  
  texts <- xml_find_all(corpusdoc, "/document/interlinear-text")
  # number of morphems by texts
  morph.by.texts <- xml_find_num(texts, "count(.//morph)")
  corpus$text_id <- rep(1:length(texts), times=morph.by.texts)
  # genre, author, date, title...
  
  paragraphs <- xml_find_all(corpusdoc, "/document/interlinear-text/paragraphs/paragraph")
  # number of morphems by paragraphs
  morph.by.paragraphs <- xml_find_num(paragraphs, "count(.//morph)")
  corpus$paragraph_id <- rep(1:length(paragraphs), times=morph.by.paragraphs)

  phrases <- xml_find_all(corpusdoc, "/document/interlinear-text/paragraphs/paragraph/phrases/word")
  # number of morphems by sentences
  morph.by.phrases <- xml_find_num(phrases, "count(.//morph)")
  corpus$phrases_id <- rep(1:length(phrases), times=morph.by.phrases)
  corpus$note <- rep(phrasesdf$note, times=morph.by.phrases)
  return(corpus)
}

#' Turn an EMELD XML document containing an interlinearized corpus into a list of data.frame.
#'
#' The EMELD XML vocabulary has been proposed for the encoding of interlinear glosses.
#' It is used by the FieldWorks software (SIL FLEX) as an export format.
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
#' @param morphems.vernacular.fields information (in vernacular language(s)) to be extracted for the morphems (and turned into corresponding columns in the data.frame describing morphems)
#' @param morphems.analysis.fields information (in analysis language(s)) to be extracted for the morphems (and turned into corresponding columns in the data.frame describing morphems)
#'
#' @return a named list. The elements are named "morphems", "words", "sentences", "texts" 
#' (some element may have been excluded throuth the "get.*" arguments, see above).
#' Each element is a data.frame containing the information on the corresponding unit.
#' In each data.frame, each row is an occurrence (the first row of the result$morphems data.frame describe the first morphem of the corpus).
#' In each data.frame, the first columns give ids refering to the line in other data.frame (so that we can link the first morphem to the text, the sentence or the word it belongs to).
#' The following columns give information about the actual occurrence. Which information are extracted from the document and included in the data frame depends upton the *.fields parameters (see above).
#' 
#' @export
#' 
#' @references Baden Hughes, Steven Bird and Catherine Bow \emph{Encoding and Presenting Interlinear Text Using XML Technologies}, http://www.aclweb.org/anthology/U03-1008
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
  #get.paragraphs=TRUE

  corpusdoc <- read_xml(file);
  
  if (length(vernacular.languages) == 0 | is.null(vernacular.languages)) {
    vernacular.languages <- unique(xml_text(xml_find_all(corpusdoc, "/document/interlinear-text/languages/language[@vernacular=true]/@lang")))
    warning(paste0("No vernacular language argument. The following vernacular languages are found in the document: ", paste(vernacular.languages, collapse=", "), "."));
  }
  
  if (length(analysis.languages) == 0 | is.null(analysis.languages)) {
    analysis.languages <- unique(xml_text(xml_find_all(corpusdoc, "/document/interlinear-text/languages/language[not(@vernacular=true)]/@lang")))
    warning(paste0("No analysis language argument. The following analysis languages are found in the document: ", paste(analysis.languages, collapse=", "), "."));
  }
  
  interlinearized <- list();
  #  res <- list(texts=textsdf, phrases=phrasesdf, words=wordsdf, morphems=morphemsdf)
  
  texts <- xml_find_all(corpusdoc, "/document/interlinear-text")
  if (get.texts) {
    textsdf <- data.frame(text_id = 1:length(texts));
    items <- items.in.element(texts, text.fields, analysis.languages)
    textsdf <- data.frame(textsdf, items, stringsAsFactors = FALSE);
    
    interlinearized$texts <- textsdf;
  }

  #paragraphs <- xml_find_all(corpusdoc, "/document/interlinear-text/paragraphs/paragraph")

  sentences <- xml_find_all(corpusdoc, "/document/interlinear-text/paragraphs/paragraph/phrases/word")
  if (get.sentences) {
    sentence.by.texts <- xml_find_num(texts, "count(./paragraphs/paragraph/phrases/word)")
    sentencesdf <- data.frame(
      text_id     =rep(1:length(texts), times=sentence.by.texts),
      sentence_id = 1:length(sentences)
    );
    items <- items.in.element(sentences, sentence.fields, analysis.languages)
    sentencesdf <- data.frame(sentencesdf, items, stringsAsFactors = FALSE);

    interlinearized$sentences <- sentencesdf;
  }
  
  words <- xml_find_all(corpusdoc, "/document/interlinear-text/paragraphs/paragraph/phrases/word/words/word")
  if (get.words) {
    word.by.texts     <- xml_find_num(texts,     "count(./paragraphs/paragraph/phrases/word/words/word)");
    word.by.sentences <- xml_find_num(sentences, "count(./words/word)");
    wordsdf <- data.frame(
      text_id     = rep(1:length(texts), times=word.by.texts),
      sentence_id = rep(1:length(sentences), times=word.by.sentences),
      word_id     = 1:length(words)
    );
    items <- items.in.element(words, words.vernacular.fields, vernacular.languages)
    wordsdf <- data.frame(wordsdf, items, stringsAsFactors = FALSE);

    items <- items.in.element(words, words.analysis.fields, analysis.languages)
    wordsdf <- data.frame(wordsdf, items, stringsAsFactors = FALSE);
    
    interlinearized$words <- wordsdf;
  }
  
  if (get.morphems) {
    morph.by.texts      <- xml_find_num(texts, "count(./paragraphs/paragraph/phrases/word/words/word/morphemes/morph)");
    #morph.by.paragraphs <- xml_find_num(paragraphs, "count(./phrases/word/words/word/morphemes/morph)");
    morph.by.sentences  <- xml_find_num(sentences, "count(./words/word/morphemes/morph)");
    morphs.by.word      <- xml_find_num(words, "count(./morphemes/morph)");
    
    morphs              <- xml_find_all(corpusdoc, "/document/interlinear-text/paragraphs/paragraph/phrases/word/words/word/morphemes/morph");
    #print(xml_text(morphs[1:3]))
    morphemsdf <- data.frame(
      text_id      = rep(1:length(texts), times=morph.by.texts),
      #paragraph_id = rep(1:length(paragraphs), times=morph.by.paragraphs),
      sentence_id  = phrases_id <- rep(1:length(sentences), times=morph.by.sentences),
      word_id      = rep(1:length(words), times=morphs.by.word),
      morphem_id   = 1:length(morphs),
      type         = xml_attr(morphs, "type")
    );

    items <- items.in.element(morphs, morphems.vernacular.fields, vernacular.languages)
    morphemsdf <- data.frame(morphemsdf, items, stringsAsFactors = FALSE);

    items <- items.in.element(morphs, morphems.analysis.fields, analysis.languages)
    morphemsdf <- data.frame(morphemsdf, items, stringsAsFactors = FALSE);

    # morphemsdf$word_txt = rep(wordsdf$word_txt, times=morphs.by.word);
    # morphemsdf$word_pos = rep(wordsdf$word_pos, times=morphs.by.word);
    # morphemsdf$word_gloss = rep(wordsdf$word_gls, times=morphs.by.word);
    # morphemsdf$note <- rep(sentencesdf$note, times=morph.by.sentences);
    interlinearized$morphems <- morphemsdf; 
  }
  return(interlinearized)
}

#' Search the "item" elements in the nodes of an unit
#'
#' In EMELD vocabulary, most of the information is in "item" elements child of text, word (=sentence), word or morphs elements. These items have two attribute: type for indicating the field (part of speech, gloss, free translation...) and lang for indicating the language (either under scrutiny or used in the analysis).
#'
#' @param elements a set of node of the same unit (morphems, sentences...)
#' @param fields a character vector of field names.
#' @param languages a character vector of language names
#'
#' @return a data frame with as many columns as fields.
items.in.element <-function(elements, fields, languages) {
  itemsl <- list()
  for (field in fields) {
    for (language in languages) {
      itemsl[[paste(field, language, sep="-")]] = xml_text(xml_find_first(elements, paste0("(./item[@type='", field,"' and @lang='", language, "'] | text())")));
    }
  }
  return(as.data.frame(itemsl))
}
