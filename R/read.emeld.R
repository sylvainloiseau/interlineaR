#' Read an EMELD XML document containing an interlinearized corpus.
#'
#' The EMELD XML vocabulary has been proposed for the encoding of interlinear glosses.
#' It is used by the FieldWorks software (SIL FLEX) as an export format.
#' 
#' If several 'note' fields in the same language are present in a sentence,
#' they will be concatenated (see the "sep" argument)
#'
#' @param file the path (or url) to a document in ELMED vocabulary
#' @param vernacular.languages character vector: one or more codes of languages analysed in the document.
#' @param analysis.languages character vector: one or more codes of languages used for the analyses (in glosses, translations, notes) in the document.
#' @param get.morphemes logical vector: should the returned list include a slot for the description of morphemes?
#' @param get.words logical vector: should the returned list include a slot for the description of words?
#' @param get.sentences logical vector: should the returned list include a slot for the description of sentences?
#' @param get.texts logical vector: should the returned list include a slot for the description of texts?
#' @param text.fields character vector: information to be extracted for the texts
#' (and turned into corresponding column in the data.frame describing texts)
#' The default are:
#' \itemize{
#' \item "title"
#' \item "title-abbreviation"
#' \item "source"
#' \item "comment"
#' }
#' @param sentence.fields character vector: information to be extracted for the sentences
#' (and turned into corresponding column in the data.frame describing sentences)
#' The default are:
#' \itemize{
#' \item "segnum" : an ID of the sentende
#' \item "gls": a translation (possibly in all analysis languages)
#' \item "lit": a litteral translation (possibly in all analysis languages)
#' \item "note": note (possibly in all analysis languages)
#' }
#' @param words.vernacular.fields character vector: information (in vernacular language(s))
#' to be extracted for the words (and turned into corresponding columns in the data.frame describing words)
#' The default are:
#' \itemize{
#' \item "txt" : the original text
#' }
#' @param words.analysis.fields character vector: information (in analysis language(s))
#' to be extracted for the words (and turned into corresponding columns in the data.frame describing words)
#' The default are:
#' \itemize{
#' \item "gls" : a gloss of the word
#' \item "pos" : the part of speech of the word
#' }
#' @param morphemes.vernacular.fields character vector: information (in vernacular language(s)) to be extracted for the morphemes (and turned into corresponding columns in the data.frame describing morphemes). May be null or empty.
#' \itemize{
#' \item "txt" : the text of the morpheme
#' \item "cf" : the canonical form of the morpheme
#' }
#' @param morphemes.analysis.fields character vector: information (in analysis language(s)) to be extracted for the morphemes (and turned into corresponding columns in the data.frame describing morphemes). May be null or empty.
#' \itemize{
#' \item "gls" : the gloss of the morpheme
#' \item "msa" : the part of speech of the morpheme
#' \item "hn" : a number for the identifiation of the morpheme amongst its homophone.
#' }
#' @param sep character vector: the character used to join multiple notes in the same language.
#'
#' @return a list with slots named "morphemes", "words", "sentences", "texts" 
#' (some slot may have been excluded throuth the "get.*" arguments, see above).
#' Each slot is a data.frame containing the information on the corresponding unit.
#' In each data.frame, each row describe an occurrence (the first row of the result$morphemes data.frame describe the first morpheme of the corpus).
#' In each data.frame, the first columns give ids refering to the line in other data.frame (so that we can link the first morpheme to the text, the sentence or the word it belongs to).
#' The following columns give information about the corresponding occurrence of the unit. Which information are extracted from the document and included in the data frame depends upton the *.fields parameters (see above).
#' Columns made are coined using the field name and the language code. For instance, if read.emeld is called with the parameters vernacular.languages="tww" and morphemes.vernacular.fields=c("txt", "cf"), then the column txt.tww and cf.tww will be created in the morphemes slot data frame.
#' 
#' @export
#' 
#' @references Baden Hughes, Steven Bird and Catherine Bow \emph{Encoding and Presenting Interlinear Text Using XML Technologies}, http://www.aclweb.org/anthology/U03-1008
#' @references SIL FieldWorks: https://software.sil.org/fieldworks/
#'
#' @examples
#' path <- system.file("exampleData", "tuwariInterlinear.xml", package="interlineaR")
#' corpus <- read.emeld(path, vernacular="tww", analysis="en")
#' head(corpus$morphemes)
#' 
#' # In some cases, one may have to combine information coming from various data.frame.
#' # Lets imagine one needs to have in the same data.frame the morphemes data 
#' # plus the "note" field attached to sentences:
#' # - The easy way is to combine all the columns of the two data frame 'morphemes' and 'sentence' :
#' combined <- merge(corpus$morphemes, corpus$sentences, by.x="sentence_id", by.y="sentence_id")
#' head(combined)
#' 
#' # - Alternatively, one may use vector extraction in order to add only the desired column
#' # to the morphemes data frame:
#' corpus$morphemes$note = corpus$sentences$note.en[ corpus$morphemes$sentence_id ]
#' head(corpus$morphemes)
read.emeld <- function(file,
                        vernacular.languages,
                        analysis.languages="en", 
                        get.morphemes=TRUE, get.words=TRUE, get.sentences=TRUE, get.texts=TRUE,
                        text.fields=c("title", "title-abbreviation", "source", "comment"),
                        sentence.fields=c("segnum", "gls", "lit", "note"),
                        words.vernacular.fields="txt",
                        words.analysis.fields=c("gls", "pos"),
                        morphemes.vernacular.fields=c("txt", "cf"),
                        morphemes.analysis.fields=c("gls", "msa", "hn"),
											 sep=";"
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

  texts_node <- xml_find_all(corpusdoc, "/document/interlinear-text")
  if (get.texts) {
    textsdf <- data.frame(
      text_id = 1:length(texts_node),
      stringsAsFactors=FALSE
      );
    if (! is.null(text.fields) & length(text.fields) > 0) {
      items <- items.in.element(texts_node, text.fields, analysis.languages)
      textsdf <- data.frame(textsdf,
                            items,
                            stringsAsFactors = FALSE);
    }
    interlinearized$texts <- textsdf;
  }

  #paragraphs <- xml_find_all(corpusdoc, "/document/interlinear-text/paragraphs/paragraph")

  sentences_node <- xml_find_all(corpusdoc, "/document/interlinear-text/paragraphs/paragraph/phrases/word")
  if (get.sentences) {
    sentence.by.texts <- xml_find_num(texts_node, "count(./paragraphs/paragraph/phrases/word)")
    sentencesdf <- data.frame(
      text_id     = rep(1:length(texts_node), times=sentence.by.texts),
      sentence_id = 1:length(sentences_node),
      stringsAsFactors=FALSE
    );
    ## Note receive a special treatment
    if ("note" %in% sentence.fields) {
    	notes <- concatenate.items.in.element(sentences_node, "note", analysis.languages, sep)
    	sentencesdf <- data.frame(
    		sentencesdf, notes,
    		stringsAsFactors = FALSE
    	);
    	sentence.fields <- sentence.fields[sentence.fields != "note"]
    }
    if (! is.null(sentence.fields) & length(sentence.fields) > 0) {
      items <- items.in.element(sentences_node, sentence.fields, analysis.languages)
      sentencesdf <- data.frame(
        sentencesdf, items,
        stringsAsFactors = FALSE
      );
    }
    interlinearized$sentences <- sentencesdf;
  }

  words_nodes <- xml_find_all(corpusdoc, "/document/interlinear-text/paragraphs/paragraph/phrases/word/words/word")
  if (get.words) {
    word.by.texts     <- xml_find_num(texts_node,     "count(./paragraphs/paragraph/phrases/word/words/word)");
    word.by.sentences <- xml_find_num(sentences_node, "count(./words/word)");
    wordsdf <- data.frame(
      text_id     = rep(1:length(texts_node), times=word.by.texts),
      sentence_id = rep(1:length(sentences_node), times=word.by.sentences),
      word_id     = 1:length(words_nodes),
      stringsAsFactors=FALSE
    );
    if (! is.null(words.vernacular.fields) & length(words.vernacular.fields) > 0) {
      items <- items.in.element(words_nodes, words.vernacular.fields, vernacular.languages)
      wordsdf <- data.frame(wordsdf,
                            items,
                            stringsAsFactors = FALSE
                            );
    }
    if (! is.null(words.analysis.fields) & length(words.analysis.fields) > 0) {
      items <- items.in.element(words_nodes, words.analysis.fields, analysis.languages)
      wordsdf <- data.frame(wordsdf,
                            items,
                            stringsAsFactors = FALSE);
    }
    interlinearized$words <- wordsdf;
  }

  if (get.morphemes) {
    morph.by.texts      <- xml_find_num(texts_node, "count(./paragraphs/paragraph/phrases/word/words/word/morphemes/morph)");
    #morph.by.paragraphs <- xml_find_num(paragraphs, "count(./phrases/word/words/word/morphemes/morph)");
    morph.by.sentences  <- xml_find_num(sentences_node, "count(./words/word/morphemes/morph)");
    morphs.by.word      <- xml_find_num(words_nodes, "count(./morphemes/morph)");
    morph_nodes         <- xml_find_all(corpusdoc, "/document/interlinear-text/paragraphs/paragraph/phrases/word/words/word/morphemes/morph");
    
    morphemsdf <- data.frame(
      text_id      = rep(1:length(texts_node), times=morph.by.texts),
      #paragraph_id = rep(1:length(paragraphs), times=morph.by.paragraphs),
      sentence_id  = rep(1:length(sentences_node), times=morph.by.sentences),
      word_id      = rep(1:length(words_nodes), times=morphs.by.word),
      morphem_id   = 1:length(morph_nodes),
      type         = xml_attr(morph_nodes, "type"),
      stringsAsFactors=FALSE
    );

    if (! is.null(morphemes.vernacular.fields) & length(morphemes.vernacular.fields) > 0) {
      items <- items.in.element(morph_nodes, morphemes.vernacular.fields, vernacular.languages)
      morphemsdf <- data.frame(morphemsdf, items, stringsAsFactors = FALSE);
    }

    if (! is.null(morphemes.analysis.fields) & length(morphemes.analysis.fields) > 0) {
      items <- items.in.element(morph_nodes, morphemes.analysis.fields, analysis.languages)
      morphemsdf <- data.frame(morphemsdf, items, stringsAsFactors = FALSE);
    }

    interlinearized$morphemes <- morphemsdf; 
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
#' @param elements a set of node of the same unit (morphemes, sentences...)
#' @param fields a character vector of field names.
#' @param languages a character vector of language names
#'
#' @return a data frame with as many columns as fields * languages.
#' @noRd
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

#' Same as items.in.element, but if multiple instnaces of the same field with the same
#' language are found, they are concatenated (with "; "). Used only for "note" in sentence.
#'
#' @param elements see items.in.element
#' @param fields see items.in.element
#' @param languages see items.in.element
#' @param sep character vector: the separator
#'
#' @return a data.frame. see items.in.element.
#' @noRd
concatenate.items.in.element <- function(elements, fields, languages, sep) {
	stopifnot(length(sep) == 1)
	itemsl <- vector(mode="list", length=length(fields) * length(languages))
	names(itemsl) <- paste(fields, rep(languages, each=length(fields)), sep="-")
	for (field in fields) {
		for (language in languages) {
			xpath_count <- paste0("count(./item[@type='", field, "' and @lang='", language, "'])");
			number_node <- xml_find_num(elements, xpath_count);
			xpath_extract <- paste0("(./item[@type='", field,"' and @lang='", language, "'] | text())");
			values <- xml_text(xml_find_all(elements, xpath_extract));
		  nodes_by_parent_nodes <- rep(1:length(elements), number_node);
			res <- vector("character", length = length(elements))
			values_concatened <- tapply(values,  nodes_by_parent_nodes, paste, collapse = "; ")
			res[as.numeric(names(values_concatened))] <- values_concatened;
			itemsl[[paste(field, language, sep="-")]] = res;
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
#' @noRd
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
