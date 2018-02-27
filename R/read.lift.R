#' Parse a dictionary in XML LIFT (Lexicon Interchange FormaT) format and turn it into a data.frame
#'
#' @param file : a length-one character vector containing the path to a LIFT XML document.
#' @param language.code : the code of the language.
#' @return a data.frame
#' @export
#'
#' @seealso read.emeld
#' @references http://code.google.com/p/lift-standard
#' @examples
#' path <- system.file("exampleData", "tuwariDictionary.lift", package="RFlex")
#' dictionary <- read.lift.old(path, language.code="tww")
read.lift.old <- function(file, language.code="tww") {
  dictionarydoc <- read_xml(file)
  entries <- xml_find_all(dictionarydoc, "/lift/entry")
  dictionary <- data.frame(
    id=xml_text(xml_find_first(entries, "@id")),
    form=xml_text(xml_find_first(entries, paste0("lexical-unit/form[@lang='", language.code, "']/text"))),
    order=xml_text(xml_find_first(entries, "@order")),
    type=xml_text(xml_find_first(entries, "trait[@name='morph-type']/@value")),
    pos1=xml_text(xml_find_first(entries, "sense[1]/grammatical-info/@value")),
    sense1=xml_text(xml_find_first(entries, "sense[1]/gloss[@lang='en']/text")),
    domain1=xml_text(xml_find_first(entries, "sense[1]/trait[@name='semantic-domain-ddp4']/@value")),
    
    # complex_form_type=xml_text(xml_find_first(entries, "relation/trait[@name='complex-form-type']/@value")),
    # component=  sapply(entries, function(x) {
    #   rel <- xml_find_all(x, "relation/@ref");
    #   if(length(rel) > 0) {
    #     return(paste(sapply(rel, function(y) xml_find_all(y, paste0("/lift/entry[@id=', y ,']"))), collapse=" "));
    #   } else {
    #     return("")
    #   }
    # }),
    class=xml_text(xml_find_first(entries, "sense/grammatical-info/trait[@name='Noun-infl-class']/@value")),
    stringsAsFactors = FALSE
    );
  return(dictionary)
}

#' Parse a dictionary in XML LIFT (Lexicon Interchange FormaT) format and turn it into a data.frame
#'
#' @param file : a length-one character vector containing the path to a LIFT XML document.
#' @param language.code : the code of the language.
#'
#' @return a data frame containing a dictionary
#' @export
#'
#' @seealso read.emeld
#' @references http://code.google.com/p/lift-standard
#' @examples
#' path <- system.file("exampleData", "tuwariDictionary.lift", package="interlineaR")
#' dictionary <- read.lift.old(path, language.code="tww")
read.lift <- function(file, language.code="tww") {
  dictionarydoc <- read_xml(file)
  entries <- xml_find_all(dictionarydoc, "/lift/entry")
  dictionary <- data.frame(
    id=xml_text(xml_find_first(entries, "@id")),
    form=xml_text(xml_find_first(entries, paste0("lexical-unit/form[@lang='", language.code, "']/text"))),
    order=xml_text(xml_find_first(entries, "@order")),
    type=xml_text(xml_find_first(entries, "trait[@name='morph-type']/@value")),
    pos1=xml_text(xml_find_first(entries, "sense[1]/grammatical-info/@value")),
    sense1=xml_text(xml_find_first(entries, "sense[1]/gloss[@lang='en']/text")),
    domain1=xml_text(xml_find_first(entries, "sense[1]/trait[@name='semantic-domain-ddp4']/@value")),
    
    # complex_form_type=xml_text(xml_find_first(entries, "relation/trait[@name='complex-form-type']/@value")),
    # component=  sapply(entries, function(x) {
    #   rel <- xml_find_all(x, "relation/@ref");
    #   if(length(rel) > 0) {
    #     return(paste(sapply(rel, function(y) xml_find_all(y, paste0("/lift/entry[@id=', y ,']"))), collapse=" "));
    #   } else {
    #     return("")
    #   }
    # }),
    class=xml_text(xml_find_first(entries, "sense/grammatical-info/trait[@name='Noun-infl-class']/@value")),
    stringsAsFactors = FALSE
  );
  return(dictionary)
}
