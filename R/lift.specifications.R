#' List of the available pieces of information for each entry (ie column in the entry table)
#'
#' @return a character vector of entries.
#' @name lift.specification
#' @export
all.entry.fields <- function(){ as.character(entry.fields.spec()[,1]) }

#' List of the available pieces of information for each sense (ie column in the sense table)
#'
#' @return a character vector of entries.
#' @rdname lift.specification
#' @export
all.sense.fields <- function(){ as.character(sense.fields.spec()[,1]) }

#' List of the available pieces of information for each example (ie column in the example table)
#'
#' @return a character vector of entries.
#' @rdname lift.specification
#' @export
all.example.fields <- function(){ as.character(example.fields.spec()[,1]) }

#' List of the available pieces of information for each relation (ie column in the relation table)
#'
#' @return a character vector of entries.
#' @rdname lift.specification
#' @export
all.relation.fields <- function(){ as.character(relation.fields.spec()[,1]) }

#' Information about the structure of the LIFT XML format in order to easily
#' generate XPath expression and extract information.
#' 
#' There are four fonctions: one for each table to be built (entries, senses, examples, relations).
#' 
#' Each functions return a table with the following columns:
#' A name for this field
#' "Path": an XPath expression toward an element.
#' "Type" how to retreive the content of this element in some frequent cases:
#' - "form" indicates that the content is in ./form/text; form contains an attribute @lang
#'    with either vernacular languages code(s), or analysis language code(s).
#'    In this case, the Sub-type column state vernacular of analysis accordingly.
#' - "trait" indicate that the content is in a @value attribute;
#'    the trait has a "name" attribute give in the Sub-type column.
#' - "gloss" is similar to "form" above.
#' "Sub-type": in the cases where Type has the values "form" or "gloss", indicats if
#'    @lang is vernacular ou analysis;
#'    in the cases where Type has the value "trait" : the value of @name.
#' "Concat" an XPath expression for building the value with the element using XPath concat()
#' "Collapse": TRUE = element may appears several time and have to be collapsed
#'    in order to build the cell value.
#' @rdname lift-format


# Lexical-unit, context node: ./entry
# Name"                              ,	"Path"                                          ,	"Type" ,	"Sub-type"   ,	"Concat",	"Collapse",	"Note",	"URL produced (sample)"                         ,	"Commentary",

#' @return a data.frame
#'
#' @name lift-format
entry.fields.spec <- function() { return(as.data.frame(matrix(c(
"lexical-unit"                      ,	"./lexical-unit"                                ,	"form" ,	"vernacular" ,	""      ,	""        ,	""    ,	"'entry/lexical-unit/form[@lang=''tww'']/text/'",	"",
"morph-type"                        ,	"."                                             ,	"trait",	"morph-type" ,	""      ,	""        ,	""    ,	"'entry/trait[@name=''morph-type'']/@value'"    ,	"",
"citation-form"                     ,	"citation"                                      ,	"form" ,	"vernacular" ,	""      ,	""        ,	""    ,	""                                              ,	"",
"note-bibliographique"              ,	"./note[@type=\"bibliography\"]"                ,	"form" ,	"analysis"   ,	""      ,	""        ,	""    ,	""                                              ,	"",
"note"                              ,	"./note[not(@type)]"                            ,	"form" ,	"analysis"   ,	""      ,	""        ,	""    ,	""                                              ,	"",
"literal-meaning"                   ,	"./field[@type=\"literal-meaning\"]"            ,	"form" ,	"analysis"   ,	""      ,	""        ,	""    ,	""                                              ,	"",
"note-restriction"                  ,	"./note[@type=\"restriction\"]"                 ,	"form" ,	"analysis"   ,	""      ,	""        ,	""    ,	""                                              ,	"",
"summary-definition"                ,	"./field[@type=\"summary-definition\"]"         ,	"form" ,	"analysis"   ,	""      ,	""        ,	""    ,	""                                              ,	"",
"variant.form"                      ,	"variant"                                       ,	"form" ,	"vernacular" ,	""      ,	""        ,	""    ,	""                                              ,	"",
"variant.environment"               ,	"variant"                                       ,	"trait",	"environment",	""      ,	""        ,	""    ,	""                                              ,	"",
"variant.morph-type"                ,	"variant"                                       ,	"trait",	"morph-type" ,	""      ,	""        ,	""    ,	""                                              ,	"",
"etymology.source"                  ,	"etymology/@source"                             ,	""     ,	""           ,	""      ,	""        ,	""    ,	""                                              ,	"",
"etymology.type"                    ,	"etymology/@type"                               ,	""     ,	""           ,	""      ,	""        ,	""    ,	""                                              ,	"",
"etymology.form"                    ,	"./etymology"                                   ,	"form" ,	"analysis"   ,	""      ,	""        ,	""    ,	""                                              ,	"",
"etymology.form2"                   ,	"./etymology"                                   ,	"form" ,	"vernacular" ,	""      ,	""        ,	""    ,	""                                              ,	"",
"etymology.gloss"                   ,	"./etymology"                                   ,	"gloss",	"analysis"   ,	""      ,	""        ,	""    ,	"etymology/gloss[@lang]/text/"                  ,	"",
"etymology.comment"                 ,	"./etymology/field[@type=\"comment\"]"          ,	"form" ,	"analysis"   ,	""      ,	""        ,	""    ,	""                                              ,	"",
"pronunciation.form"                ,	"./pronunciation"                               ,	"form" ,	"vernacular" ,	""      ,	""        ,	""    ,	""                                              ,	"",
## TODO
# or strictly english?
"pronunciation.cv-pattern"          ,	"./pronunciation[field[@type=\"cv-pattern\"]]"  ,	"form" ,	"analysis"   ,	""      ,	""        ,	""    ,	""                                              ,	"",
## TODO
# or strictly english?
"pronunciation.tone"                ,	"./pronunciation[field[@type=\"tone\"]]"        ,	"form" ,	"analysis"   ,	""      ,	""        ,	""    ,	""                                              ,	"",
"pronunciation.location"            ,	"./pronunciation"                               ,	"trait",	"location"   ,	""      ,	""        ,	""    ,	""                                              ,	""
), ncol=9, byrow=TRUE), stringsAsFactors=FALSE))}

#"sense (under: entry)",	"./sense"
#"Name"                              ,	"Path"                                          ,	"Type" ,	"Sub-type"            ,	"Concat"     ,	"Collapse",	"Note"  ,	"URL produced (sample)"                         ,	"Commentary",

#' @return a data.frame
#'
#' @name lift-format
sense.fields.spec <- function() {return(as.data.frame(matrix(c(
"grammatical-info.value"            ,	"grammatical-info/@value"                       ,	""     ,	""                    ,	""           ,	""        ,	""      ,	""                                              ,	"",
"gloss"                             ,	"gloss"                                         ,	""     ,	"analysis"            ,	""           ,	""        ,	""      ,	"'gloss[@lang=''en'']/text'"                    ,	"",
"definition"                        ,	"definition"                                    ,	"form" ,	"analysis"            ,	""           ,	""        ,	""      ,	""                                              ,	"",
"note-anthropology"                 ,	"./note[@type=\"anthropology\"]"                ,	"form" ,	"analysis"            ,	""           ,	""        ,	""      ,	""                                              ,	"",
"note-bibliography"                 ,	"./note[@type=\"bibliography\"]"                ,	"form" ,	"analysis"            ,	""           ,	""        ,	""      ,	""                                              ,	"",
"note-discourse"                    ,	"./note[@type=\"discourse\"]"                   ,	"form" ,	"analysis"            ,	""           ,	""        ,	""      ,	""                                              ,	"",
"note-encyclopedic"                 ,	"./note[@type=\"encyclopedic\"]"                ,	"form" ,	"analysis"            ,	""           ,	""        ,	""      ,	""                                              ,	"",
"note"                              ,	"./note[not(@type)]"                            ,	"form" ,	"analysis"            ,	""           ,	""        ,	""      ,	""                                              ,	"",
"note-grammar"                      ,	"./note[@type=\"grammar\"]"                     ,	"form" ,	"analysis"            ,	""           ,	""        ,	""      ,	""                                              ,	"",
"note-phonology"                    ,	"./note[@type=\"phonology\"]"                   ,	"form" ,	"analysis"            ,	""           ,	""        ,	""      ,	""                                              ,	"",
"note-restrictions"                 ,	"./note[@type=\"restrictions\"]"                ,	"form" ,	"analysis"            ,	""           ,	""        ,	""      ,	""                                              ,	"",
"scientific-name"                   ,	"./field[@type=\"scientific-name\"]"            ,	"form" ,	"analysis"            ,	""           ,	""        ,	""      ,	""                                              ,	"",
"note-semantics"                    ,	"./note[@type=\"semantics\"]"                   ,	"form" ,	"analysis"            ,	""           ,	""        ,	""      ,	""                                              ,	"",
"note-sociolinguistics"             ,	"./note[@type=\"sociolinguistics\"]"            ,	"form" ,	"analysis"            ,	""           ,	""        ,	""      ,	""                                              ,	"",
"note-source"                       ,	"./note[@type=\"source\"]"                      ,	"form" ,	"analysis"            ,	""           ,	""        ,	""      ,	""                                              ,	"",
"reversal"                          ,	"./reversal"                                    ,	"form" ,	"analysis"            ,	""           ,	""        ,	""      ,	""                                              ,	"attention un attribut @type qui contient la langue",
"sense-type"                        ,	""                                              ,	"trait",	"sense-type"          ,	""           ,	""        ,	""      ,	""                                              ,	"",
"status"                            ,	""                                              ,	"trait",	"status"              ,	""           ,	""        ,	""      ,	""                                              ,	""
# "relation"                          ,	"relation"                                      ,	""     ,	""                    ,	"concat(@type,':'        ,@ref)"   ,	"TRUE",	""      ,	""                                              ,	"",
# "usage-type"                        ,	""                                              ,	"trait",	"usage-type"          ,	""           ,	"TRUE"    ,	""      ,	""                                              ,	"",
# "semantic-domain-ddp4"              ,	""                                              ,	"trait",	"semantic-domain-ddp4",	""           ,	"TRUE"    ,	""      ,	""                                              ,	"",
# "grammatical-info.traits"           ,	"grammatical-info"                              ,	""     ,	""                    ,	"concat(@name,':'        ,@value)" ,	"TRUE",	""      ,	""                                              ,	""
), ncol=9, byrow=TRUE), stringsAsFactors=FALSE))}

#"example (under sense)",	base uri: "./sense/example"
#"Name"                              ,	"Path"                                          ,	"Type" ,	"Sub-type"            ,	"Concat"     ,	"Collapse",	"Note"  ,	"URL produced (sample)"                         ,	"Commentary",

#' @return a data.frame
#'
#' @name lift-format
example.fields.spec <- function() {return(as.data.frame(matrix(c(
"example.source"                    ,	"./@source"                                     ,	""     ,	""                    ,	""           ,	""        ,	""      ,	""                                              ,	"",
"example.form"                      ,	"./"                                            ,	"form" ,	"vernacular"          ,	""           ,	""        ,	""      ,	""                                              ,	"",
"example.translation"               ,	"./translation"                                 ,	"form" ,	"analysis"            ,	""           ,	""        ,	""      ,	""                                              ,	"",
"example.translation.type"          ,	"./translation/@type"                           ,	""     ,	""                    ,	""           ,	""        ,	""      ,	""                                              ,	"",
"example.note"                      ,	"./note[@type=\"reference\"]"                   ,	"form" ,	"analysis"             ,	""           ,	""        ,	""      ,	""                                              ,	""
), ncol=9, byrow=TRUE), stringsAsFactors=FALSE))}
# or english for the example.note?

#"Relation (under entry)",	base uri "./relation"
#"Name"                              ,	"Path"                                          ,	"Type" ,	"Sub-type"            ,	"Concat"     ,	"Collapse",	"Note"  ,	"URL produced (sample)"                         ,	"Commentary",

#' @return a data.frame
#'
#' @name lift-format
relation.fields.spec <- function() {return(as.data.frame(matrix(c(
"type"                              ,	"./@type"                                       ,	""     ,	""                    ,	""           ,	""        ,	""      ,	""                                              ,	"",
"ref"                               ,	"./ref"                                         ,	""     ,	""                    ,	""           ,	""        ,	""      ,	""                                              ,	"",
"order"                             ,	"./@order"                                      ,	""     ,	""                    ,	""           ,	""        ,	""      ,	""                                              ,	"",
"is-primary"                        ,	""                                              ,	"trait",	"is-primary"          ,	""           ,	""        ,	""      ,	""                                              ,	"",
"complex-form-type"                 ,	""                                              ,	"trait",	"complex-form-type"   ,	""           ,	""        ,	""      ,	""                                              ,	"",
"Comment"                           ,	"./field[@type=\"summary\"]"                    ,	"form" ,	"analysis"            ,	""           ,	""        ,	""      ,	""                                              ,	""
), ncol=9, byrow=TRUE), stringsAsFactors=FALSE))}

