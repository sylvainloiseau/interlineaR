#' Parse a dictionary in XML LIFT (Lexicon Interchange FormaT) vocabulary
#' and turn it into a set of data.frame
#'
#' The dictionary is turned into a list of up to four data frame: "entries", "senses", "examples" and "relations".
#' The data frame are pointing to each other through IDs, following a relational data model.
#' 
#' "Field" in this document denote a piece of information in LIFT, such as the "gloss"
#' in a sense or "citation form" of an entry. A field may correspond to several columns in the resulting data frame,
#' since fields are multilingual. "gloss" is an analysis field, thus if two
#' analysis.languages are declared, for instance "en" and "fr", then two columns
#' will be present, gloss.en and gloss.fr, in the senses data frame.
#' The "citation form" field, on the other hand, is an vernacular language field, thus if
#' several vernacular fields are declared, several form columns will be present in the entries data frame.
#'
#' @param file : a length-one character vector containing the path to a LIFT XML document.
#' @param vernacular.languages character vector: the code of the vernacular language.
#' @param analysis.languages character vector: code of the object language used in the glosses and analyses.
#' @param get.entry logical length-1 vector: include the entries table in the result?
#' @param get.sense logical length-1 vector: include the senses table in the result?
#' @param get.example logical length-1 vector: include the examples table in the result?
#' @param get.relation logical length-1 vector: include the relations table in the result?
#' @param entry.fields character vector: names of the fields to be included in the entries table. See available.entry.fields() for the complete list of the available fields.
#' @param sense.fields character vector: names of the fields to be included in the senses table. See available.sense.fields() for the complete list of the available fields.
#' @param example.fields character vector: names of the fields to be included in the examples table. See available.example.fields() for the complete list of the available fields.
#' @param relation.fields character vector: names of the fields to be included in the relations table. See available.relation.fields() for the complete list of the available fields.
#' @param simplify logical length-1 vector: if true, columns containing only empty values are removed from all data frame.
#' @param sep character vector: the character used to join multiple notes in the same language.
#' 
#' @return a list with up to four slots named "entries", "senses", "examples" and "relations",
#' each slot containing a data.frame
#'
#' @export
#'
#' @seealso write.CLDF for serialization
#' @references http://code.google.com/p/lift-standard
#' @examples
#' path <- system.file("exampleData", "tuwariDictionary.lift", package="interlineaR")
#' dictionary <- read.lift(path, vernacular.languages="tww")
#' 
#' # Reduce the size of the data frames by filtering to columns actually containing something...
#' dictionary <- read.lift(path, vernacular.languages="tww", simplify=TRUE)
#' 
#' # Get information in the different analysis languages used in the document (english and tok pisin)
#' dictionary <- read.lift(path, vernacular.languages="tww", analysis.languages=c("en", "tpi"))
#' 
#' # Restrict to entries and senses dataframe, and explicitly ask for some fields:
#' dictionary <- read.lift(
#'   path,
#'   vernacular.languages="tww",
#'   get.example=FALSE,
#'   get.relation=FALSE,
#'   entry.fields=c("lexical-unit", "morph-type"),
#'   sense.fields=c("grammatical-info.value", "gloss", "definition",
#'   "semantic-domain-ddp4", "grammatical-info.traits")
#' )
#' 
read.lift <- function(file, vernacular.languages, analysis.languages="en", 
											get.entry=TRUE, get.sense=TRUE, get.example=TRUE, get.relation=TRUE,
											entry.fields=available.entry.fields(), sense.fields=available.sense.fields(), example.fields=available.example.fields(), relation.fields=available.relation.fields(),
											simplify=FALSE,
											sep=";"
											) {
	dictionary <- list();
	dictionarydoc <- read_xml(file);
	
  entrie_nodes <- xml_find_all(dictionarydoc, "/lift/entry");
  if (get.entry) {
  	entriesdf <- data.frame(
  		id_LIFT=xml_text(xml_find_first(entrie_nodes, "@id")),
  		id=1:length(entrie_nodes),
  		stringsAsFactors = FALSE
  	);

  	entry.spec <- entry.fields.spec();
  	entriesdf <- populate.table(entrie_nodes, entriesdf, entry.fields, entry.spec, vernacular.languages, analysis.languages, sep);
  	
  	dictionary$entries <- entriesdf;
  }
  
  sense_nodes <- xml_find_all(dictionarydoc, "/lift/entry/sense");
  if (get.sense) {
  	sense.by.lexems <- xml_find_num(entrie_nodes, "count(sense)");
  	sensedf <- data.frame(
  		id_LIFT=xml_text(xml_find_first(sense_nodes, "@id")),
  		id=1:length(sense_nodes),
  		lexem_id=rep(1:length(entrie_nodes), sense.by.lexems),
  		stringsAsFactors = FALSE
  	);

  	sense.spec <- sense.fields.spec();
  	sensedf <- populate.table(sense_nodes, sensedf, sense.fields, sense.spec, vernacular.languages, analysis.languages, sep);
  	
  	dictionary$senses <- sensedf;
  }
  
  if (get.example) {
  	example_nodes <- xml_find_all(dictionarydoc, "/lift/entry/sense/example");
  	if (length(example_nodes) > 0) {
  		example.by.senses <- xml_find_num(sense_nodes, "count(example)");
  		example.by.entries <- xml_find_num(entrie_nodes, "count(sense/example)");
  		exampledf <- data.frame(
  			id=1:length(example_nodes),
  			lexem_id=rep(1:length(entrie_nodes), example.by.entries),
  			sense_id=rep(1:length(sense_nodes), example.by.senses),
  			stringsAsFactors = FALSE
  		);
  		
  		example.spec <- example.fields.spec();
  		exampledf <- populate.table(example_nodes, exampledf, example.fields, example.spec, vernacular.languages, analysis.languages, sep);
  	} else {
  		exampledf <- data.frame();
  	}
  	dictionary$examples <- exampledf;
  }
  
  if (simplify) {
  	dictionary <- simplify_table_list(dictionary)
  }
  
  ## TODO get.relations
  return(dictionary)
}

#' Populate a data frame with pieces of information found in a node set according to a list of column to be crated.
#'
#' @param nodes a node set. Each node correspond to a row in the table. Nodes can be lexical entries, or exemples, or senses, or relations...
#' @param table a table containing some information to be augmented with new columns.
#' @param field.names character vector: the name of the field we are interested in. Each element in this vector
#' will result in at least one column (if an element denote an information given in verncular or analysis language,
#' and if several vernacular or analysis languages are declared (see the parameters "vernacular.languages" and
#' "analysis.languages", then this element will result in several column)
#' @param field.specs The specification (how to get information?) for all the available fields. See the page lift-format.
#' @param vernacular.languages character vector: the code of the vernacular languages used in the dictionary
#' @param analysis.languages character vector: the code of the vernacular languages used in the dictionary
#'
#' @return a data frame with the requested columns
#' @noRd
populate.table <- function(nodes, table, field.names, field.specs, vernacular.languages, analysis.languages, sep) {
	for (i in 1:length(field.names)) {
		field.name <- field.names[i];
		spec <- field.specs[ field.specs[ ,1] == field.name, ];
		if (nrow(spec) == 0) stop(paste0("Unknown field: ", field.name));
		field <- get.fields(nodes, spec, vernacular.languages, analysis.languages, sep);
		returned_field_name <- names(field);
		for (i in 1:length(field)) {
			table[[ returned_field_name[i] ]] <- field[[i]];
		}
	}
	return(table);
}

#' Extract values according to the specification of a field in a LIFT dictionary
#' 
#' @param nodes the context node to evaluate the XPath queries with
#' @param spec A specification: contains information in order to build one or serveral XPath queries
#' @param vernacular.languages character vector: the vernacular languages codes used in the dictionary
#' @param analysis.languages character vector: the analysis languages codes used in the dictionary
#' @noRd
get.fields <- function(nodes, spec, vernacular.languages, analysis.languages, sep) {
	field.name <- spec[1, 1]
	xpath <- spec[1, 2]
	type <- spec[1, 3]
	subtype <- spec[1, 4]
	concat <- spec[1, 5]
	collapse <- spec[1, 6]

	field_l <- list();
	
	if (xpath == "") {
		xpath <- ".";
	}
	
	if (type != "") {
		if (type=="form" | type=="gloss") {
			languages <- "";
			if (subtype == "analysis") {
				languages <- analysis.languages
			} else if (subtype =="vernacular") {
				languages <- vernacular.languages
			} else {
				stop(paste0("Unknown subtype: ", subtype))
			}
			for(l in languages) {
				xpath_l <- paste0("(", xpath, "/", type, "[ @lang='", l, "']/text | text())");
				field_l [[ paste0(field.name, ".", l) ]] <- xml_text(xml_find_first(nodes, xpath_l));
			}
		} else if (type == "trait") {
			## case of collapse (no concat here)
			if (collapse=="TRUE") {
				xpath_count <- paste0("count(", xpath, "/trait[@name=\"", subtype, "\"])");
				number_node <- xml_find_num(nodes, xpath_count);
				xpath_extract <- paste0("(", xpath, "/trait[@name=\"", subtype, "\"]/@value | text())");
				values <- xml_text(xml_find_all(nodes, xpath_extract));
				nodes_by_parent_nodes <- rep(1:length(nodes), number_node);
				res <- vector("character", length = length(nodes))
				values_concatened <- tapply(values,  nodes_by_parent_nodes, paste, collapse = sep)
				res[as.numeric(names(values_concatened))] <- values_concatened;
				field_l [[ field.name ]] <- res;
			} else {
				xpath <- paste0("(", xpath, "/trait[@name=\"", subtype, "\"]/@value | text())");
				field_l [[ field.name ]] <- xml_text(xml_find_first(nodes, xpath));
			}
		}
	} else if (type == "") {
			## case of concat and collapse
			if (collapse=="TRUE" & concat != "") {
				xpath_count <- paste0("count(", xpath, ")");
				number_node <- xml_find_num(nodes, xpath_count);
				new_node <- xml_find_all(nodes, xpath);
				values <- as.character(xml_find_first(new_node, concat));
				nodes_by_parent_nodes <- rep(1:length(nodes), number_node);
				res <- vector("character", length = length(nodes))
				values_concatened <- tapply(values,  nodes_by_parent_nodes, paste, collapse = sep)
				res[as.numeric(names(values_concatened))] <- values_concatened;
				field_l [[ field.name ]] <- res;
			} else {
			field_l [[ field.name ]] <- xml_text(xml_find_first(nodes, xpath));
			}
	} else {
		stop(paste0("Unknown type value: ", type))
	}
	return(field_l)
}