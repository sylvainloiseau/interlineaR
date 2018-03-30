#' Parse a dictionary in XML LIFT (Lexicon Interchange FormaT) format and turn it into a data.frame
#'
#' @param file : a length-one character vector containing the path to a LIFT XML document.
#' @param vernacular.languages character vector: the code of the language.
#' @param analysis.languages character vector: code of the object language used in  the glosses and analyses.
#' @param get.entry logical length-1 vector: include the entries table in the result?
#' @param get.sense logical length-1 vector: include the senses table in the result?
#' @param get.example logical length-1 vector: include the examples table in the result?
#' @param get.relation logical length-1 vector: include the relations table in the result?
#' @param entry.fields character vector: names of the fields to be included in the entries table
#' @param sense.fields character vector: names of the fields to be included in the senses table 
#' @param example.fields character vector: names of the fields to be included in the examples table
#' @param relation.fields character vector: names of the fields to be included in the relations table
#' 
#' @return a list with up to four slots named "entries", "senses", "examples" and "relations", each slot containing a data.frame
#'
#' @export
#'
#' @seealso read.emeld
#' @references http://code.google.com/p/lift-standard
#' @examples
#' path <- system.file("exampleData", "tuwariDictionary.lift", package="interlineaR")
#' dictionary <- read.lift(path, vernacular.languages="tww")
read.lift <- function(file, vernacular.languages, analysis.languages="en", 
											get.entry=TRUE, get.sense=TRUE, get.example=TRUE, get.relation=TRUE,
											entry.fields=entry.field(), sense.fields=sense.field(), example.fields=example.field(), relation.fields=relation.field()
											) {
	dictionary <- list();
	dictionarydoc <- read_xml(file)
	
  entrie_nodes <- xml_find_all(dictionarydoc, "/lift/entry")
  
  if (get.entry) {
  	entriesdf <- data.frame(
  		id_LIFT=xml_text(xml_find_first(entrie_nodes, "@id")),
  		id=1:length(entrie_nodes),
  		stringsAsFactors = FALSE
  	);

  	entry.spec <- get.entry.spec();
  	entriesdf <- populate.table(entrie_nodes, entriesdf, entry.fields, entry.spec, vernacular.languages, analysis.languages)
  	
  	dictionary$entries <- entriesdf;
  }
  
  sense_nodes <- xml_find_all(dictionarydoc, "/lift/entry/sense")
  if (get.sense) {
  	sense.by.lexems <- xml_find_num(entrie_nodes, "count(sense)")
  	sensedf <- data.frame(
  		id_LIFT=xml_text(xml_find_first(sense_nodes, "@id")),
  		id=1:length(sense_nodes),
  		lexem_id=rep(1:length(entrie_nodes), sense.by.lexems),
  		stringsAsFactors = FALSE
  	);

  	sense.spec <- get.sense.spec();
  	sensedf <- populate.table(sense_nodes, sensedf, sense.fields, sense.spec, vernacular.languages, analysis.languages)
  	
  	dictionary$senses <- sensedf;
  }
  
  if (get.example) {
  	example_nodes <- xml_find_all(dictionarydoc, "/lift/entry/sense/example")
  	if (length(example_nodes) > 0) {
  		example.by.senses <- xml_find_num(sense_nodes, "count(example)")
  		exampledf <- data.frame(
  			id=1:length(example_nodes),
  			sense_id=rep(1:length(sense_nodes), example.by.senses),
  			stringsAsFactors = FALSE
  		);
  		
  		example.spec <- get.example.spec()
  		exampledf <- populate.table(example_nodes, exampledf, example.fields, example.spec, vernacular.languages, analysis.languages)
  	} else {
  		exampledf <- data.frame()
  	}
  	dictionary$example <- exampledf;
  }
  
  return(dictionary)
}

populate.table <- function(entrie_nodes, df, field.names, field.specs, vernacular.languages, analysis.languages) {
	for (r in 1:length(field.names)) {
		field.name <- field.names[r];
		spec <- field.specs[ field.specs[ ,1] == field.name, ]
		if (nrow(spec) == 0) stop(paste0("Unknown field: ", field.name));
		field <- get.fields(entrie_nodes, spec, vernacular.languages, analysis.languages)
		#df <- cbind(entriesdf, fields);
		returned_field_name <- names(field)
		for (i in 1:length(field)) {
			df[[ returned_field_name[i] ]] <- field[[i]];
		}
	}
	return(df)
}

#' Extract values according to the specification of a field in a LIFT dictionary
#' 
#' @param nodes the context node to evaluate the XPath queries with
#' @param spec A specification: contains information in order to build one or serveral XPath queries
#' @param vernacular.languages character vector: the vernacular languages codes used in the dictionary
#' @param analysis.languages character vector: the analysis languages codes used in the dictionary
get.fields <- function(nodes, spec, vernacular.languages, analysis.languages) {
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
				xpath <- paste0("(", xpath, "/", type, "[ @lang='", l, "']/text | text())");
				field_l [[ paste0(field.name, ".", l) ]] <- xml_text(xml_find_first(nodes, xpath));
			}
		} else if (type == "trait") {
			## TODO : case of concat and collapse
			# if (collapse=="TRUE") {
			# 	select the node and iterate on nodes.
			# use xml_find_all
			# } else {
			xpath <- paste0("(", xpath, "/trait[@name=\"", subtype, "\"]/@value | text())");
			field_l [[ field.name ]] <- xml_text(xml_find_first(nodes, xpath));
			# }
		}
	} else if (type == "") {
			## TODO : case of concat and collapse
			# if (collapse=="TRUE" & Concat != "") {
			# 	select the node , iterate on nodes and run xpath...(Concat), collapse the result
			# use xml_find_all
			# } else {
			field_l [[ field.name ]] <- xml_text(xml_find_first(nodes, xpath));
			# }
	} else {
		stop(paste0("Unknown type value: ", type))
	}
	return(field_l)
}