#' Serialize a list of data frame (representing for instance a dictionary
#' or a corpus of interlinear glossed texts) following the CLDF specifications.
#'
#' @param l a list of data frame
#' @param dir length-1 character vector: the directory to use
#' @param newdir length-1 character vector: a directory to be created (into dir) containing the created files
#' @param meta a tree of list expressing the metadata and ready to be serialized with jsonlite::toJSON. See \link{create_CLDF_metadata} for easily generating metadata. Can be NULL to skip writting a metadata file. 
#' \link{create_CLDF_metadata} can be used to generate a skeleton of default values to be further refined and passed to the meta parameter.
#' @importFrom jsonlite toJSON 
#' @importFrom utils write.table
#' @references http://cldf.clld.org https://github.com/cldf/cldf/
#' @rdname write_CLDF
#' @export
#' @examples
#' # serializing with CLDF using the default metadata values (thanks to the class attribute)
#' path <- system.file("exampleData", "tuwariDictionary.lift", package="interlineaR")
#' dictionary <- read.lift(path, vernacular.languages="tww", simplify=TRUE)
#' #write_CLDF(dictionary)
#' 
#' # serializing with overriding the defaults
#' meta <- create_CLDF_metadata(dictionary)
#' meta$tables[[2]]$`dc:conformsTo` <- "http://cldf.clld.org/v1.0/terms.rdf#SenseTable"
#' #write_CLDF(dictionary, meta=meta)
#' 
# metadata=list(
#   tables=list(
#     list(
#       `dc:conformsTo`= "http://cldf.clld.org/v1.0/terms.rdf#EntryTable",
#       tableSchema=list(columns=list(list(name="ID"), list(name="Headword")))
#     ),
#     list(
#       `dc:conformsTo`= "http://cldf.clld.org/v1.0/terms.rdf#SenseTable",
#       tableSchema=list(columns=list(list(name="ID"), list(name="Headword")))
#     )
#   )
# )
write_CLDF <- function(l, dir=".", newdir="CLDF", meta=create_CLDF_metadata(l)) {

	fullpath <- paste0(dir, "/", newdir);
	res <- dir.create(fullpath)
	if (!res) {
		stop(paste0("Unable to create ", fullpath))
	}
	
	if(!is.null(meta)) {
		serialized <- toJSON(meta, auto_unbox=TRUE, pretty=TRUE)
		cat(serialized, file=paste0(fullpath, "/metadata.json"))
		urls <- sapply(meta$tables, `[[`, "url")
	} else {
		# we still need urls
		urls <- paste0(names(l), ".csv");
	}
	
	write_CLDF_table_list(l, urls, meta$dialect, fullpath)
}

#' Create the CLDF metadata for a list of data frame representing a CLDF component
#' (Supported: Dictionary, interlinear glossed text corpus)
#' 
#' Metadata are expressed, in R, as a tree of lists. This tree can be converted to JSON using jsonlite::toJSON. For instance,
#' \verb{
#' list(
#'   format=list(
#'     x="a",
#'     y="b"
#'   ),
#'   version=list(
#'     "1",
#'     "b"
#'   )
#' )
#' }
#' will result in the following JSON fragment:
#' \verb{
#' {
#' 	"format": {
#' 		"x": "a",
#' 		"y": "b"
#' 	},
#' 	"version": [
#' 		"1",
#' 		"b"
#' 		]
#' }
#' }
#'
#' @param l the list of data.frame
#'
#' This function tries to generate the metadata tree according to the following heuristic:
#' \itemize{
#'   \item if the list or some of the inner tables have an attribute "CLDF_metadata", the content of
#'   this attribute is supposed to be a tree of lists expressing the metadata for this object and it
#'   is aggregated to the metadata.
#'   \item if the outer list has CLDF Module name in its "class" attributes,
#'   it will be used for generating the correct Module metadata.
#'   If some of the inner table have a CLDF Component name in their class attribute,
#'   it will also be used for generating the correct metadata.
#'   For instance, a list with the value "Dictionary" in its class attribute, containing tables with
#'   "EntryTable" and "SenseTable" in their class attribues, will result in a metadata file describing
#'   a Dictionary module with its two mandatory components.
#'   \item In last resort, if no information is found about the table, the names of their slot in the outer list will be
#'   used as url and as component name in the metadata file.
#' }
#'
#' @return a tree of list containing the metadata. This tree is ready to be serialized using jsonlite::toJSON.
#' @export
#'
#' @references http://cldf.clld.org https://github.com/cldf/cldf/
create_CLDF_metadata <- function(l) {
	meta <- list()
	meta$`@context`<- list("http://www.w3.org/ns/csvw", list("@language"= "en"))
	
	## Module
	meta <- get_CLDF_Module_metadata(l, meta);
	
	## Components
	### in last resort, we will use the list slot names as Component name.
	default_component_names <- names(l);
	meta$tables <- mapply(get_CLDF_Component_metadata, l, default_component_names, SIMPLIFY=FALSE, USE.NAMES=FALSE);

	return(meta)
}

#' Try to get the Module name, or use "Generic".
#' @param l the list of data.frame
#' @param meta the metadata to populate
#' @noRd
get_CLDF_Module_metadata <- function(l, meta) {
	CLDF_metadata <- attr(l, "CLDF_metadata");
	if (!is.null(CLDF_metadata)) {
		return(CLDF_metadata);
	}
	UseMethod("get_CLDF_Module_metadata");
}

get_CLDF_Module_metadata.Dictionary <- function(l, meta) {
	meta$`dc:conformsTo` <- "http://cldf.clld.org/v1.0/terms.rdf#Dictionary";
	return(meta)
}

get_CLDF_Module_metadata.IGTCorpus <- function(l, meta) {
	meta$`dc:conformsTo` <- "http://cldf.clld.org/v1.0/terms.rdf#Generic";
	return(meta)
}

get_CLDF_Module_metadata.default <- function(l, meta) {
	meta$`dc:conformsTo` <- "http://cldf.clld.org/v1.0/terms.rdf#Generic";
	return(meta)
}

#' Try to extract the medata of a table from an attribute.
#' If no metadata are present, ask "create_CLDF_Component_metadata".
#' @param table the data.frame
#' @param tablename a slot name in the outer list to be used in last resort
#' @noRd
get_CLDF_Component_metadata <- function(table, tablename) {
	meta <- attr(table, "CLDF_metadata")
	if (is.null(meta)) {
		meta <- create_CLDF_Component_metadata(table, tablename)
	}
	meta$tableSchema <- create_tableSchema_CLDF_metadata(table, meta$`dc:conformsTo`)
	return(meta);
}

#' Look for a class name,  in last resort use the R name.
#' @param table the data.frame
#' @param tablename a slot name in the outer list to be used in last resort
#' @noRd
create_CLDF_Component_metadata <- function(table, tablename) {
	meta <- list()
	UseMethod("create_CLDF_Component_metadata")
}

create_CLDF_Component_metadata.EntryTable <- function(table, tablename) {
	meta$url="entries.csv";
	meta$`dc:conformsTo` <- "http://cldf.clld.org/v1.0/terms.rdf#EntryTable"
	return(meta)
}

create_CLDF_Component_metadata.SenseTable <- function(table, tablename) {
	meta$url="senses.csv";
	meta$`dc:conformsTo` <- "http://cldf.clld.org/v1.0/terms.rdf#SenseTable"
	return(meta)
}

create_CLDF_Component_metadata.ExampleTable <- function(table, tablename) {
	meta$url="examples.csv";
	meta$`dc:conformsTo` <- "http://cldf.clld.org/v1.0/terms.rdf#ExampleTable"
	return(meta)
}

create_CLDF_Component_metadata.default <-function(table, tablename) {
	meta$url=paste0(tablename, ".csv");
	meta$`dc:conformsTo` <- paste0(tablename); #"http://cldf.clld.org/v1.0/terms.rdf#", 
	return(meta)
}

create_tableSchema_CLDF_metadata <- function(table, ComponentURL) {
	tableSchema= list(
		columns=lapply(
			colnames(table),
			create_column_CLDF_metadata
		)
	)
	return(tableSchema)
}

create_column_CLDF_metadata <- function(name="ID", required=TRUE, property="id", datatype=list("base"= "string", "format"= "[a-zA-Z0-9_\\-]+")) {
	columnMetadata <- list(
		"name"= name,
		"required"= TRUE,
		"propertyUrl"= paste0("http://cldf.clld.org/v1.0/terms.rdf#", property),
		"datatype"= datatype
	)
	return(columnMetadata);
}

write_CLDF_table_list <- function(l, urls, dialect, fullpath) {
	
	## TODO the following three lines are copied from read.CLDF. To be reused.
	encoding <- getMetaOrDefault(dialect$encoding, "utf-8")
	header <- getMetaOrDefault(dialect$header, TRUE)
	delimiter <- getMetaOrDefault(dialect$delimiter, ",")
	
	for (i in 1:length(l)) {
		table <- l[[i]];
		url <- urls[i];
		
		write.table(table, paste0(fullpath, "/", url), 
												col.names = header, sep = delimiter, quote = TRUE,  # TODO ?
												fileEncoding = encoding)
	}	
}