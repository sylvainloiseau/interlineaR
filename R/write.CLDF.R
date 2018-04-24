#' Serialize a list of data frames (representing for instance a dictionary
#' or a corpus of interlinear glossed texts) following the CLDF specifications.
#'
#' According to the CLDF specifications, the data frames are turned into a set
#' of CVS files plus an optional metadata file in JSON. See \link{create_CLDF_metadata}
#' for the generation of the metadata. The csv file names as well as the CSV options are specified in the metadata.
#'
#' @param l a list of data frames
#' @param dir length-1 character vector: a path to an existing directory
#' @param cldf_dir length-1 character vector: the name of a directory to be created (into 'dir') that will received the set of files
#' @param meta a list: the metadata (a format that can be serialized with jsonlite::toJSON). You can (see examples):
#' \itemize{
#'   \item use NULL to skip writting any metadata file. The list slot names are used as CSV filenames.
#'   \item ignore this argument. \link{create_CLDF_metadata} will be silently called and a default metadata file will be generated.
#'   \item provide a list containing the metadata. You can call \link{create_CLDF_metadata} and edit the returned list
#'   in order not to create it from scratch.
#' }
#' @importFrom jsonlite toJSON 
#' @importFrom utils write.table
#' @references http://cldf.clld.org https://github.com/cldf/cldf/
#' @rdname write_CLDF
#' @export
#' @seealso create_CLDF_metadata
#' @examples
#' path <- system.file("exampleData", "tuwariDictionary.lift", package="interlineaR")
#' dictionary <- read.lift(path, vernacular.languages="tww", simplify=TRUE)
#' 
#' # No metadata file
#' #write_CLDF(dictionary, meta=NULL)
#' 
#' # with no 'meta' argument, create_CLDF_metadata() is called:
#' #write_CLDF(dictionary)
#' 
#' # Use create_CLDF_metadata to create a skeleton to be edited:
#' meta <- create_CLDF_metadata(dictionary)
#' meta$foo$bar <- "gaz"
#' #write_CLDF(dictionary, meta=meta)
write_CLDF <- function(l, dir=".", cldf_dir="CLDF", meta=create_CLDF_metadata(l), metadata_filenames="metadata.json") {

	urls <- vector(mode="character", length=length(l));
	
	## If there is metadata, we need URLs
	if (!is.null(meta)) {
		urls <- sapply(meta$tables, `[[`, "url")
		if (any(sapply(urls, is.null))) stop("The metadata don't contain URLs")
	}
	
	## Check: output dir
	fullpath <- paste0(dir, "/", cldf_dir);
	res <- dir.create(fullpath)
	if (!res) {
		stop(paste0("Unable to create ", fullpath))
	}
	
	if(!is.null(meta)) {
		serialized <- toJSON(meta, auto_unbox=TRUE, pretty=TRUE)
		cat(serialized, file=paste0(fullpath, "/", metadata_filenames))
	} else {
		## we still need urls
		if (is.null(names(l))) stop("Can't guess filenames: no metadata and no slot names in the outer list.")
		urls <- paste0(names(l), ".csv");
	}
	
	write_CLDF_table_list(l, urls, meta$dialect, fullpath)
}

#' Create the CLDF metadata for a list of data frame representing a CLDF Module
#' (Supported: Dictionary Module)
#' 
#' Metadata are expressed as lists of lists.
#'
#' @param l list: the list of data.frames
#' @param module length-1 character vector: the name of a CLDF Module
#'
#' This function generate the metadata tree according to the following rules:
#'
#' \itemize{
#'   \item if the "module" argument is not NULL, it is used as a CLDF Module short name.
#'   \item if the list or some of the inner tables have an attribute "CLDF_metadata", the content of
#'   this attribute is supposed to be a tree of lists expressing the metadata for this object and it
#'   is aggregated to the returned metadata.
#'   \item if the outer list has CLDF Module name in its "class" attribute,
#'   it will be used for generating the correct Module metadata.
#'   If some of the inner tables have a CLDF Component name in their "class" attributes,
#'   it will also be used for generating the correct metadata.
#'   For instance, a list with the value "Dictionary" in its class attribute, containing tables with
#'   "EntryTable" and "SenseTable" in their class attribues, will result in a metadata file describing
#'   a Dictionary module with its two mandatory components.
#' }
#'
#' @return a tree of list containing the metadata. This tree is ready to be serialized using jsonlite::toJSON.
#' @export
#'
#' @references http://cldf.clld.org https://github.com/cldf/cldf/
#' 
#' @examples 
#' 
#' # Use the class attribute in order for create_CLDF_metadata to generate the correct metadata :
#' path <- system.file("exampleData", "tuwariDictionary.lift", package="interlineaR")
#' dictionary <- read.lift(path, vernacular.languages="tww", simplify=TRUE)
#' class(dictionary) <- append(class(dictionary), "Dictionary")
#' meta <- create_CLDF_metadata(dictionary)
#'
#' # If the list does not contain any class information nor a CLDF_metadata attribute,
#' # the Module is supposed to be "Generic" and the slot names of the list are used as Component name.
#' path <- system.file("exampleData", "tuwariDictionary.lift", package="interlineaR")
#' dictionary <- read.lift(path, vernacular.languages="tww", simplify=TRUE)
#' meta <- create_CLDF_metadata(dictionary)
#'
#' # The returned list can be edited
#' meta <- create_CLDF_metadata(dictionary)
#' meta$tables[[2]]$`dc:conformsTo` <- "http://cldf.clld.org/v1.0/terms.rdf#SenseTable"
#' #write_CLDF(dictionary, meta=meta)
create_CLDF_metadata <- function(l, module=NULL) {
	meta <- list()
	meta$`@context`<- list("http://www.w3.org/ns/csvw", list("@language"= "en"))
	module_name <- "";
	
	CLDF_metadata <- attr(l, "CLDF_metadata");
	## Get the Module and the Module metadata
	if (!is.null(CLDF_metadata)) {
		meta <- CLDF_metadata;
		module_qname <- meta$`dc:conformsTo`
		module_name <- strsplit(module_qname, "#")[[1]][2]
	} else {
		CLDF_classname <- get_CLDF_classname(l);
		if (!is.null(module)) {
			module_name <- module
		} else if (!is.null(CLDF_classname)) {
			module_name <- CLDF_classname;
		}
		meta$`dc:conformsTo` <- paste0("http://cldf.clld.org/v1.0/terms.rdf#", module_name);
		meta$dialect$commentPrefix = list(NULL) ## TODO buggy here: create a sublist.
	}

	## Aggregate pieces of information regarding the various Components
	default_component_names <- names(l);
	meta$tables <- mapply(get_CLDF_Component_metadata, l, default_component_names, SIMPLIFY=FALSE, USE.NAMES=FALSE);

	return(meta)
}

#' Look for the module name in the class attribute.
#' @param l the list of data.frame
#' @noRd
get_CLDF_classname <- function(l) {
	UseMethod("get_CLDF_classname");
}

get_CLDF_classname.Dictionary <- function(l) {
	return("Dictionary")
}

get_CLDF_classname.IGTCorpus <- function(l) {
	return("Generic")
}

get_CLDF_classname.default <- function(l) {
	return(NULL)
}

#' Try to extract the medata of a table from an attribute.
#' If no metadata are present, ask "get_CLDF_Component_name".
#' @param table the data.frame
#' @param tablename a slot name in the outer list to be used in last resort
#' @noRd
get_CLDF_Component_metadata <- function(table, tablename) {
	meta <- attr(table, "CLDF_metadata")
	if (is.null(meta)) {
		component_name <- get_CLDF_Component_name(table, tablename)
		if (is.null(component_name)) {
			meta$url=paste0(tablename, ".csv");
			meta$`dc:conformsTo` <- paste0(tablename);
		} else {
			meta <- get_component_default_metadata(component_name)
		}
	}
	return(meta);
}

#' Look for a class name,  in last resort use the R name.
#' @param table the data.frame
#' @param tablename a slot name in the outer list to be used in last resort
#' @noRd
get_CLDF_Component_name <- function(table, tablename) {
	meta <- list()
	UseMethod("get_CLDF_Component_name")
}

get_CLDF_Component_name.EntryTable <- function(table, tablename) {
	return("EntryTable")
}

get_CLDF_Component_name.SenseTable <- function(table, tablename) {
	return("SenseTable")
}

get_CLDF_Component_name.ExampleTable <- function(table, tablename) {
	return("ExampleTable")
}

get_CLDF_Component_name.default <-function(table, tablename) {
	return(NULL)
}

write_CLDF_table_list <- function(l, urls, dialect, fullpath) {
	stopifnot(length(l) == length(urls));

	## TODO the following three lines are copied from read.CLDF. To be reused.
	encoding <- ifelse(!is.null(dialect$encoding), dialect$encoding, "UTF-8")
	header <- ifelse(!is.null(dialect$header), dialect$header, TRUE)
	delimiter <- ifelse(!is.null(dialect$delimiter), dialect$delimiter, ",")
	
	for (i in 1:length(l)) {
		table <- l[[i]];
		url <- urls[i];
		
		write.table(table, paste0(fullpath, "/", url), 
												col.names = header, sep = delimiter, quote = TRUE,  # TODO ?
												fileEncoding = encoding)
	}	
}

get_component_default_metadata <- function(component_name) {
	path <- system.file("cldf/Components/", paste0(component_name, ".json"), package="interlineaR")
	lines <- readLines(path)
	specif <- fromJSON(lines, simplifyVector = FALSE)
	return(specif);
}
