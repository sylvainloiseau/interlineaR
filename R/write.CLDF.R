#' Serialize a list of data frames (representing for instance a dictionary
#' or a corpus of interlinear glossed texts) following the CLDF specifications.
#'
#' According to the CLDF specifications, the data frames are turned into a set
#' of CVS files plus an optional metadata file in JSON. See \link{create_CLDF_metadata}
#' for the generation of the metadata.
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
#' @param metadata_filenames length-1 character vector: name of the metadata file to be written (if any).
#' @importFrom jsonlite toJSON
#' @importFrom utils write.table
#' @references http://cldf.clld.org https://github.com/cldf/cldf/
#' @rdname write_CLDF
#' @export
#' @seealso create_CLDF_metadata
#' @examples
#' path <- system.file("exampleData", "tuwariDictionary.lift", package="interlineaR")
#' dictionary <- read.lift(path, vernacular.languages="tww", simplify=TRUE, get.example=FALSE)
#'
#' # No metadata file
#' #write_CLDF(dictionary, dir="~", meta=NULL)
#'
#' # with no 'meta' argument, create_CLDF_metadata() is called:
#' #write_CLDF(dictionary, dir="~")
#'
#' # Use create_CLDF_metadata to create a skeleton to be edited:
#' meta <- create_CLDF_metadata(dictionary)
#' meta$foo$bar <- "gaz"
#' #write_CLDF(dictionary, dir="~", meta=meta)
write_CLDF <- function(l, dir=".", cldf_dir="CLDF", meta=create_CLDF_metadata(l), metadata_filenames="metadata.json") {

  ## The bare minimum: URLs for each table
  urls <- vector(mode = "character", length = length(l))
  ## either from the metadata, if any...
  if (!is.null(meta)) {
    urls <- sapply(meta$tables, `[[`, "url")
    if (any(sapply(urls, is.null))) stop("The metadata doesn't contain URLs")
  } else {
    ## ...or from the slot names.
    if (is.null(names(l))) stop("Can't guess filenames: no metadata and no slot names in the outer list.")
    urls <- paste0(names(l), ".csv")
  }

  ## Create the output directory
  fullpath <- paste0(dir, "/", cldf_dir)
  res <- dir.create(fullpath)
  if (!res) {
    stop(paste0("Unable to create ", fullpath))
  }

  ## Write the metadata file
  if (!is.null(meta)) {
    serialized <- toJSON(meta, auto_unbox = TRUE, pretty = TRUE)
    cat(serialized, file = paste0(fullpath, "/", metadata_filenames))
  }

  ## Write the tables
  write_CLDF_table_list(l, urls, meta$dialect, fullpath)
}

#' Generate a CLDF metadata tree from a list of data frames representing a CLDF Module
#'
#' Metadata are represented as lists of lists. The order of the Component metadata in the "tables"
#' slot match the order of the data frame in the list.
#'
#' @param l list: the list of data.frames
#'
#' This function generate a metadata tree according to the following heuristic:
#' 
#' 1/ generating metadata for the Module:
#' 
#' \itemize{
#'   \item if the list has an attribute "CLDF_metadata", the content of
#'   this attribute is supposed to be a tree of lists expressing the metadata for this object and it
#'   is aggregated to the returned metadata.
#'   \item if the outer list has CLDF Module name in its "class" attribute,
#'   it will be used for determining the Module name.
#'   \item in last ressort, the names of the list elements (the tables) are matched
#'   with the default Components of each Module in order to determine the Module name.
#'   \item if everything fails an error is thrown.
#' }
#' 
#' 2/ generating metadata for the Components:
#'
#' \itemize{
#'   \item for each inner table, if it has an attribute "CLDF_metadata", the content of
#'   this attribute is supposed to be a tree of lists expressing the metadata for this object and it
#'   is aggregated to the returned metadata.
#'   \item if it has a CLDF Component name in its "class" attributes,
#'   it will also be used for generating the correct metadata.
#'   \item The table names are matched with the Component "alternate" names (ie. lower case and pluralised: entries for EntryTable, etc.)
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
#' dictionary <- read.lift(path, vernacular.languages="tww", simplify=TRUE, get.example = FALSE)
#' class(dictionary) <- append(class(dictionary), "Dictionary")
#' meta <- create_CLDF_metadata(dictionary)
#'
#' # If the list does not contain any class information nor a CLDF_metadata attribute,
#' # the Module the slot names of the list are matched with the Component alternate names.
#' path <- system.file("exampleData", "tuwariDictionary.lift", package="interlineaR")
#' dictionary <- read.lift(path, vernacular.languages="tww", simplify=TRUE, get.example = FALSE)
#' meta <- create_CLDF_metadata(dictionary)
#'
#' # The returned list can be edited
#' meta <- create_CLDF_metadata(dictionary)
#' meta$tables[[2]]$`dc:conformsTo` <- "http://cldf.clld.org/v1.0/terms.rdf#SenseTable"
#' #write_CLDF(dictionary, meta=meta)
create_CLDF_metadata <- function(l) {
  meta <- list()
  meta$`@context` <- list("http://www.w3.org/ns/csvw", list("@language" = "en"))
  module_name <- ""

  CLDF_metadata <- attr(l, "CLDF_metadata")
  CLDF_classname <- get_CLDF_classname(l)
  ## Get the Module and the Module metadata
  if (!is.null(CLDF_metadata)) {
    meta <- CLDF_metadata
    module_qname <- meta$`dc:conformsTo`
    module_name <- strsplit(module_qname, "#")[[1]][2]
  } else {
  	if (!is.null(CLDF_classname)) {
  		module_name <- CLDF_classname
  		meta$`dc:conformsTo` <- paste0("http://cldf.clld.org/v1.0/terms.rdf#", module_name)
  		meta$dialect$commentPrefix <- list(NULL) ## TODO buggy here: create a sublist.
  	} else {
  		# ugly
  		meta <- guess_modules_metadata(paste0(names(l), ".csv"))
  	}
  }

  
  ## Aggregate pieces of information regarding the various Components
  default_component_names <- names(l)
  meta$tables <- mapply(get_CLDF_Component_metadata, l, default_component_names, SIMPLIFY = FALSE, USE.NAMES = FALSE)

  return(meta)
}

#' Look for the module name in the class attribute.
#' @param l the list of data.frame
#' @noRd
get_CLDF_classname <- function(l) UseMethod("get_CLDF_classname")

get_CLDF_classname.Dictionary <- function(l) return("Dictionary")

get_CLDF_classname.ParallelText <- function(l) return("ParallelText")

get_CLDF_classname.StructureDataset <- function(l) return("StructureDataset")

get_CLDF_classname.Wordlist <- function(l) return("Wordlist")

get_CLDF_classname.IGTCorpus <- function(l) return("Generic")

get_CLDF_classname.default <- function(l) return(NULL)

#' Try to extract the medata of a table from an attribute.
#' If no metadata are present, ask "get_CLDF_Component_name_with_classname".
#' @param table the data.frame
#' @param tablename a slot name in the outer list to be used in last resort
#' @noRd
get_CLDF_Component_metadata <- function(table, tablename) {
  meta <- attr(table, "CLDF_metadata")
  if (is.null(meta)) {
    component_name <- get_CLDF_Component_name_in_candidates(class(table))
    if (is.null(component_name)) {
    	component_name <- get_CLDF_Component_name_with_surname(tablename);
    	if (is.null(component_name)) {
    		stop(paste0("No information found about the Component implemented by the table: ", tablename))
    	}
    }
    meta <- get_component_default_metadata(component_name)
  }
  return(meta)
}

get_CLDF_Component_name_in_candidates <- function(candidates) {
	Components <- c("BorrowingTable", "CognateTable", "CognatesetTable", "EntryTable", "ExampleTable", "FormTable", "FunctionalEquivalentTable", "FunctionalEquivalentsetTable", "LanguageTable", "ParameterTable", "SenseTable", "ValueTable")
	i <- candidates %in% Components
  if (sum(i) == 1) {
  	return(candidates[i])
  } else if (sum(i) > 1) {
  	stop(paste0("Several Component while one is expected: ", paste(Components[i], collapse=" ")))
  } else {
  	return(NULL)
  }
}

get_CLDF_Component_name_with_surname <- function(surname) {
	Components <- c(borrowings="BorrowingTable", cognates="CognateTable", cognatesets="CognatesetTable", entries="EntryTable", examples="ExampleTable", forms="FormTable", functionalEquivalents="FunctionalEquivalentTable",functionalEquivalentsets="FunctionalEquivalentsetTable", languages="LanguageTable", parameters="ParameterTable", senses="SenseTable", values="ValueTable")
	component <- Components[surname]
	if (is.na(component)) {
		return(NULL)
	}
	return(component)
}

write_CLDF_table_list <- function(l, urls, dialect, fullpath) {
  stopifnot(length(l) == length(urls))

  ## TODO the following three lines are copied from read.CLDF. To be reused.
  encoding <- ifelse(!is.null(dialect$encoding), dialect$encoding, "UTF-8")
  header <- ifelse(!is.null(dialect$header), dialect$header, TRUE)
  delimiter <- ifelse(!is.null(dialect$delimiter), dialect$delimiter, ",")

  for (i in 1:length(l)) {
    table <- l[[i]]
    url <- urls[i]

    write.table(table, paste0(fullpath, "/", url),
      col.names = header, sep = delimiter, quote = TRUE, # TODO ?
      fileEncoding = encoding
    )
  }
}

get_component_default_metadata <- function(component_name) {
  path <- system.file("cldf/Components/", paste0(component_name, ".json"), package = "interlineaR")
  lines <- readLines(path)
  specif <- fromJSON(lines, simplifyVector = FALSE)
  return(specif)
}
