#' Read a set of CSV files following the CLDF format.
#'
#' Read the metadata file in order to get the CSV dialect specification, Module and Component information,
#' or use default values if no metadata file is provided.
#'
#' The metadata fragment describing a table is stored in an "CLDF_metadata" attribute on each data frame.
#'
#' @param directory length-1 character vector: path to the directory containing the CLDF files
#'
#' @param metadata_filename name of the metadata file in the directory, or NULL if no metadata file exists.
#' If NULL, we try to match the filenames in the directory with the default filenames of the Modules in order
#' to guess the Module of this data set.
#'
#' @return a list of data frames. The CLDF Module name is recorded in the class attribute of the outer list and the CLDF Component name(s)
#' are recorded in the class attributes of the inner data frames.
#'
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom utils read.table
#' @references http://cldf.clld.org https://github.com/cldf/cldf/
read_CLDF <- function(directory, metadata_filename=NULL) {

	stopifnot(length(directory) == 1)
  if (file.access(directory, 0) != 0) stop(paste0("The directory ", directory, " does not seem to exist"))
  if (file.access(directory, 4) != 0) stop(paste0("The directory ", directory, " cannot be read"))

  if (!is.null(metadata_filename)) {
  	stopifnot(length(metadata_filename) == 1)
  	metadata_filename_full <- paste0(directory, "/", metadata_filename)
  	if (file.access(metadata_filename_full, 0) != 0) stop(paste0("The metadata file ", metadata_filename_full, " does not seem to exist"))
  	if (file.access(metadata_filename_full, 4) != 0) stop(paste0("The metadata file ", metadata_filename_full, " cannot be read"))
  	
    ## Read file and parse JSON
    metaFile <- readLines(metadata_filename_full)
    meta <- fromJSON(metaFile, simplifyVector = FALSE)
  } else {
  	## If there is no metadata, we are supposed to be able to guess the Components
  	## through the filenames, the Module through the Components, and the metadata of this Module.
  	## Otherwise, an error is thrown.
    filenames <- list.files(directory, all.files = FALSE, include.dirs = FALSE, full.names = FALSE)
    meta <- guess_modules_metadata(filenames)
  }

	## Get dialect definition from metadata or back to default
	dialect <- get_dialect_or_default(meta$dialect)
	
  module_qname <- meta$`dc:conformsTo`
  module_name <- strsplit(module_qname, "#")[[1]][2]

  ## filenames, as ordered in metadata
  filenames <- sapply(meta$tables, `[[`, "url")
  
  ## qualified CLDF component url for each csv file
  components_qnames <- sapply(meta$tables, function(x) x$`dc:conformsTo`)
  ## short CLDF component name for each csv file
  component_names <- sapply(strsplit(components_qnames, "#"), `[[`, 2)

  ## an R name for each table
  tablename <- component_names

  ## The outer list to be populated
  res <- vector(mode = "list", length = length(tablename))
  names(res) <- tablename
  class(res) <- append(class(res), c("CLDF", module_name))
  module_metadata <- meta
  module_metadata$table <- NULL
  attr(res, "CLDF_metadata") <- module_metadata
  
  for (i in 1:length(filenames)) {
    ## Get the col names according to the metadata
    col_names <- sapply(meta$tables[[i]]$tableSchema$columns, `[[`, "name")

    ## TODO check if a specific dialect exist for this table?

    ## Read the table
    table <- read.table(paste0(directory, "/", filenames[i]),
      header = dialect$header, sep = dialect$delimiter, quote = dialect$quoteChar,
      fileEncoding = dialect$encoding, comment.char = dialect$commentPrefix, #col.names = col_names,
      blank.lines.skip = dialect$skipBlankRows, strip.white = dialect$trim,
      skip = dialect$skipRows, stringsAsFactors = FALSE
    )
    ## TODO : unused: headerRowCount doubleQuote skipInitialSpace lineTerminators
    # http://w3c.github.io/csvw/metadata/#dialect-commentPrefix
    
    if (ncol(table) == length(col_names)) {
    	colnames(table) <- col_names
    } else {
    	warning(paste("supplementary column(s) in data. Can't use metadata column names for table: ", tablename[[i]], ".") )
    }

    ## Skip Columns
    if (dialect$skipColumns > 0) {
      stopifnot(dialect$skipColumns <= ncol(table))
      table <- table[, (dialect$skipColumns + 1):ncol(table), drop = FALSE]
    }

    ## Store metadata information
    attr(table, "CLDF_metadata") <- meta$tables[[i]]
    class(table) <- append(class(table), component_names[i])

    res[[tablename[i]]] <- table
  }

  return(res)
}

guess_modules_metadata <- function(components, match.with.field="url") {
  modules <- c("Dictionary")
  for (module in modules) {
    m_specification <- get_modules_default_metadata(module)
    m_field <- "";
    if (match.with.field=="url") {
    	m_field <- sapply(m_specification$tables, `[[`, "url")
    } else if (match.with.field=="name") {
    	m_field <- sapply(strsplit(sapply(m_specification$tables, `[[`, "dc:conformsTo"), "#"), `[[`, 2)
    } else {
    	stop(paste0("Unknown argument: ", match.with.field))
    }
    if (setequal(components, m_field)) {
      meta <- m_specification
      return(meta)
    }
  }
  stop(paste(
    "No Module specification matching the urls: ",
    paste(components, collapse = " ")
  ))
}

get_dialect_or_default <- function(metadata_dialect) {
  default_dialect <- get_default_dialect()
  dialect <- list()
  for (feature in c("encoding", "lineTerminators", "quoteChar", "doubleQuote", "skipRows", "commentPrefix", "header", "headerRowCount", "delimiter", "skipColumns", "skipBlankRows", "skipInitialSpace", "trim")) {
    given <- metadata_dialect[[feature]]
    default <- default_dialect[[feature]]
    dialect[[feature]] <- ifelse(!is.null(given), given, default)
  }
  return(dialect)
}

get_default_dialect <- function() {
  dialect <- list(
    encoding = "utf-8",
    lineTerminators = c("\r\n", "\n"),
    quoteChar = "\"",
    doubleQuote = TRUE,
    skipRows = 0,
    commentPrefix = "#",
    header = TRUE,
    headerRowCount = 1,
    delimiter = ",",
    skipColumns = 0,
    skipBlankRows = FALSE,
    skipInitialSpace = FALSE,
    trim = FALSE
  )
  return(dialect)
}

get_modules_default_metadata <- function(module_name) {
  path <- system.file("cldf/Modules/", paste0(module_name, ".json"), package = "interlineaR")
  if (file.access(path, 0) != 0) stop(paste0("The specification file ", path, " does not seem to exist"))
  if (file.access(path, 4) != 0) stop(paste0("The metadata file ", path, " cannot be read"))
  
  lines <- readLines(path)
  specif <- fromJSON(lines, simplifyVector = FALSE)
  return(specif)
}

