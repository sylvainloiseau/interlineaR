#' Read a set of csv files according to a metadata files following the CLDF format.
#'
#' Read the metadata file in order to get the csv dialect specification, Module and Component information.
#' The CLDF name (for module and component) are recorded in the class attribute of the outer list and of the inner data.frame).
#' The metadata fragment describing a table is recorded in an "CLDF_metadata" attribute on each data frame.
#' 
#' @param directory path to the directory containing the CLDF bundle
#'
#' @param metadata_filename name of the metadata file in the directory
#' 
#' @return a list of data frame.
#'
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom utils read.table
read.CLDF <- function(directory, metadata_filename) {
  ## Read file and parse JSON
	metaFile <- readLines(paste0(directory,"/",metadata_filename))
  meta <- fromJSON(metaFile, simplifyVector = FALSE)
  
  ## filename of each csv file
  filenames <- sapply(meta$tables, function(x) x$url)
  
  ## CLDF type url for each csv file
  qtypes <- sapply(meta$tables, function(x) x$`dc:conformsTo`)
  
  ## short CLDF type for each csv file
  types <- sapply(strsplit(qtypes, "#"), `[[`, 2)

  ## a R name for each table : either the the short CLDF type, or
  ## back to the filename
  tablename <- mapply(ifelse, (!is.null(types)) & types != "", types, filenames)
  
  ## Get dialect definition from metadata or back to default
  dialect <- meta$dialect
  
  encoding <- getMetaOrDefault(dialect$encoding, "utf-8")
  lineTerminators <- getMetaOrDefault(dialect$lineTerminators, 
    c("\r\n", "\n"))
  quoteChar <- getMetaOrDefault(dialect$quoteChar, "\"")
  doubleQuote <- getMetaOrDefault(dialect$doubleQuote, TRUE)
  skipRows <- getMetaOrDefault(dialect$skipRows, 0)
  commentPrefix <- getMetaOrDefault(dialect$commentPrefix, 
    "#")
  header <- getMetaOrDefault(dialect$header, TRUE)
  headerRowCount <- getMetaOrDefault(dialect$headerRowCount, 
    1)
  delimiter <- getMetaOrDefault(dialect$delimiter, ",")
  skipColumns <- getMetaOrDefault(dialect$skipColumns, 0)
  skipBlankRows <- getMetaOrDefault(dialect$skipBlankRows, 
    FALSE)
  skipInitialSpace <- getMetaOrDefault(dialect$skipInitialSpace, 
    FALSE)
  trim <- getMetaOrDefault(dialect$trim, FALSE)
  
  ## The outer list to be populated
  res <- list();
  
  for (i in 1:length(filenames)) {
  	## Get the colnames according to the metadata
    colnames <- sapply(meta$tables[[i]]$tableSchema$columns, `[[`, "name")
    
    ## Read the table
    table <- read.table(paste0(directory, "/", filenames[i]), 
      header = header, sep = delimiter, quote = quoteChar, 
      fileEncoding = encoding, comment.char = commentPrefix, #col.names = colnames, 
      blank.lines.skip = skipBlankRows, strip.white = trim, 
      skip = skipRows, stringsAsFactors = FALSE)
    
    ## TODO : unused: headerRowCount doubleQuote skipInitialSpace lineTerminators
    # http://w3c.github.io/csvw/metadata/#dialect-commentPrefix

    ## Skip Columns
    if (skipColumns > 0) {
      stopifnot(skipColumns <= ncol(table))
      table <- table[, (skipColumns + 1):ncol(table), drop = FALSE]
    }
    
    ## Store metadata information
    attr(table, "CLDF_metadata") <- meta$tables[[i]]
    class(table) <- c(class(table), types[i])
    
    res[[tablename[i]]] <- table;
  }

  return(res)
}

getMetaOrDefault <- function(meta, default) {
  ifelse(!is.null(meta), meta, default)
}
