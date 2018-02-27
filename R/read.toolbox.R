#' Read a toolbox (SIL) text file and turn it into a list of data frame.
#' 
#' The triplet of lines (mb, ge, ps) (containing reps. morphems, glosses
#' and part of speech) that do not have an equal number of white-space
#' separated item ('unaligned') are discarded.
#'
#' @param path : the path to a toolbox text file.
#' 
#' @param add.unaligned : If TRUE: if any of the mb/ge/ps triplet does not have the same
#' number of morphems (resp. morphem glosses and part of speech),
#' an extra slot named 'unaligned' is added to the returned list giving information on all the unaligned triplets. If FALSE: print
#' a warning message only.
#' 
#' @return a list with three slots "texts", "sentences" and "morphemes", each containing a data frame.
#' In these data frame, each row is an occurrence of the unit concerned.
#' 
#' @export
#' @seealso read.emeld
#' @references https://software.sil.org/toolbox/
#' @importFrom stats reshape
#' @examples
#' corpuspath <- system.file("exampleData", "tuwariToolbox.txt", package="interlineaR")
#' corpus <- read.toolbox(corpuspath)
read.toolbox <- function(path, add.unaligned=FALSE) {
  lines <- readLines(path)

  ## clean texts
  fields <- .line2field(lines);
  fields <- .remove.header(fields, "id");
  ## Blank lines cannot be remove yet. They are indicative of interlinear triplet (mb, ge, ps)
  #fields <- fields[!grepl(x=fields, pattern = "^$")];
  
  texts_index     <- grepl(pattern = "\\\\id",  x=fields, perl=TRUE);
  sentences_index <- grepl(pattern = "\\\\ref", x=fields, perl=TRUE);
  triplet_index   <- grepl(pattern = "^$",      x=fields, perl=TRUE);

  texts_ids      <- cumsum(texts_index);
  sentences_ids  <- cumsum(sentences_index);
  triplet_ids    <- cumsum(triplet_index);
  
  ## Now remove blank lines
  fields         <- fields[!triplet_index]
  texts_ids      <- texts_ids[!triplet_index]
  sentences_ids  <- sentences_ids[!triplet_index]
  triplet_ids    <- triplet_ids[!triplet_index]
  
  ## extract field name
  field.end   <- regexpr(" ", fields )
  field_name  <- sapply(fields, substr, start=2, stop=field.end) # trimws();
  field_value <- sapply(fields, substr, start=field.end+1, stop=length(fields))# trimws();
  
  # field_name <- trimws(field_name);
  # field_value <- trimws(field_value);
  
  df <- data.frame(
    texts_ids=texts_ids,
    sentences_ids=sentences_ids,
    triplet_ids=triplet_ids,
    field_name=field_name,   # contain "id" and "ref", "tx", and "ge", mb", "ps"...
    field_value=field_value  # contain "you Accpl woman -? to_sleep -Ask", "Pr mode n -sfx.n v -sfx.v", etc.
  );

  res <- list();
  res$texts <- df[field_name == "id ", c(1,5)]
  
  sentences <- df[field_name %in% c("ref", "tx ", "nt ", "ft "), c(1,2,4,5)]
  sentences <- reshape(sentences, idvar = c("texts_ids", "sentences_ids"), direction = "wide", timevar="field_name");
  colnames(sentences) <- c("texts_ids", "sentences_ids", "ref", "tx", "nt", "ft");
  res$sentences <- sentences;

  ## Checking that there is the same nbr of mb, ge and ps field by triplet
  ## No: reshape take care of this below
  # s <- split(df, f=list(df$sentences_ids, df$triplet_ids));
  # n.mb.by.triplet <- sapply(s, function(x) {sum(x$field_name == "mb " )})
  # n.ge.by.triplet <- sapply(s, function(x) {sum(x$field_name == "ge " )})
  # n.ps.by.triplet <- sapply(s, function(x) {sum(x$field_name == "ps " )})
  # same.nb <- n.mb.by.triplet == n.ge.by.triplet & n.mb.by.triplet == n.ps.by.triplet
  # different <- names(n.mb.by.triplet[! same.nb]);
  # if (length(different) > 0) {
  #   warning(paste0(length(different), " incomplet interlinear triplet(s) (ie. mb/ge/ps group) are deleted belonging to sentence(s): ", paste( unique(df$sentence_ids[df$triplet_ids %in% different]) ,collapse=" ")))
  #   df[!df$sentences_id == as.numeric(different),]
  # }
  
  ## from long to wide format (with mb, ge and ps as different columns)
  morphs <- df[df$field_name %in% c("mb ", "ge ", "ps "), ];
  morphs <- reshape(morphs, idvar = c("texts_ids", "sentences_ids", "triplet_ids"), direction = "wide", timevar="field_name");
  colnames(morphs) <- c("texts_ids", "sentences_ids", "triplet_ids", "mb", "ge", "ps");

  ## tokenizing each of mb, ge, ps columns.
  sep=' +';
  ### First, we have to remove unequals length fields.
  mb <- strsplit(as.character(morphs$mb), sep, perl = TRUE);
  ge <- strsplit(as.character(morphs$ge), sep, perl = TRUE);
  ps <- strsplit(as.character(morphs$ps), sep, perl = TRUE);
  lmb <- sapply(mb, length);
  lge <- sapply(ge, length);
  lps <- sapply(ps, length);
  different <- lmb != lge | lmb != lps
  ndifferent <- length(different);
  if (ndifferent > 0) {
    ref <- res$sentences[ morphs[different,]$sentences_ids, "ref"];
    if (add.unaligned) {
      res$unaligned <- data.frame(ref=ref, mb=morphs$mb[different], ge=morphs$ge[different], ps=morphs$ps[different]);
    } 
    msg <- paste0(
      length(different),
      " interlinearized lines were deleted ",
      "due to un enequals numbers of morphems between the mb, ge and ps lines. ",
      "These interlinearized lines belong to the following references: ",
      paste(unique(ref), collapse= " ")
    );
    warning(msg);
  }

  ### Then, the data frame is reshaped either with base R... 
  morphs <- morphs[!different,];
  ntokens   <- lmb[!different];
  morphs <- data.frame(
    texts_ids     = rep(morphs$texts_ids, ntokens),
    sentences_ids = rep(morphs$sentences_ids, ntokens),
    #triplet_ids= rep(morphs$triplet_ids, ntokens), ## we get rid of this information now
    mb = unlist(mb[!different]),
    ge = unlist(ge[!different]),
    ps = unlist(ps[!different])
  );

    ### ...or using dplyr
    # morphs <- morphs[!different,];
    # morphs %>%
    # group_by(triplet_ids) %>%
    # mutate(
    #   mb = strsplit(as.character(mb), sep),
    #   ge = strsplit(as.character(ge), sep),
    #   ps = strsplit(as.character(ps), sep)
    # ) %>%
    # unnest();
  
  res$morphems <- morphs;
  return(res);
}

# read.dictionary <- function (url) {
#   lines <- readLines(url)
#   field_merged <- .line2field(lines);
#   
#   field_merged <- .remove.header(field_merged, "lx");
#   
#   entry <- .group.by.key(field_merged, "lx");
#   
#   # extract field name
#   field_name <- lapply(entry, substr, start=2, stop=3)
#   
#   # extract field value
#   for (i in 1:length(entry)) {
#     x <- entry[[i]];
#     x <- substr(x, start=5, stop=nchar(x, type="c"));
#     entry[[i]] <- x
#   }
#   
#   # add field names to value
#   for (i in 1:length(entry)) {
#     x <- entry[[i]];
#     names(x) <- field_name[[i]];
#     entry[[i]] <- x
#   }
#   return(entry)
# }

# read and collapse multi-line fields into one line.
.line2field <- function(lines) {
  continuing.index <- grep(pattern = "^[^\\\\]", x=lines);
  lines[continuing.index-1] <- paste(lines[continuing.index-1], lines[continuing.index], sep=" ");
  lines <- lines[-continuing.index];
  return(lines);

  # field_index <- grep(pattern = "^\\\\", x=lines, perl=TRUE)
  # length_field <- c(field_index[2:length(field_index)] - field_index[1:(length(field_index)-1)], (length(lines)+1) - field_index[length(field_index)]);
  # field_id <- rep(1:length(field_index), length_field)
  # field <- split(lines, field_id)
  # field_merged <- sapply(field, paste, collapse=" ");
  # return(field_merged);
}

#' Remove all lines up to the first line beginning with "\ref"
#'
.remove.header <- function(field, key) {
  # group fields by entry.
  key <- paste("^\\\\", key, sep="");
  while(! grepl(pattern=key, x=field[1], perl=T)) {
    field <- field[-1];
  }
  return(field);
}


.get.index <- function(data, key) {
  key <- paste("^\\\\", key, sep="");
  id_index <- grep(pattern = key, x=data, perl=TRUE)
  return(id_index);
}

# .group.by.key <- function(data, key) {
#   key <- paste("^\\\\", key, sep="");
#   entry_index <- grep(pattern = key, x=data, perl=TRUE)
#   length_entry <- c(entry_index[2:length(entry_index)] - entry_index[1:(length(entry_index)-1)], (length(data)+1) - entry_index[length(entry_index)]);
#   entry_id <- rep(1:length(entry_index), length_entry)
#   data <- split(data, entry_id)
#   return(data);
# }
