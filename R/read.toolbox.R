#' Parse a Toolbox (SIL) text file
#' 
#' @param path length-1 character vector: the path to a toolbox text file.
#' @param text.fields.suppl character vector: the code of supplementary fields to be searched for each text (genre, ...). "id" is mandatory and need not to be listed here.
#' @param sentence.fields.suppl character vector: the code of supplementary fields to be searched for each sentence (such as ft, nt). "ref" is mandatory and need not to be listed here.
#' @param word.fields.suppl character vector: the code of supplementary fields to be searched for each word. "tx" is mandatory and need not to be listed here.
#' @param morpheme.fields.suppl character vector: the code of supplementary fields to be searched for each morpheme. "mb", "ge", "ps" are mandatory and need not to be listed here.
#'
#' @return a list with four slots "texts", "sentences", "words" and "morphemes",
#' each one containing a data frame. In these data frame, each row describe an occurrence
#' of the corresponding unit.
#' 
#' @export
#' @seealso \link{read.emeld} (XML vocabulary for interlinearized glossed texts)
#' @references https://software.sil.org/toolbox/
#' @importFrom reshape2 dcast
#' @examples
#' corpuspath <- system.file("exampleData", "tuwariToolbox.txt", package="interlineaR")
#' corpus <- read.toolbox(corpuspath)
read.toolbox <- function(path,
                         text.fields.suppl=NULL,
                         sentence.fields.suppl=c("tx", "nt", "ft"),
												 word.fields.suppl=NULL,
                         morpheme.fields.suppl=NULL, 
												 encoding = "unknown") {
 
  lines <- readLines(path, encoding = encoding);

  ## fields have mandatory elements for each level
  text.fields <- unique(c("id", text.fields.suppl));
  sentence.fields <- unique(c("ref", sentence.fields.suppl))
  word.fields  <- unique(c("tx", morpheme.fields.suppl))
  morpheme.fields  <- unique(c("mb", "ge", "ps", morpheme.fields.suppl))

  ## clean texts
  fields <- line2field(lines);

  ## Blank lines cannot be remove yet. They are indicative of interlinear triplet (mb, ge, ps)

  ## get index of id, ref and blank lines
  texts_index     <- grepl(pattern = "\\\\id",  x=fields, perl=TRUE);
  ### Remove all lines up to the first line beginning with "\id"
  first_id <- which(texts_index)[1]
  if(is.na(first_id)) {
  	stop("The file seems not to be well-formed. No \\id field (i.e. text title) found.")
  }
  if (first_id > 1) {
    fields <- fields[-(1:(first_id-1))]
    texts_index <- texts_index[-(1:(first_id-1))]
  }
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
  field.end   <- regexpr("\\\\\\b\\w+\\b", fields);
  field.end   <- attr(field.end, "match.length");
  field_name  <- substr(fields, start=2, stop=field.end)
  field_value <- substr(fields, start=field.end+2, stop=nchar(fields));
  field_name  <- trimws(field_name);
  field_value <- trimws(field_value);

  ## Create a dataframe in "long" (not wide) format: key / values pairs on two columns.
  df <- data.frame(
    texts_ids=texts_ids,
    sentences_ids=sentences_ids,
    triplet_ids=triplet_ids,
    field_name=field_name,   # contain "id" and "ref", "tx", and "ge", mb", "ps"...
    field_value=field_value,  # contain "you Accpl woman -? to_sleep -Ask", "Pr mode n -sfx.n v -sfx.v", etc.
    stringsAsFactors=FALSE
  );

  ## texts slot
  res <- list();
  res$texts <- df[field_name %in% text.fields, c(1,5)]

  ## sentences slot
  sentences <- df[field_name %in% sentence.fields, c(1,2,4,5)]
  ## Base R
  #sentences <- reshape(sentences, idvar = c("texts_ids", "sentences_ids"), direction = "wide", timevar="field_name");
  ## package reshape2
  sentences <- dcast(sentences, texts_ids + sentences_ids ~ field_name, fun.aggregate = paste, collapse=" ", value.var="field_value")
  ## package tidyr
  #sentences <- spread(sentences, key=field_name, value=field_value, fill="")
  res$sentences <- sentences;

  ## words slot
  words <- tokenize.aligned.field.set(df, fields=word.fields, masterfield="tx", unit_id_name="words_id");
  res$words <- words;

  ## morphemes slot
  words.outer <- df[field_name == "tx", "field_value"];
  morphemes <- tokenize.aligned.field.set(df, fields=morpheme.fields, masterfield="mb", unit_id_name="morphemes_id"); #, included.in=words.outer);
  res$morphemes <- morphemes;

  return(res);
}

#' read and collapse multi-line fields into one line.
#' One can encouter:
#'     \\mb samuel -we  ta -li  -lo
#'     nefi    -mwii
#' where a field is actually on two lines.
#' @param lines character vector : lines of the toolbox file
#' @noRd
line2field <- function(lines) {
	continuing.index <- grep(pattern = "^[^\\\\]", x=lines);
	if (length(continuing.index) > 0) {
		lines[continuing.index-1] <- paste(lines[continuing.index-1], lines[continuing.index], sep=" ");
		lines <- lines[-continuing.index];
	}
	return(lines);
}

#' Get the boudaries (start, end) of tokens in a vector of untokenized string.
#'
#' For words (tx) or morphemes (mb), several other fields may be aligned according to the positions
#' of tokens in this two "master" fields. The end boundaries of token is the last character of the token
#' or, if there are following white space, the last white space following it.
#' 
#' @param string character vector: strings to be tokenized in morphemes
#' @return a list with two slot: (i) "index_mb_start", a list of vectors 
#' (one per string in mb) giving the position of the first character of each morphemes and (ii)
#' "index_mb_end", a list of vectors (one per string in mb) giving the position of the last character
#' of each morphemes
#' @noRd
get.tokens.boundaries <- function(string) {
  index_start <- gregexpr("( [^ ]|^.)", string, perl=T);
  #mb_lengths <- nchar(string);

  index_end <- lapply(index_start, function(x) { x[-1] });
  index_end <- mapply(function(x, y) { c(x, y) }, index_end, 10000); #mb_lengths
  index_start <- mapply(function(x, y) {c(1, x[-1]+1)}, index_start);

  index_start <- lapply(index_start, `attributes<-`, NULL)
  index_end <- lapply(index_end, `attributes<-`, NULL)
  return(list(index_start=index_start, index_end=index_end));
}

#' Tokenize a set of fields associated to an unit (the set of fields related to word segmentation for instance, or to morpheme segmentation).
#' 
#' Set of fields have a "master field" (such as tx for words, or mb for morphemes), and other fields
#' associated with it (such as "ge" and "ps" for the morphemes master field.)
#' 
#' @param longformat the data table of the toolbox fields in long format (melted)
#' @param fields character vector: the aligned fields to tokenize (for instance, "mb", "ge" and "ps")
#' @param masterfield the master in the fields to be tokenized (for instance, "mb").
#' @param unit_id_name character vector (length-1): the name of the column to be created containing an ID for each token.
#' @param included.in character vector: an optional super-ordinated unit (for instance words for morphemes).
#' @param included.in_id character vector (length-1): the name of the column to be created containing an ID of the outer unit.
#'
#' @return a data frame with one token by row and as many columns as fields.
#' 
#' @noRd
tokenize.aligned.field.set <- function(longformat,
																					fields=c("mb", "ge", "ps"),
																					masterfield="mb",
																					unit_id_name="morphemes_id",
																					included.in=NULL, included.in_id="words_id") {
  data <- longformat[longformat$field_name %in% fields, ];
  actual.fields <- unique(data$field_name); # actually found
  data <- dcast(data, texts_ids + sentences_ids + triplet_ids ~ field_name, fun.aggregate = paste, collapse=" ", value.var="field_value")

  if (nrow(data) == 0) {
  	stop(paste0("empty \"", masterfield, "\" matrix"))
  }

  is.empty <- data[[masterfield]]  == "";
  data <- data[!is.empty,];
  
  boundaries <- get.tokens.boundaries(data[[masterfield]]);
  start_b <- boundaries$index_start
  end_b <- boundaries$index_end
  
  n.tokens.by.master <- lengths(start_b);

  units <- data.frame(
    texts_ids     = rep(data$texts_ids, n.tokens.by.master),
    sentences_ids = rep(data$sentences_ids, n.tokens.by.master),
    triplet_ids   = rep(data$triplet_ids, n.tokens.by.master),
    stringsAsFactors=FALSE
  );
  
  units[[ unit_id_name ]] <- 1:sum(n.tokens.by.master);

  # bug TODO: the last "end" as to be replaced by the actual length of the string.
  for (f in actual.fields) {
    units[[f]]<- trimws(
      unlist(
        mapply(
          function(text, start, end) {
            substring(text, first = start, last= end)
          },
          data[[f]],
          start_b,
          end_b,
          SIMPLIFY=FALSE)
      )
    );
  }

  if (!is.null(included.in)) {
    super <- get.tokens.boundaries(included.in);
    outer_start <- super$index_start;
    
    ids <- mapply(function(inner, outer)  sapply(inner, function(x) sum(outer <= x)), start_b, outer_start, SIMPLIFY = FALSE )
    units[[included.in_id]] <- unlist(ids)
  }

  return(units);
}
