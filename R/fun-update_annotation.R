#' update library annotation
#'
#' Check all geneIDs in library annotation file against GeneBank and get up-to-date information.
#'
#' Since information in data bases can change it is prudent
#' to refresh the library annotation from time to time.
#' This function takes every geneID in an annotation file and checks its current status
#' in GeneBank: whether is has been withdrawn or replaced (and if so, by what new geneID).
#' It then queries Genebank again to retrieve the gene type (protein-coding, pseudogene, etc.).
#' Once the geneIDs are updated, yet another query is sent to GeneBank
#' to retrieve the current gene symbol, gene description, map location,
#' chromosome number, and aliases. This is done in batches of up to 499 items at a time.
#' (This seems like an odd limit but \code{reutils} forces
#' saving results to a file at 500 or more records per query.)
#'
#' Original geneIDs and gene symbols are kept in separate columns,
#' \code{original_geneid} and \code{original_gene_symbol}.
#'
#' @section File format:
#' As of version 2.4 the function has undergone some generalization.
#' It now serves not only the original Dharmacon file but also other text files.
#' The input file may be tab- or comma delimited.
#' It must contain the following information:
#' plate number, well/position (e.g. A01), and geneID.
#' All other columns are immaterial but will not be dropped.
#'
#' Annotation files can be re-updated. In such a case the columns
#' \code{original_geneid} and \code{original_gene_symbol} will remain as they are
#' and the update will be run with original geneIDs rather than the updated ones.
#'
#' @section Dependencies:
#' GeneBank queries are handled with the package \code{reutils}.
#' Random errors that occur on queries are handled with \code{retry}.
#' Data is loaded (and saved) with package \code{data.table}.
#' Data processing is mostly done in base R, with (minimal) use of \code{tidyr}.
#' Several internal functions are called here, see \code{Functions}.
#'
#' @section Processing time:
#' \code{check_geneid_status}
#' queries GeneBank one geneID at a time, which may swamp the server,
#' hence a 0.5 second pause is introduced before every query.
#'
#' @param infile file containing the original annotation;
#'               must be compatible with \code{\link[data.table]{fread}};
#'               deafults to internally stored Dharmacon annotation from 16th May 2015
#'               (plate numbers have been unified, originally each subset was numbered independently)
#' @param outfile (optional) path to a file to save the updated annotation
#' @param verbose logical whether or not to report progres,
#' @param ... ellipsis to facilitate control of internal functions' verbosity
#'
#' @return The function either invisibly returns a data frame
#'         or saves to a specified path and returns nothing.
#'
#' @export
#'
#' @seealso
#' \code{\link[reutils]{reutils}}, \code{\link[siscreenr]{retry}}, \code{\link[base]{dots}}
#'

update_annotation <- function(infile, outfile, verbose = FALSE, ...) {
  # check if infile exists
  if (!missing(infile) && !file.exists(infile)) stop('"infile" not found')
  # check if infile is different from outfile
  if (!missing(infile) & !missing(outfile)) {
    if (infile == outfile) {
      warning('"infile" will be overwritten!', immediate. = TRUE)
      what.do <- readline('continue? (Yes/No) ')
      if (is.element(what.do, c('No', 'no', 'N', 'n')))
        stop('interrupted by user', call. = FALSE) else
          if (what.do != 'Yes') stop('invalid choice')
    }
  }
  # check if outfile can be created (only in different to infile)
  if (!missing(infile) & !missing(outfile)) {
    if (infile != outfile) {
      tryCatch(file.create(outfile),
               message = function(m) stop('there is a problem with creating "outfile"', call. = FALSE),
               warning = function(w) stop('there is a problem with creating "outfile"', call. = FALSE),
               error = function(e) stop('there is a problem with creating "outfile"', call. = FALSE)
      )}
  }
  # load original annotation
  if (verbose) message('loading annotation')
  annotation_original <-
      if (missing(infile)) {
        readRDS('extdata/ANNOTATION.LIBRARY.GENOMIC_20150416.original.plates.adjusted.rds')
        } else {
          data.table::fread(file = infile, check.names = TRUE)
        }

  # drop data.table class as it would will cause problems
  data.table::setDF(annotation_original)

  # TODO: check if file format is valid

  # change column names to lower case
  nms <- tolower(names(annotation_original))

  # these columns are added during the update
  # if they are all here already, it means the file was previously updated
  added_columns <- c('gene_type', 'withdrawn', 'replaced', 'gene_symbol',
                     'description', 'map_location', 'chromosome', 'aliases',
                     'original_geneid', 'original_gene_symbol')

  if (all(is.element(added_columns, nms))) {
    ## if the file has undergone a previous update
    # hold on to "original geneids"
    hold_geneid <- annotation_original$original_geneid
    hold_gene_symbol <- annotation_original$original_gene_symbol
    # remove all added variables
    ind <- !is.element(nms, added_columns)
    annotation_original <- annotation_original[ind]
    # restore old geneids and gene symbols
    annotation_original$geneid <- hold_geneid
  } else {
    ## if this is a new file, the format must be unified
    # define a function
    replacer <- function(x, string, replacement, error, error2) {
      ind <- grep(string, x)
      if (length(ind) == 1) x[ind] <- replacement else
        if (length(ind) == 0) stop(error, "\n\ttry: ", replacement, call. = FALSE) else
          stop(error2, ':\n\t', paste(x[ind], collapse = ', '), call. = FALSE)
      return(x)
    }
    # create vectors of regexs, replacements, and error messages
    strings <- c('^plate[-,_,\\., ]?(no|num|number)?', '^wells?|^positions?',
                 'gene[-,_,\\., ]?ids?', 'gene.?symbols?')
    replacements <- c('plate', 'position', 'geneid', 'original_gene_symbol')
    errors <- paste('"data" contains no apparent specification of',
                    c('plate numbers', 'position', 'gene ids', 'gene symbols'))
    errors2 <- paste('"data" contains ambiguous specification of',
                     c('plate numbers', 'position', 'gene ids', 'gene symbols'))
    # run the function across the column names
    for (i in seq_along(strings)) {
      nms <- replacer(nms, strings[i], replacements[i], errors[i], errors2[i])
    }
    # replace column names
    names(annotation_original) <- nms
    ## required column names are now present and gene_symbol is renamed to original_gene_symbol
  }

  # change geneids to character, required for merging later
  annotation_original$geneid <- as.character(annotation_original$geneid)
  # extract original geneIDs
  suppressWarnings({
    geneids <- as.numeric(unique(annotation_original$geneid))
    geneids <- geneids[!is.na(geneids)]
  })

  # check geneID status
  if (verbose) message('checking geneID status')
  if (verbose) double_check <- check_geneids(geneids, verbose, ...) else
    suppressMessages(double_check <- check_geneids(geneids, verbose, ...))
  # add geneID status to annotation and amend geneIDs
  annotation_checked <- merge(annotation_original, double_check, by = 'geneid', all = TRUE)
  annotation_checked$original_geneid <- annotation_checked$geneid
  annotation_checked$geneid <- ifelse(is.na(annotation_checked$new_geneid),
                                      annotation_checked$original_geneid,
                                      annotation_checked$new_geneid)
  # extract new geneIDs
  suppressWarnings({
    geneids <- as.numeric(unique(annotation_checked$geneid))
    geneids <- geneids[!is.na(geneids)]
  })
  # get gene fields for all new geneIDs
  if (verbose) message('getting locus info...')
  fields <- get_gene_fields_batch(geneids, verbose, ...)
  # append fields to amended annotation
  if (verbose) message('appending to annotation...')
  annotations_merged <- merge(annotation_checked, fields, by = 'geneid', all = TRUE)
  # update annotation
  if (verbose) message('formatting annotation...')
  annotation_updated <- annotations_merged
  # update names
  nms <- names(annotation_updated)
  nms[grepl('duplex.?catalog.?number', nms)] <- 'duplex_catalog_number'
  nms[grepl('pool.?catalog.?number', nms)] <- 'pool_catalog_number'
  nms[grepl('gene.?accession', nms)] <- 'gene_accession'
  names(annotation_updated) <- nms
  # modify "plate" and "position" columns
  annotation_updated$plate <- as.numeric(gsub('[P,p]late[_,-,\\., ]?', '', annotation_updated$plate))
  annotation_updated$position <- toupper(annotation_updated$position)

  # function that wraps several fields in a column into a single string
  wrap <- function(column) {
    f <- list(annotation_updated$plate, annotation_updated$position)
    wrapped <- tapply(annotation_updated[[column]], f, FUN = paste, collapse = ', ')
    wrapped <- unsplit(wrapped, f)
    annotation_updated[[paste0(column, 's')]] <- wrapped
    ind <- !is.element(names(annotation_updated), column)
    annotation_updated <- annotation_updated[ind]
    return(annotation_updated)
  }
  # apply function to column "sequence"
  if (is.element('sequence', nms)) {
    if (verbose) message('\t wrapping sequences')
    annotation_updated <- wrap('sequence')
  }
  # and again to column "duplex_catalog_nuber"
  if (is.element('duplex_catalog_number', nms)) {
    if (verbose) message('\t wrapping duplex catalog number')
    annotation_updated <- wrap('duplex_catalog_number')
  }

  # clean up duplicated rows, rearrange columns and rows
  if (verbose) message('\t rearranging')
  annotation_updated$new_geneid <- NULL
  vars_wishlist <- c('plate', 'position', 'geneid',
            'gene_symbol', 'aliases', 'description', 'map_location', 'chromosome', 'gene_type',
            'sequences', 'duplex_catalog_numbers', 'pool_catalog_number',
            'withdrawn', 'replaced', 'original_geneid', 'original_gene_symbol')
  vars_present <- intersect(vars_wishlist, names(annotation_updated))
  vars_other <- setdiff(names(annotation_updated), vars_wishlist)
  annotation_updated <- annotation_updated[c(vars_present, vars_other)]
  if (verbose) message('\t removing duplicate rows')
  annotation_updated <- annotation_updated[!duplicated(annotation_updated), ]
  if (verbose) message('\t reordering table')
  annotation_updated <- annotation_updated[order(annotation_updated$plate, annotation_updated$position), ]
  rownames(annotation_updated) <- 1:nrow(annotation_updated)

  # fill in "none"s and "???"s in geneID, gene symbol and description columns
  if (verbose) message('\t filling in missing values')
  # define function that will fill in missing values
  infiller <- function(data, column) {
    data[[column]] <- ifelse(data$geneid == 'none', 'none', data[[column]])
    data[[column]] <- ifelse(data$geneid == '???', '???', data[[column]])
    return(data)
  }
  # list variables that will be processed
  fill.in <- c('gene_symbol', 'aliases', 'description',
               'map_location', 'chromosome', 'gene_type', 'withdrawn', 'replaced')
  # execute
  for (f in fill.in) annotation_updated <- infiller(annotation_updated, f)

  # save or return result
  if (missing(outfile)) {
    if (verbose) message('finished')
    invisible(annotation_updated)
  } else {
    if (verbose) message('saving file')
    data.table::fwrite(annotation_updated, file = outfile, sep = '\t')
    if (verbose) message('done!')
  }
}

#' @describeIn update_annotation
#' runs \code{check_geneid_status} for all geneIDs and returns a \code{data.frame};
#' pauses for 0.5 second before each request to avoid swamping the server
#'
#' @keywords internal
#'
check_geneids <- function(geneIDs, verbose, ...) {
  # define function that counts iterations
  how_to_count <- function() {
    X <- length(geneIDs)
    iteration <- 1
    function(verbose, ...) {
      if (verbose) message('\t geneID ', iteration, ' of ', X, '...')
      iteration <<- iteration + 1
    }
  }
  count <- how_to_count()
  # define counting function operator for checking geneid status
  check_geneid_status_with_pause <- function(x, verbose, ...) {
    count(verbose, ...)
    Sys.sleep(0.5)
    check_geneid_status(x, verbose, ...)
  }
  m <- vapply(geneIDs, function(x) check_geneid_status_with_pause(x, verbose, ...), character(4))
  d <- as.data.frame(t(m), stringsAsFactors = FALSE)
  d$withdrawn <- as.logical(d$withdrawn)
  d <- tidyr::separate(d, 'replaced', c('replaced', 'new_geneid'), sep = 'ID: ')
  # # TODO: remove tidyr
  # strsplit(d$replaced, split = 'ID: ')
  # # end TODO
  d$replaced <- ifelse(is.na(d$replaced), FALSE, TRUE)
  d$new_geneid <- as.numeric(d$new_geneid)
  return(d)
}

#' @describeIn update_annotation
#' checks a single geneID and retrieves its withdrawn and replaced status (TRUE/FALSE)
#' and a potential new geneID; then retrieves the gene type (protein-coding, pseudo, etc.);
#' there is a 1 second pause between queries; returns a character vector
#'
#' @keywords internal
#'

check_geneid_status <- function(geneID, verbose, ...) {
  if (length(geneID) != 1L) stop('"geneID" must be of length 1')
  efetch_object <- reutils::efetch(geneID, db = 'gene', rettype = 'gene_table')
  efetch_text <- reutils::content(efetch_object, as = 'text')
  withdrawn <- as.character(grepl('withdrawn', efetch_text))
  replaced <-
    if (grepl('replaced', efetch_text)) {
      grep('replaced', unlist(strsplit(efetch_text, split = '\n')), value = TRUE)
    } else NA_character_
  # a pause here prevents some delays I do not understand
  Sys.sleep(1)
  # get gene type
  if (verbose) message('\t getting gene type')
  gene_type <- retry(get_gene_type(geneID), 5, 'failed to retrieve', verbose, ...)
  result <- c(geneid = as.character(geneID), withdrawn = withdrawn, replaced = replaced,
              gene_type = gene_type)
  if (length(result) == 4) return(result) else browser()
}

#' @describeIn update_annotation
#' queries the gene data base and retrieves five fields:
#' gene symbol, gene description, map location, chromosome number, and aliases
#' (other geneIDs associated with the geneID);
#'
#' @keywords internal
#'
get_gene_fields <- function(geneIDs) {
  # get gene symbol, aliases, description, map location and chromosome
  e_object <- reutils::efetch(geneIDs, db = 'gene', rettype = 'docsum', retmode = 'text')
  e_object_as_text <- reutils::content(e_object, as = 'text')
  split_text <- strsplit(e_object_as_text, split='<[/]?Name>')[[1]]
  gene_symbol <- split_text[seq(from = 2, to = length(split_text), by = 2)]
  split_text <- strsplit(e_object_as_text, split='<[/]?OtherAliases>')[[1]]
  aliases <- split_text[seq(from = 2, to = length(split_text), by = 2)]
  split_text <- strsplit(e_object_as_text, split='<[/]?Description>')[[1]]
  description <- split_text[seq(from = 2, to = length(split_text), by = 2)]
  split_text <- strsplit(e_object_as_text, split='<[/]?MapLocation>')[[1]]
  map_location <- split_text[seq(from = 2, to = length(split_text), by = 2)]
  split_text <- strsplit(e_object_as_text, split='<[/]?Chromosome>')[[1]]
  chromosome <- split_text[seq(from = 2, to = length(split_text), by = 2)]

  return(
    data.frame(
      geneid = as.character(geneIDs), gene_symbol, aliases, description, map_location, chromosome,
      stringsAsFactors = F))
}

#' @describeIn update_annotation
#' runs \code{get_gene_fields} in batches of 499 and less; this is necessary as
#' the results of \code{efetch} are unworkable for larger sets
#'
#' @keywords internal
#'
get_gene_fields_batch <- function(geneIDs, verbose, ...) {
  # we shall be calling efetch, which can only be done for less than 500 geneIDs at a time
  # test how many items there are
  howmany <- length(geneIDs)
  # if there area less that 500 items, a single call suffices
  if (howmany < 500) {
    if (verbose) message('\t ... in one batch') # scoping not working...
    return(get_gene_fields(geneIDs))
  } else {
    # if there are 500 or more, we shall do it in 499-item steps
    # how many steps will there be?
    steps <- ceiling(howmany / 499)
    if (verbose) message('\t ... in ', steps, ' batches') # scoping not working...
    # create funcion factory that will select a 499-long intervals from a long vector
    stepper <- function(step) {
      # return function that returns the i-th 499-element section of x
      function(x, verbose, ...) {
        if (verbose) message('\t\t batch ', step)
        X <- x[1:499 + 499 * (step -1)]
        X[!is.na(X)]
      }
    }
    # run the stepper function factory:
    # generate a list of functions that each returns a section of a vector
    steppers <- lapply(1:steps, stepper)
    # separate geneIDs into list of intervals
    ranges <- lapply(steppers, function(f) f(geneIDs, verbose, ...))
    # do the deed over the list
    parts <- lapply(ranges, get_gene_fields)
    # wrap into single data frame
    return(do.call(rbind, parts))
  }
}

#' @describeIn update_annotation
#' queries the gene data base and retrieves the gene type field
#'
#' @keywords internal
#'
get_gene_type <- function(geneID) {
  efetch_object <- reutils::efetch(geneID, db = 'gene')
  efetch_text <- reutils::content(efetch_object, as = 'text')
  if (is.null(efetch_text)) stop('efetch returned NULL')
  efetch_text_split <- strsplit(efetch_text, '\n')[[1]]
  gene_type_field <- grep('gene_type', efetch_text_split, value = TRUE)
  gene_type_field_split <- strsplit(gene_type_field, '\"')
  extract_field(gene_type_field_split)
}

#' @describeIn update_annotation
#' called by \code{get_gene_type} to extract the gene type
#' with provisions in case the field does not exist or is NA
#'
#' @keywords internal
#'
extract_field <- function(x) {
  tryCatch(x[[1]][2], error = function(e) return('unknown gene type'))
  if (is.na(x[[1]][2])) return('unknown gene type') else return(x[[1]][2])
}
