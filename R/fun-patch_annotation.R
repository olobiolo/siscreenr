#' fix missing gene types in annotation file
#'
#' Patch errors in annotation that escaped \code{\link[acutils]{retry}}.
#'
#' Even though \code{update_annotation} uses \code{acutils::retry}
#' to alleviate random errors that occur when calling \code{reutils::efetch} in bulk,
#' some queries still return errors. These will manifest as a "failed to retrieve"
#' value in \code{gene_type}.
#'
#' This function extracts the erroneous rows from a file or table,
#' saves them to a temporary file in the current working directory,
#' and runs \code{update_annotation} on that file.
#' The fixed rows replace the bad ones in the original table and the fixed talbe is returned.
#'
#' Once called, will run recursively until no errors are left.
#' Will throw a warning if there are many errors to fix.
#'
#' @param x \code{data.frame}
#'          or path to file readable by \code{\link[data.table]{fread}}
#'
#' @return The annotation table with all bad rows fixed.
#'
#' @export
#'
patch_annotation <- function(x) {
  if (is.character(x)) x <- as.data.frame(data.table::fread(x))
  good <- x[x$gene_type != 'failed to retrieve', , drop = FALSE]
  bad <- x[x$gene_type == 'failed to retrieve', , drop = FALSE]
  if (nrow(bad) > 50)
    warning('Over 50 corrupt rows to fix! This may take a while.', immediate. = TRUE)
  data.table::fwrite(bad, 'residue.txt', sep = '\t')
  redone <- update_annotation('residue.txt', verbose = TRUE)
  on.exit(unlink('residue.txt'))
  # function that changes some columns to character
  tochar <- function(x) {
    # convert column in data frame to character, if exists
    # @param x a \code{data.frame}
    f <- function(data, column) {
      if (is.element(column, names(data))) {data[[column]] <- as.character(data[[column]])}
      return(data)
    }
    nms <- c("geneid", "ginumber", "gene_accession", "old_geneid",
             "sequences", "duplex_catalog_numbers", "duplex_catalog_numbers", "pool_catalog_number",
             "chromosome", "withdrawn", "replaced", "old_gene_symbol")
    for (n in nms)  x <- f(x, n)
    return(x)
  }
  good <- tochar(good)
  redone <- tochar(redone)
  corrected <- rbind(good, redone)

  corrected <- corrected[order(corrected$plate, corrected$position), ]
  if (any(grepl('failed to retrieve', redone$gene_type))) patch_annotation(corrected) else return(corrected)
}
