#' add annotation to screen object
#'
#' Merge library annotation with screen object.
#'
#' The library annotation is merged with the screen object,
#' dropping all plates not present in the screen.
#' Missing gene symbols are substituted with their respective well types.
#'
#' @param scr a \code{data.frame} containing the screen object
#' @param ann path to library annotation file;
#'            if missing, defaults to internally stored annotation
#'
#' @return A \code{data.table}.
#'
#' @section Mismatching annotation:
#' If the built-in annotation cannot be merged with the screen object,
#' an error will be thrown and the annotation will be assigned in the
#' Global Environment. It can then be modified and saved in a different file.
#'
#' @export
#'

attach_annotation <- function(scr, ann) {
  if (!inherits(scr, 'data.frame')) stop('scr must be a data frame')
  if (!all(is.element(c("plate", "position"), names(scr))))
    stop("scr must contain \"plate\" and \"position\" columns")
  if (!missing(ann)) {
    if (!is.character(ann)) stop('ann must be a character string')
    ann <- data.table::fread(ann, check.names = TRUE)
    if (!all(is.element(c("plate", "position"), names(ann))))
      stop("ann must contain \"plate\" and \"position\" columns")
  } else {
    ann <- readRDS(
      system.file("extdata/ANNOTATION.LIBRARY.GENOMIC_20150416.original.plates.adjusted.rds",
                  package = "siscreenr"))
    data.table::setnames(ann,
                         old = c("Plate", "Well", "genesymbol"),
                         new = c("plate", "position", "gene_symbol"))
    ann$plate <- as.numeric(gsub("Plate ", "", ann$plate, fixed = TRUE))
  }
  if (is.factor(scr$plate)) scr$plate <- as.numeric(as.character(scr$plate))

  scr <- data.table::as.data.table(scr)
  # merge items
  ## check for common columns
  common_columns <- intersect(names(ann), names(scr))
  ## defensive
  if (length(common_columns) == 0L) {
    assign(".annotation", ann, pos = as.environment('package:siscreenr'))
    stop("scr and ann have no common columns", "\n",
         "scr: ", paste(names(scr), collapse = ", "), "\n",
         "ann: ", paste(names(ann), collapse = ", "), "\n",
         "current annotation has been assigned as .annotation",
         "terminating")
  }
  x <- merge(scr, ann, by = common_columns, all.x = TRUE, all.y = FALSE)
  # insert well types in place of missing gene_symbols (concerns mostly control wells)
  x$well_type <- as.character(x$well_type)
  x$gene_symbol <- ifelse(is.na(x$gene_symbol), x$well_type, x$gene_symbol)
  # arrange by plate, well and replica
  data.table::setorderv(x, c('plate', 'position', 'replica'))

  return(x)
}
