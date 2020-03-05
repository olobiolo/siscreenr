#' add annotation to screen object
#'
#' Merge library annotation with screen object.
#'
#' The library annotation is merged with the screen object,
#' dropping all plates not present in the screen.
#' Missing gene symbols are substituted with their respective well types.
#'
#' @param scr a \code{data.frame} containing the screen object
#' @param ann path to library annotation file
#'
#' @return a \code{data.table}
#'
#' @export
#'

attach_annotation <- function(scr, ann) {
  if (!is.data.frame(scr)) stop('scr must be a data frame')
  if (!all(is.element(c("plate", "position"), names(scr))))
    stop("scr must contain \"plate\" and \"position\" columns")
  if (!is.character(ann)) stop('ann must be a character string')
  ann <- data.table::fread(ann, check.names = TRUE)
  if (!all(is.element(c("plate", "position"), names(ann))))
    stop("ann must contain \"plate\" and \"position\" columns")
  if (is.factor(scr$plate)) scr$plate <- as.numeric(as.character(scr$plate))

  # merge items
  x <- merge(scr, ann, all.x = TRUE, all.y = FALSE)
  # insert well types in place of missing gene_symbols (concerns mostly control wells)
  x$well_type <- as.character(x$well_type)
  x$gene_symbol <- ifelse(is.na(x$gene_symbol), x$well_type, x$gene_symbol)
  # arrange by plate, well and replica
  setorderv(d, c('plate', 'position', 'replica'))
  #x <- x[order(x$plate, x$position, x$replica), ]

  return(x)
}
