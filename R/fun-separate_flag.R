#' separate plate flags into well flags
#'
#' If a column in a screen object contains a string with a list of wells
#' of interest, convert it into a logical vector that flags individual wells.
#'
#' During the screening campaign hardware or software errors may occur that
#' could potentially affect individual wells or whole plates. This should be kept in mind
#' and will typically be noted in the screen log file. If a problem is persistent,
#' it is prudent to create a dedicated column to keep track of affected wells.
#'
#' Since the screen log is based on plates and flags may concern individual wells,
#' all problematic wells in a plate will likely be enumerated such that upon reading
#' the log file, they will be contained in a single string, e.g. "A1, H17".
#'
#' In order to improve legibility of the screen report these comments, rather than
#' be repeated as is for all wells, they will be converted into a logical flag,
#' where the noted wells will get a \code{TRUE} value and the others will get a \code{FALSE}.
#'
#' Comments that read "all" or "whole plate" will flag all wells in that plate.
#'
#' @section Well identifiers:
#' A well can be identified in two ways: by a number from 1 through the number of wells in a plate
#' or by its row/column coordinates, e.g. "A3".
#' ScanR automatically assigns the former into a column called "Index".
#' This is renamed to "well" by \code{build_screen}.
#' ScanR also puts the latter into a column called "Description" but I recommend
#' to disregard it as it may cause problems if the export file is defined by groups
#' rather than wells.
#' Well coordinates can be introduced by the layout file, typically in a single
#' column called "position" or in two columns called "row" and "column", or both.
#'
#' @section Caution:
#' It is crucial that notes in a flagging column are kept consistent,
#' i.e. use either the well number or position. Also, when using position, pay attention
#' to whether you use a strict 3-character format or a loose one, that is whether you
#' call an "A2" well "A2" or "A02". Keep this consistent between your files.
#'
#' @seealso \code{insert_zeros}
#'
#' @param scr a screen object, i.e. a \code{data.frame}
#' @param well.ID name of column with well identifier, given as character;
#'               see \code{Well identifiers}
#' @param flag name of column to transform, given as character
#' @param newname optional new name for the \code{flag} column, given as string
#' @param sep splitting terms for \code{flag} strings, passed to \code{strsplit}
#'
#' @return a \code{data frame} where the flag variable was converted from
#'         string to logical and optionally renamed
#'
#' @export
#'

separate_flag <- function(scr, flag = 'wells_rescanned', newname,
                          sep = ', ', well.ID = 'position') {
  #check arguments
  if (!is.data.frame(scr)) stop("\"scr\" must be a data.frame")
  if (!is.element(flag, colnames(scr))) stop('invalid column name in "flag"')
  if (!missing(newname)) {
    if (!is.character(newname) || length(newname) != 1)
      stop('"newname" must be a character string')
  }
  if (!is.character(sep)) stop('"sep" must be a character string')
  if (!is.element(well.ID, colnames(scr))) stop('invalid column name in "well.ID"')

  # isolate well.ID
  A <- scr[[well.ID]]
  # isolate flag and split the string
  B <- strsplit(scr[[flag]], split = sep)
  # compare the two
  C <- mapply(is.element, A, B)
  # recognize when the whole plate has been affected
  D <- ifelse(B[[flag]] == "all", TRUE, C)
  D <- ifelse(grepl("whole plate", B[[flag]]), TRUE, D)
  # replace original column in scr
  scr[[flag]] <- D

  # change column name if required or return
  if (missing(newname)) {
    return(scr)
  } else {
    colnames(scr)[grep(flag, colnames(scr))] <- newname
    return(scr)
  }
}
