% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun-build_screen.R
\name{build_screen}
\alias{build_screen}
\title{build screen object}
\usage{
build_screen(
  logfile,
  layout,
  datadir = "./data/",
  rem.col,
  zero.to.NA = FALSE,
  wells = "Index",
  verbose = TRUE
)
}
\arguments{
\item{logfile}{path to screen log file; see \code{Log file}}

\item{layout}{path to layout file that describes well types; see \code{Layout file}}

\item{datadir}{path to directory where data files are stored}

\item{rem.col}{columns to remove, given either as character vector or as vector of numeric indices;
input 0 to remove last column;}

\item{zero.to.NA}{logical flag whether to convert all 0 values to NAs}

\item{wells}{name of column that stores well numbers; will be renamed to "wells"}

\item{verbose}{logical flag whether to print all communications}
}
\value{
an invisible \code{data.frame}
}
\description{
Read screen log and layout, and collate all data into a single object.
This is the first stage of the analysis. Restrictions on log/layout files are discussed here.
The function tries to cope with some cases and also runs checks on data completion.
There are many communications that the function will normally print; these can be silenced.
The final object is returned invisibly as printing it all would be counterproductive.
}
\details{
The function first checks the data files present in the data directory against
the plates logged in the screen log.
Data file names must begin with the full plate names as given in the screen log.
Suffixes are allowed but must be separated by \code{_}. They will be discarded.
Unexpected and missing plates are reported.

Once the plate list has been compiled, the layout file is loaded and all present
data files that correspond to logged plates are read and wrapped into a single data frame.

The plate number and screen log data are used to obtain dates of the plates'
preparation, plating, and imaging.

There is quite a lot of printed communication the function does, hence the \code{verbose} argument.
}
\section{Log file}{

The screen log this must be a tab delimited file and contain columns:
\code{plateno}, \code{plated}, \code{imaged}. Any number of other columns is acceptable.
The plate number must be contained verbatim within the corresponding result file name.
Any suffixes following an underscore will be dropped.
}

\section{Layout file}{

The layout file must be a tab delimited file and contain columns:
(numeric) \code{well} and \code{well.type} or \code{well_type}.
Other columns may be necessary if there are multiple layouts,
e.g. for different plating days or different plate types.
Maintain a \href{https://en.wikipedia.org/wiki/Wide_and_narrow_data}{narrow data format}.

The file should also specify a well's coordinates on a plate, either as
\code{position}, e.g. "E02", or \code{row} and \code{column}.
By default ScanR sorts and numbers wells by row,
in contrast to the default filling of matrices in R.
Thus, \code{\link{normalize}} sorts the data frame by column and row before
running the median polish and if these columns are absent it throws an error.
}

\section{Dropping columns}{

Columns are removed upon request. This option exists for two reasons.
First, ScanR always adds an empty column at the end called "X". Second, the
second column in every exported file,
called "Description", contains the names of groups of wells.
Since we only ever export data by well and not by group,
this may cause problems if not all wells are scanned.
Also we load layout and annotation separately, the column is redundant anyway.
}

\section{Dependencies}{

Data files are loaded with \code{read.delim}.
Dates are handled with \code{lubridate}.
A number of internal functions are used.
}

