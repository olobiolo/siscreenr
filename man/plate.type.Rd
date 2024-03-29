% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun-plate.type.R
\name{plate.type}
\alias{plate.type}
\alias{plate.type.converter}
\alias{plate.type.conversion}
\alias{plate.type.converter.key}
\alias{plate_type}
\alias{plate_type_conversion}
\alias{show_conversion_key}
\alias{edit_conversion_key}
\alias{recover_conversion_key}
\title{plate type conversion}
\usage{
plate.type.converter(x)

show_conversion_key()

edit_conversion_key(key)

recover_conversion_key()
}
\arguments{
\item{x}{a screen object}

\item{key}{a data frame with all character columns}
}
\description{
Manage the translation of plate type codes during building a screen object.
}
\details{
During building a screen object \code{build_screen} calls the internal function
\code{plate.type.converter} to read the plate type
encoded in field 4 of the plate name and create the columns
\code{plate_type} and \code{replica} accordingly.
This is done according to a dictionary file stored in the package directory.
}
\section{Functions}{
\itemize{
\item \code{plate.type.converter}: Takes a screen object and returns one in which columns "plate_type" and "replica" are altered.

\item \code{show_conversion_key}: Reads and prints the conversion key as a data frame with all character columns

\item \code{edit_conversion_key}: The new key will replace the file in the package directory.
When this function is run, it creates a backup copy of the original key.
The key can be recovered with \code{recover_conversion_key}.

\item \code{recover_conversion_key}: When the file that contains the key for plate type conversion is modified
by \code{edit_conversion_key}, a backup copy is created in the package directory.
This function will remove the current key file and restore the original.
}}

\keyword{internal}
