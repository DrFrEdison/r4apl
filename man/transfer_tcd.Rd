% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transfer_TCD.R
\name{transfer_tcd}
\alias{transfer_tcd}
\title{Transfer TCD Data}
\usage{
transfer_tcd(tcd1A = NA, tcd2B, colnames = c("time", "TCD1A", "TCD2B"))
}
\arguments{
\item{tcd1A}{A data frame or data table containing TCD1A data with time and signal columns. Default is \code{NA}.}

\item{tcd2B}{A data frame or data table containing TCD2B data with time and signal columns.}

\item{colnames}{A character vector specifying the column names for the merged data. Default is \code{c("time", "TCD1A", "TCD2B")}.}
}
\value{
A merged data frame or data table of TCD data containing time, TCD1A, and TCD2B values.
If \code{tcd1A} is \code{NA}, only \code{tcd2B} is returned.
}
\description{
This function merges Thermal Conductivity Detector (TCD) data from two sources,
handling cases where one source is \code{NA}, or the time differences between them
are minimal or zero. It also supports rolling joins when the number of rows differs.
}
\examples{
\dontrun{
  # Example usage:
  merged_data <- transfer_tcd(tcd1A, tcd2B, colnames = c("time", "TCD1A", "TCD2B"))
}
}
