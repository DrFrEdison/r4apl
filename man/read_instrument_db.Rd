% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_instrument_database.R
\name{read_instrument_db}
\alias{read_instrument_db}
\title{Read Instrument Database and Filter Data}
\usage{
read_instrument_db(
  instrument = "GC-MS",
  date = NA,
  time = NA,
  subdate = "2024-07-01",
  pattern = "qxx-db"
)
}
\arguments{
\item{date}{Character, optional. A manual date (format: "YYYYMMDD") to filter the files. Default is NA.}

\item{time}{Character, optional. A manual time (format: "HHMM") to further filter the files by time. Default is NA.}

\item{subdate}{Character, optional. A date (format: "YYYY-MM-DD") to filter the data by the \code{datetime} field. Default is "2024-07-01".}

\item{dir}{Character vector, the directories where GC database files are stored. Default is \code{c(wd$GC_MS$data, wd$GC_FID_TCD$data)}.}
}
\value{
A \code{data.table} containing the filtered GC data.
}
\description{
This function reads Gas Chromatography (GC) database files from a specified directory, allows filtering by date and time, and optionally returns a subset of the data.
}
\examples{
\dontrun{
# Read GC database and filter by date and time
read_instrument_db(date = "20240701", time = "1200")
}

}
