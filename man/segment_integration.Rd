% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/integration.R
\name{segment_integration}
\alias{segment_integration}
\title{Segment-wise Chromatographic Peak Integration}
\usage{
segment_integration(
  RT,
  Intensity,
  segment,
  findpeak,
  reprocess = NULL,
  addpeak = NULL,
  removepeak = NULL,
  name,
  ID,
  png = T
)
}
\arguments{
\item{RT}{Retention time vector}

\item{Intensity}{Intensity vector}

\item{segment}{Numeric vector defining segment boundaries}

\item{findpeak}{Data.frame with parameters per segment}

\item{reprocess}{Optional data.frame with peaks to reprocess}

\item{addpeak}{Optional data.frame with peaks to manually add}

\item{removepeak}{Optional data.frame with peaks to manually remove}

\item{name}{Sample name for plot title}

\item{ID}{Sample ID for filename}

\item{png}{Logical, if TRUE saves plots to PNG}
}
\value{
Data.frame of integrated peaks
}
\description{
Integrates chromatographic data over defined RT segments with baseline correction, optional peak addition and reprocessing.
}
