#' Transfer Mass Spectrometry Data
#'
#' This function processes Mass Spectrometry (MS) data, extracting mass and time values,
#' and generating several transformations of the MS data, including normalization.
#'
#' @param dat A data frame or data table containing MS data with columns for mass and time.
#' @param mass.pattern A regular expression pattern to identify mass-related columns in `dat`. Default is `"m[0-9]"`.
#' @param time.pattern A string to identify the time-related column in `dat`. Default is `"time_MS"`.
#'
#' @return A list containing:
#' \item{time}{Time values extracted from the data.}
#' \item{mass}{Mass values extracted and processed from the data.}
#' \item{mz}{The mass/charge (m/z) ratios from the mass-related columns.}
#' \item{mz.log10}{A matrix of log10-transformed m/z values (log10(m/z + 1)).}
#' \item{normalized.by.row}{A matrix of normalized m/z values (by row, dividing by the maximum).}
#' \item{Counts}{Total Ionic Counts.}
#' \item{Counts50}{Total Ionic Counts for masses >= 50.}
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   result <- transfer_ms(dat, mass.pattern = "m[0-9]", time.pattern = "time_MS")
#' }
#' @export
transfer_ms <- function(dat
                        , mass.pattern = "m[0-9]"
                        , time.pattern = "RT"){

  mass.col <- grep(mass.pattern, colnames(dat), value = F)
  mass.abs <- grep(mass.pattern, colnames(dat), value = T)
  mass.abs <- as.numeric(gsub(gsub("\\[0\\-9\\]", "", mass.pattern), "", mass.abs))

  time.abs <- dat[ , get(time.pattern)]

  returnlist <- list()

  returnlist$time <- time.abs
  returnlist$mass <- mass.abs
  returnlist$mz <- as.matrix( dat[ , mass.col, with = F] )
  # returnlist$mz.log10 <- log10(as.matrix(returnlist$mz)+1)
  # returnlist$normalized.by.row <- t(apply(returnlist$mz, 1, function(x) x / max(x, na.rm = TRUE)))

  # returnlist$Counts <- apply(returnlist$mz, 1, sum)
  # returnlist$Counts <- returnlist$Counts / max(returnlist$Counts)

  # returnlist$Counts50 <- apply(returnlist$mz, 1, function( x ) sum(x[ returnlist$mass >= 50] ))
  # returnlist$Counts50 <- returnlist$Counts50 / max(returnlist$Counts50)

  return(returnlist)
}
