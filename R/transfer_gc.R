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
#' \item{mz.mtx}{A matrix of mass/charge values.}
#' \item{mz.log10.p1.mtx}{A matrix of log10-transformed m/z values (log10(m/z + 1)).}
#' \item{normalized.by.row}{A matrix of normalized m/z values (by row, dividing by the maximum).}
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   result <- transfer.ms(dat, mass.pattern = "m[0-9]", time.pattern = "time_MS")
#' }
#' @export
transfer.ms <- function(dat
                        , mass.pattern = "m[0-9]"
                        , time.pattern = "time_MS"){
  
  mass.col <- grep(mass.pattern, colnames(dat), value = F)
  mass.abs <- grep(mass.pattern, colnames(dat), value = T)
  mass.abs <- as.numeric(gsub(gsub("\\[0\\-9\\]", "", mass.pattern), "", mass.abs))
  
  time.abs <- dat[ , get(time.pattern)]
  
  returnlist <- list()
  
  returnlist$time <- time.abs
  returnlist$mass <- mass.abs
  returnlist$mz <- dat[ , mass.col, with = F]
  returnlist$mz.mtx <- as.matrix(returnlist$mz)
  returnlist$mz.log10.p1.mtx <- log10(as.matrix(returnlist$mz.mtx)+1)
  returnlist$normalized.by.row <- t(apply(returnlist$mz.mtx, 1, function(x) x / max(x, na.rm = TRUE)))
  mz_mtx_norm <- t(apply(returnlist$mz.mtx, 1, function(x) x / max(x, na.rm = TRUE)))
  
  return(returnlist)
}

#' Transfer TCD Data
#'
#' This function merges Thermal Conductivity Detector (TCD) data from two sources 
#' when the time differences between them are minimal or zero, handling cases where 
#' the number of rows differs by performing a rolling join.
#'
#' @param tcd1A A data frame or data table containing TCD1A data with time and signal columns.
#' @param tcd2B A data frame or data table containing TCD2B data with time and signal columns.
#' @param colnames A character vector specifying the column names for the merged data. Default is `c("time", "TCD1A", "TCD2B")`.
#'
#' @return A merged data frame of TCD data containing time, TCD1A, and TCD2B values.
#' 
#' @examples
#' \dontrun{
#'   # Example usage:
#'   merged_data <- transfer.tcd(tcd1A, tcd2B, colnames = c("time", "TCD1A", "TCD2B"))
#' }
#' @export
transfer.tcd <- function(tcd1A = NA
                         , tcd2B = NA
                         , colnames = c("time", "TCD1A", "TCD2B")){
  
  # Merge TCD data when both have a time difference of 0 and the same number of rows
  if(nrow(tcd1A) == nrow(tcd2B)){
    
    if(unique(tcd1A$time_TCD1A - tcd2B$time_TCD2B) == 0){
      
      tcd.merge <- cbind(tcd1A, tcd2B$signal_TCD2B)
      colnames(tcd.merge) <- colnames
      return(tcd.merge)
      
    }
  }
  
  # Perform rolling join if rows differ
  if(nrow(tcd1A) != nrow(tcd2B)){
    
    setkey(tcd1A, time_TCD1A)
    setkey(tcd2B, time_TCD2B)
    
    # Perform a rolling join using the nearest time points
    tcd.merge <- tcd1A[tcd2B, roll = "nearest"]
    
    colnames(tcd.merge) <- colnames
    return(tcd.merge)
    
  }
}

