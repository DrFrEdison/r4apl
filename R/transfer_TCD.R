#' Transfer TCD Data
#'
#' This function merges Thermal Conductivity Detector (TCD) data from two sources,
#' handling cases where one source is `NA`, or the time differences between them
#' are minimal or zero. It also supports rolling joins when the number of rows differs.
#'
#' @param tcd1A A data frame or data table containing TCD1A data with time and signal columns. Default is `NA`.
#' @param tcd2B A data frame or data table containing TCD2B data with time and signal columns.
#' @param colnames A character vector specifying the column names for the merged data. Default is `c("time", "TCD1A", "TCD2B")`.
#'
#' @return A merged data frame or data table of TCD data containing time, TCD1A, and TCD2B values.
#' If `tcd1A` is `NA`, only `tcd2B` is returned.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   merged_data <- transfer_tcd(tcd1A, tcd2B, colnames = c("time", "TCD1A", "TCD2B"))
#' }
#' @export
transfer_tcd <- function(tcd1A = NA, tcd2B, colnames = c("time", "TCD1A", "TCD2B")) {

  # If tcd1A is NA, return tcd2B with appropriate column names
  if (all(is.na(tcd1A))) {
    warning("TCD1A data is missing. Returning only TCD2B data.")
    colnames(tcd2B) <- c(colnames[1], colnames[3])  # Set appropriate column names for TCD2B
    return(tcd2B)
  }

  # Check if both datasets have time and signal columns
  if (!any(grepl("time", colnames(tcd1A))) || !any(grepl("signal", colnames(tcd1A)))) {
    stop("tcd1A does not contain required 'time' and 'signal' columns.")
  }
  if (!any(grepl("time", colnames(tcd2B))) || !any(grepl("signal", colnames(tcd2B)))) {
    stop("tcd2B does not contain required 'time' and 'signal' columns.")
  }

  # Extract time and signal columns from both datasets
  time1A <- tcd1A[, get(grep("time", colnames(tcd1A), value = TRUE))]
  time2B <- tcd2B[, get(grep("time", colnames(tcd2B), value = TRUE))]
  signal2B <- tcd2B[, get(grep("signal", colnames(tcd2B), value = TRUE))]

  # If tcd1A and tcd2B have the same number of rows and times match exactly
  if (nrow(tcd1A) == nrow(tcd2B) && all(time1A == time2B)) {
    tcd.merge <- cbind(tcd1A, signal_TCD2B = signal2B)
    colnames(tcd.merge) <- colnames
    return(tcd.merge)
  }

  # If the number of rows differs or times don't match exactly, perform rolling join
  if (nrow(tcd1A) != nrow(tcd2B)) {
    setkeyv(tcd1A, grep("time", colnames(tcd1A), value = TRUE))
    setkeyv(tcd2B, grep("time", colnames(tcd2B), value = TRUE))

    # Perform a rolling join using the nearest time points
    tcd.merge <- tcd1A[tcd2B, roll = "nearest"]
    colnames(tcd.merge) <- colnames
    return(tcd.merge)
  }

  # If we reach here, it means no matching condition was met
  stop("Unable to merge TCD data: time columns or row numbers do not match.")
}


