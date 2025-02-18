#' Transfer TCD Data
#'
#' This function merges Thermal Conductivity Detector (TCD) data from two sources,
#' handling cases where one source is `NA`, or the RT differences between them
#' are minimal or zero. It also supports rolling joins when the number of rows differs.
#'
#' @param TCD1A A data frame or data table containing TCD1A data with RT and TCD columns. Default is `NA`.
#' @param TCD2B A data frame or data table containing TCD2B data with RT and TCD columns.
#' @param colnames A character vector specifying the column names for the merged data. Default is `c("RT", "TCD1A", "TCD2B")`.
#'
#' @return A merged data frame or data table of TCD data containing RT, TCD1A, and TCD2B values.
#' If `TCD1A` is `NA`, only `TCD2B` is returned.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   merged_data <- transfer_tcd(TCD1A, TCD2B, colnames = c("RT", "TCD1A", "TCD2B"))
#' }
#' @export
transfer_tcd <- function(TCD1A = NA, TCD2B, colnames = c("RT", "TCD1A", "TCD2B")) {

  # If TCD1A is NA, return TCD2B with appropriate column names
  if (all(is.na(TCD1A))) {
    warning("TCD1A data is missing. Returning only TCD2B data.")
    colnames(TCD2B) <- c(colnames[1], colnames[3])  # Set appropriate column names for TCD2B
    return(TCD2B)
  }

  # Check if both datasets have RT and TCD columns
  if (!any(grepl("RT", colnames(TCD1A))) || !any(grepl("TCD", colnames(TCD1A)))) {
    stop("TCD1A does not contain required 'RT' and 'TCD' columns.")
  }
  if (!any(grepl("RT", colnames(TCD2B))) || !any(grepl("TCD", colnames(TCD2B)))) {
    stop("TCD2B does not contain required 'RT' and 'TCD' columns.")
  }

  # Extract RT and TCD columns from both datasets
  RT1A <- TCD1A[, get("RT")]
  RT2B <- TCD2B[, get("RT")]
  signal2B <- TCD2B[, get("TCD2B")]

  # If TCD1A and TCD2B have the same number of rows and RTs match exactly
  if (nrow(TCD1A) == nrow(TCD2B) && all(RT1A == RT2B)) {
    tcd.merge <- cbind(TCD1A, TCD2B = signal2B)
    colnames(tcd.merge) <- colnames
    return(tcd.merge)
  }

  # If the number of rows differs or RTs don't match exactly, perform rolling join
  if (nrow(TCD1A) != nrow(TCD2B)) {
    setkeyv(TCD1A, grep("RT", colnames(TCD1A), value = TRUE))
    setkeyv(TCD2B, grep("RT", colnames(TCD2B), value = TRUE))

    # Perform a rolling join using the nearest RT points
    tcd.merge <- TCD1A[TCD2B, roll = "nearest"]
    colnames(tcd.merge) <- colnames
    return(tcd.merge)
  }

  # If we reach here, it means no matching condition was met
  stop("Unable to merge TCD data: RT columns or row numbers do not match.")
}


