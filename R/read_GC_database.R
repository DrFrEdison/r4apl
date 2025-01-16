#' Read GC Database and Filter Data
#'
#' This function reads Gas Chromatography (GC) database files from a specified directory, allows filtering by date and time, and optionally returns a subset of the data.
#'
#' @param dir Character vector, the directories where GC database files are stored. Default is `c(wd$GC_MS$data, wd$GC_FID_TCD$data)`.
#' @param date Character, optional. A manual date (format: "YYYYMMDD") to filter the files. Default is NA.
#' @param time Character, optional. A manual time (format: "HHMM") to further filter the files by time. Default is NA.
#' @param order Integer, optional. Specifies the order position of files from newest to oldest. Default is NA.
#' @param subdate Character, optional. A date (format: "YYYY-MM-DD") to filter the data by the `datetime` field. Default is "2024-07-01".
#' @param instrument Character, specifies the instrument type to filter directories, either "GC_MS" or "GC_FID_TCD". Default is "GC_MS".
#'
#' @return A `data.table` containing the filtered GC data.
#'
#' @examples
#' \dontrun{
#' # Read GC database and filter by date and time
#' read_gc_database(date = "20240701", time = "1200")
#' }
#'
#' @export
read_gc_database <- function(dir = c(wd$GC_MS$data, wd$GC_FID_TCD$data),
                                  date = NA,
                                  time = NA,
                                  order = NA,
                                  subdate = "2024-07-01",
                                  instrument = "GC_MS") {

  # Filter directories based on the instrument type
  dir <- dir[basename(dir) %in% instrument]

  # Ensure the directory exists
  if (length(dir) == 0) stop("No valid directory found for the instrument type.")

  # Set working directory to the filtered path
  setwd(dir)

  # Retrieve the list of GC database files
  files <- dir(pattern = "*database.csv$")
  if (length(files) == 0) stop("No GC database files found in the specified directory.")

  # Order files based on their creation time
  files.ctime <- file.info(files)$ctime
  files.order <- files[order(files.ctime)]

  # Filter files by date if provided
  if (!is.na(date)) {
    files <- files[substr(files, 1, 8) == date]
    if (length(files) == 0) stop("No files found for the specified date.")
    files.order <- files[order(file.info(files[substr(files, 1, 8) == date])$ctime)]
  }

  # Filter files by both date and time if provided
  if (!is.na(date) && !is.na(time)) {
    files.order <- files[substr(files, 1, 13) == paste0(date, "_", time)]
    if (length(files.order) == 0) stop("No files found for the specified date and time.")
  }

  # Filter by manual order if provided
  if (!is.na(order)) {
    files.order <- rev(files.order)[order]
    if (length(files.order) == 0) stop("No files found for the specified order.")
  }

  # Read the most recent file from the ordered list
  dat <- fread(files.order[length(files.order)], sep = ";", dec = ",", encoding = "Latin-1", na.strings = "")

  # Convert 'datetime' column to POSIXct and order by it
  if ("datetime" %in% colnames(dat)) {
    dat$datetime <- as.POSIXct(dat$datetime, tz = "UTC")
    dat <- dat[order(dat$datetime), ]
  } else {
    stop("No 'datetime' column found in the data.")
  }

  # Filter by 'subdate' if provided
  if (!is.na(subdate)) {
    dat <- dat[as.Date(dat$datetime) >= as.Date(subdate), ]
    if (nrow(dat) == 0) warning("No data found for the specified subdate.")
  }

  # Convert numeric-like columns to numeric
  numeric_columns <- dat[, lapply(.SD, function(col) all(grepl("^[0-9.]+$", col)))]
  numeric_columns <- names(dat)[as.logical(unlist(numeric_columns))]
  dat[, (numeric_columns) := lapply(.SD, as.numeric), .SDcols = numeric_columns]

  # Remove columns that are empty or contain only NA
  non_empty_cols <- sapply(dat, function(col) !all(is.na(col) | as.character(col) == ""))
  dat <- dat[, names(non_empty_cols), with = FALSE]

  # Convert certain columns to factors if they exist
  factor_cols <- c("dir", "sample.name", "detector", "data.file", "method")
  factor_cols <- intersect(factor_cols, colnames(dat))
  dat[, (factor_cols) := lapply(.SD, factor), .SDcols = factor_cols]

  return(dat)
}
