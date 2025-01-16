#' Read IC Database and Filter Data
#'
#' This function reads Ion Chromatography (IC) database files from a specified directory, allows filtering by date and time, and optionally returns a subset of the data.
#'
#' @param dir Character, the directory where IC database files are stored. Default is `wd$team_labor$datensicherung$IC`.
#' @param date Character, optional. A manual date (format: "YYYYMMDD") to filter the files. Default is NA.
#' @param time Character, optional. A manual time (format: "HHMM") to filter the files. Default is NA.
#' @param order Integer, optional. Specifies the order position of files from newest to oldest. Default is NA.
#' @param subdat Logical, whether to return a smaller subset of the data. Default is TRUE.
#' @param subdate Character, a date (format: "YYYY-MM-DD") to filter the data by `Bestimmungsstart`. Default is "2024-07-01".
#'
#' @return A data.table containing the filtered IC data.
#'
#' @examples
#' \dontrun{
#' read_ic_database(date = "20240701", time = "1200")
#' }
#'
#' @export
read_ic_database <- function(dir = wd$hw$Datensicherung$IC$root,
                                  date = NA,
                                  order = NA,
                                  subdat = TRUE,
                                  subdate = "2024-07-01") {

  # Set working directory to IC csv database folder
  setwd(file.path(dir, "csv_Datenbank"))

  # Retrieve the list of IC database files
  files <- dir(pattern = "*ic_database.csv$")
  files.ctime <- file.info(files)$ctime
  files.order <- files[order(file.info(files)$ctime)]

  # Filter files by date if provided
  if (!is.na(date)) {
    files <- files[substr(files, 1, 8) == date]
    files.order <- files[order(file.info(files[substr(files, 1, 8) == date])$ctime)]
    if (length(files) == 0) warning("No files found for the specified date.")
  }

  # Filter by manual order if specified
  if (!is.na(order)) files.order <- rev(files.order)[order]

  # Read the most recent file from the ordered list
  dat <- fread(files.order[length(files.order)], sep = ";", dec = ",", encoding = "Latin-1", na.strings = "")

  # Convert 'Bestimmungsstart' column to POSIXct and order by it
  dat$Bestimmungsstart <- as.POSIXct(dat$Bestimmungsstart, tz = "UTC")
  dat <- dat[order(dat$Bestimmungsstart), ]

  # Clean up column names by removing unnecessary prefixes
  colnames(dat) <- gsub("Anionen\\.|AHWP\\.", "", colnames(dat))

  # Subset the data if 'subdat' is TRUE
  if (subdat) {
    subcol <- c("Bestimmungsstart", "Ident", "Probentyp", "Methodenname", "Verdünnung",
                "Bestimmungs-ID", "Bestimmungsdauer [min]", grep("Info", colnames(dat), value = TRUE)[1],
                grep("Konzentration", colnames(dat), value = TRUE), grep("Kalibrierpunkte", colnames(dat), value = TRUE))

    subcol <- grep("mittelwert|anteil", subcol, invert = TRUE, value = TRUE)

    missing_cols <- subcol[!subcol %in% colnames(dat)]
    if (length(missing_cols) > 0) {
      warning(paste0("Columns not found: ", paste(missing_cols, collapse = ", ")))
      subcol <- subcol[subcol %in% colnames(dat)]
    }

    dat <- dat[, ..subcol]
  }

  # Filter data by 'subdate' if provided
  if (!is.na(subdate)) dat <- dat[as.Date(dat$Bestimmungsstart) >= as.Date(subdate), ]

  # Convert numeric-like columns to numeric
  numeric_columns <- dat[, lapply(.SD, function(col) all(grepl("^[0-9.]+$", col)))]
  numeric_columns <- names(dat)[as.logical(unlist(numeric_columns))]
  dat[, (numeric_columns) := lapply(.SD, as.numeric), .SDcols = numeric_columns]

  # Remove columns that are empty or contain only NA
  non_empty_cols <- sapply(dat, function(col) !all(is.na(col) | as.character(col) == ""))
  dat <- dat[, names(non_empty_cols), with = FALSE]

  # Convert certain columns to factors
  if ("Probentyp" %in% colnames(dat)) dat$Probentyp <- factor(dat$Probentyp)
  if ("Methodenname" %in% colnames(dat)) dat$Methodenname <- factor(dat$Methodenname)

  return(dat)
}
