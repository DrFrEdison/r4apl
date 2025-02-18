#' Read GC Chromatogram and Detector Data
#'
#' This function reads Gas Chromatography (GC) chromatogram and Druck data from CSV files, based on the entries in the provided database.
#' It filters files by the specified sample ID and detector types and returns the raw data for each detector.
#'
#' @param database A data frame or list containing details about the GC data, including the directory paths (`dir`), file basenames (`basename`), and detector types (`detector`).
#'
#' @return A list where each element corresponds to a detector type, containing the raw data from the chromatogram files.
#' The data is organized by detector and filename.
#'
#' @examples
#' \dontrun{
#' # Assuming GC_MS$database is a data frame with the necessary fields
#' gc_data <- read_gc_chromatogram(database = GC_MS$database)
#' }
#'
#' @export
read_gc_chromatogram <- function(database
                                 , instrument
                                 , detector = c("MS", "TCD1A", "TCD2B", "SIM")) {

  # Check if 'dir' column exists in the database
  if (!"dir" %in% colnames(database)) stop("The database must contain a 'dir' column.")

  # Check if the instrument directory exists in 'wd'
  instrument <- gsub("\\-", "\\_", instrument)
  if (!instrument %in% names(wd$data$csv)) stop("Instrument not found in working directories.")

  # Define the path where chromatogram data is stored
  csv.path <- wd$data$csv[[ which( names(wd$data$csv) %in% instrument ) ]]

  # List all CSV files in the path, searching recursively
  dirp <- unique(database$dir)
  gc.files <- list()
  for(i in seq_along(dirp)){

    gc.files[[ i ]] <- dir(file.path(csv.path, dirp[ i ]), pattern = "*.csv$")
    gc.files[[ i ]] <- file.path(file.path(csv.path, dirp[ i ]), gc.files[[ i ]])

  }

  gc.files <- unlist(gc.files)

  # Filter the files based on the provided basenames in the database
  gc.files.detector <- list()
  for (i in seq_along(detector)) {

    gc.files.detector[[ i ]] <- gc.files[ which( unlist(lapply(gregexpr( paste0("_", detector[ i ], "_"), basename(gc.files)), function( x ) x[[ 1 ]])) > 0 ) ]

  }

  gc.files <- gc.files.detector
  gc.files <- unlist(gc.files)

  date.time.pattern <- paste0(gsub("\\-", "", substr(as.Date(database$datetime, tz = "UTC"), 3, 12))
                              , "_"
                              , gsub("\\:", "", as.character(strftime(database$datetime, format = "%H:%M:%S", tz = "UTC"))))

  gc.file.to.read <- list()
  for(i in 1 : nrow(database)){
    data_file <- gregexpr(database$data_file[ i ], basename(gc.files))
    date.time.match <- gregexpr(date.time.pattern[ i ], gc.files)

    gc.file.to.read[[ i ]] <- gc.files[ which( unlist( lapply(date.time.match, function( x ) x[[ 1 ]])) > 0 & unlist( lapply(data_file, function( x ) x[[ 1 ]])) > 0) ]
    gc.file.to.read[[ i ]] <- unique(gc.file.to.read[[ i ]])
  }

  ## Read the CSV files for each detector type
  gc.raw <- lapply(gc.file.to.read, function(files) {
    lapply(files, function(file) fread(file))
  })

  # Function to extract detector type from file path
  extract_detector <- function(file_path) {
    # Check for each detector type in the file path (in order of specificity)
    for (det in rev(gsub("GC-", "", detector))) {
      if (grepl(det, file_path)) {
        return(det)
      }
    }
    return(NA) # Return NA if no detector type is found
  }

  for(i in seq_along(gc.raw)) names(gc.raw[[ i ]]) <- as.character(unlist(sapply(basename(gc.file.to.read[[ i ]]), extract_detector)))

  # Name the raw data based on the file basenames
  for (i in seq_along(gc.raw)){

    if(!is.na(as.numeric(substr(database$data_file[ i ], 1, 1)))) names(gc.raw)[[i]] <- paste0("x", database$data_file[ i ]) else{

      names(gc.raw)[[i]] <- database$data_file[ i ]

    }
  }

  gc.raw <- gc.raw[ unlist(lapply(gc.raw, length)) > 0 ]
  return(gc.raw)
}
