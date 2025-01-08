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
#' gc_data <- find.read.GC.chromatogram(database = GC_MS$database)
#' }
#'
#' @export
find.read.GC.chromatogram <- function(database = GC_MS$database
                                      , instrument
                                      , wd = wd) {

  # Check if 'dir' column exists in the database
  if (!"dir" %in% colnames(database)) stop("The database must contain a 'dir' column.")

  # Check if the instrument directory exists in 'wd'
  if (!instrument %in% names(wd)) stop("Instrument not found in working directories.")

  # Define the path where chromatogram data is stored
  csv.path <- wd[[instrument]]$data

  # List all CSV files in the path, searching recursively
  gc.files <- dir(path = csv.path, pattern = "*.csv$", recursive = TRUE)

  # Filter the files based on the provided basenames in the database
  gc.files <- gc.files[basename(gc.files) %in% database$basename]

  if (length(gc.files) == 0) stop("No matching GC files found based on the provided database.")

  # Get the unique detector types from the database
  detector <- unique(database$detector)

  # Initialize an empty list to store files by detector type
  gc.files.detector <- list()

  # Loop through each detector and find corresponding files
  for (i in seq_along(detector)) {
    gc.files.detector[[i]] <- gc.files[grep(paste0("_", detector[i], "_"), basename(gc.files))]
  }

  # Set the names of the list to the detector types
  names(gc.files.detector) <- detector

  # Read the CSV files for each detector type
  gc.raw <- lapply(gc.files.detector, function(files) {
    lapply(files, function(file) fread(file.path(csv.path, file)))
  })

  # Name the raw data based on the file basenames
  for (i in seq_along(gc.raw)) names(gc.raw[[i]]) <- basename(gc.files.detector[[i]])

  return(gc.raw)
}
