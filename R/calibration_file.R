#' Load Calibration File for an Instrument
#'
#' This function retrieves and loads a calibration `.rds` file based on the specified instrument, calibration type, and date.
#'
#' @param instrument Character, the name of the instrument (e.g., "IC", "HPLC").
#' @param type Character, the type of calibration (e.g., "area", "height").
#' @param date Character, the date or part of the date to search for in the file names (e.g., "2024-07-01").
#' @param path Character, the root directory where calibration files are stored. Default is `wd$cal`.
#'
#' @return An R object (typically a list or model object) read from the `.rds` calibration file.
#'
#' @examples
#' \dontrun{
#' # Load a calibration file for IC instrument of type "area" from a specific date
#' calibration_data <- calibration_file(instrument = "IC", type = "area", date = "2024-07-01")
#' }
#'
#' @export
calibration_file <- function(instrument = instrument, 
                             type = type, 
                             date = datep, 
                             path = wd$cal) {
  
  # Search for the calibration directory based on the instrument
  path <- grep(instrument, path, value = TRUE)
  
  # Retrieve all .rds files in the directory
  cal.file <- dir(pattern = "\\.rds$", path = path)
  
  # Filter files based on the specified type and date
  cal.file <- grep(type, cal.file, value = TRUE)
  cal.file <- grep(date, cal.file, value = TRUE)
  
  # Check if calibration file was found
  if (length(cal.file) == 0) stop("No calibration file found for the given parameters.")
  
  # Load and return the calibration data
  return(readRDS(file.path(path, cal.file)))
}


#' Generate a Calibration Matrix from a Calibration File
#'
#' This function generates a calibration matrix from a list of calibration models. The matrix is constructed from the model data of the calibration files.
#'
#' @param cal.file A list of calibration files (models) loaded from `.rds` files. Each element of the list should contain a model component.
#'
#' @return A matrix where each row corresponds to a data point and each column represents a different calibration model.
#'
#' @examples
#' \dontrun{
#' # Generate a calibration matrix from a list of calibration models
#' calibration_data <- calibration_file(instrument = "IC", type = "area", date = "2024-07-01")
#' cal_matrix <- calibration_matrix(cal.file = calibration_data)
#' }
#'
#' @export
calibration_matrix <- function(cal.file = cal.file) {
  
  # Extract the model data from each calibration file and combine them into a matrix
  cal.model <- do.call(cbind, lapply(cal.file, function(x) x$model))
  
  # Create a matrix of row indices matching the number of models
  cal.matrix <- matrix(rep(1:nrow(cal.model), ncol(cal.model) / 2), 
                       nrow = nrow(cal.model), 
                       ncol = ncol(cal.model) / 2)
  
  # Assign column names from the list of calibration files
  colnames(cal.matrix) <- names(cal.file)
  
  return(cal.matrix)
}

