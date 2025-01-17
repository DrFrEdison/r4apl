#' Read IC Chromatogram and Druck Data
#'
#' This function reads Ion Chromatography (IC) chromatogram and Druck data from a text file, based on the specified sample ID.
#'
#' @param ID Character, the sample ID to search for in the text files.
#'
#' @return A list containing the datetime, identification (Ident), sample ID, and optionally the Druck and Chromatogram data if found.
#'
#' @examples
#' \dontrun{
#' read_ic_chromatogram(ID = "20240701_1234")
#' }
#'
#' @export
read_ic_chromatogram <- function(ID) {

  # Define file paths and search for the correct file
  txt_dir <- wd$hw$Datensicherung$IC$txt
  txt_files <- dir(txt_dir, pattern = "*.txt$")

  # Find the matching file based on the sample ID
  txt_sub <- substr(txt_files, 1, nchar(ID))
  txt_found_file <- txt_files[txt_sub %in% gsub("\\:", "\\#", ID)]
  txt_found_file <- txt_found_file[length(txt_found_file)]

  # If no file is found, return an error
  if (is.na(txt_found_file)) {
    stop("No matching file found for the provided ID.")
  }

  # Read the file
  raw_data <- fread(file.path(txt_dir, txt_found_file), header = FALSE, encoding = "Latin-1", fill = TRUE, sep = ";")

  # Extract datetime, Ident, and sample ID from the file
  datetime <- as.POSIXct(substr(as.character(raw_data[1, ]), 1, 19), tz = "UTC")
  Ident <- raw_data[2, ]
  sample_ID <- raw_data[3, ]

  # Check if the sample ID matches the provided ID
  if (sample_ID != ID) {
    warning("The sample ID in the txt file does not match the provided ID.")
  }

  # Process the chromatogram data
  if (any(raw_data$V1 %in% "Anionen")) {
    Chromatogram <- process_section_ic(raw_data, "Anionen", "Druck")
  } else {
    warning("Anionen chromatogram not found in the txt file.")
  }

  # Process the Druck data
  if (any(raw_data$V1 %in% "Druck")) {
    Druck <- process_section_ic(raw_data, "Druck", end_marker = NULL)
  } else {
    warning("Druck data not found in the txt file.")
  }

  # Prepare the return list
  return_list <- list(datetime = datetime, Ident = Ident, sample_ID = sample_ID)
  if (exists("Chromatogram")) return_list$Chromatogram <- Chromatogram
  if (exists("Druck")) return_list$Druck <- Druck

  # Return the list, removing any NULL elements
  return_list <- return_list[!unlist(lapply(return_list, is.null))]
  return(return_list)
}

#' Helper Function to Process Sections in the IC File
#'
#' This helper function processes sections of the IC file (e.g., Chromatogram or Druck) based on the given section name.
#'
#' @param raw_data The raw data read from the txt file.
#' @param section_name The section name to process (e.g., "Anionen", "Druck").
#' @param end_marker The marker indicating the end of the section. Default is NULL (process until the end).
#'
#' @return A data.table containing the processed section.
process_section_ic <- function(raw_data, section_name, end_marker = NULL) {
  # Find the start of the section
  section_start <- which(raw_data$V1 %in% section_name) + 1

  # Find the end of the section if there's an end marker
  if (!is.null(end_marker)) {
    section_end <- which(raw_data$V1 %in% end_marker) - 1
  } else {
    section_end <- nrow(raw_data)
  }

  # Extract the section data
  section_data <- raw_data[section_start:section_end, ]
  column_names <- unlist(strsplit(as.character(section_data$V1[2]), ";"))
  section_data <- section_data[-c(1, 2), ]

  # Split the data into columns and assign names
  section_data <- section_data[, tstrsplit(V1, ";", type.convert = TRUE)]
  setnames(section_data, column_names)

  # Remove rows with NA values
  section_data <- section_data[complete.cases(section_data), ]

  return(section_data)
}
