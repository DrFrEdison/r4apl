#' Read and Process Teckso Log File
#'
#' This function reads a Teckso log file, extracts relevant information such as timestamps, elapsed time, and messages,
#' and specifically processes pressure logs for P1 and P2 (in mBar). It returns pressure readings along with their corresponding timestamps.
#'
#' @param logfile A string representing the path to the log file.
#' @param header Logical, indicating whether the file contains a header. Default is FALSE.
#' @param sep A string representing the separator used in the log file. Default is newline (`"\n"`).
#' @param col.names A string or vector of strings representing column names for the log entries. Default is "log_entry".
#'
#' @return A list containing two data frames: one for pressure readings of P1 and one for pressure readings of P2.
#' Each data frame contains datetime, pressure, and unit columns.
#'
#' @examples
#' \dontrun{
#' log_data <- read.teckso.log(logfile = "path/to/teckso.log")
#' }
#'
#' @export
read.teckso.log <- function(logfile, header = FALSE, sep = "\n", col.names = "log_entry") {

  # Load the log file data
  log_data <- fread(logfile, header = header, sep = sep, col.names = col.names)

  # Extract the timestamp, elapsed time, and message from log entries
  log_data[, `:=`(
    timestamp = sub("\\[(.*?)\\].*", "\\1", log_entry),
    elapsed_time = sub(".*\\((.*?)\\).*", "\\1", log_entry),
    message = sub(".*\\): (.*)", "\\1", log_entry)
  )]

  # Convert timestamp to POSIXct and format to include milliseconds
  options(digits.secs = 3)
  log_data[, timestamp := as.POSIXct(timestamp, format = "%Y.%m.%d-%H:%M:%S")]

  # Clean up messages by removing specific tags
  log_data$message <- gsub("\\[TAG\\=TERMINAL\\,PRIO=MSG\\]", "", log_data$message)

  # Reduce multiple spaces to a single space in the message
  for (i in 1:4) log_data$message <- gsub("\\s{2,}", " ", log_data$message)

  # Function to process pressure logs for a given pressure type (P1 or P2)
  process_pressure_logs <- function(pressure_type, log_data) {
    pressure_logs <- grep("mBar", log_data$message)
    pressure_logs <- pressure_logs[grep(pressure_type, log_data$message[pressure_logs])]
    pressure_grep <- gregexpr("mBar", log_data$message[pressure_logs])

    # Extract pressure values and units
    pressure_values <- substr(log_data$message[pressure_logs], unlist(pressure_grep) - 7, unlist(pressure_grep) + 3)
    pressure_units <- gsub("\\s", "", substr(pressure_values, 7, nchar(pressure_values)))
    pressure_values <- as.numeric(substr(pressure_values, 1, 6))

    # Extract the corresponding timestamps
    pressure_timestamps <- log_data$timestamp[pressure_logs]

    return(data.frame(datetime = pressure_timestamps, pressure = pressure_values, unit = pressure_units))
  }

  # Process logs for Pressure P1 and P2
  pressure_logs_P1 <- process_pressure_logs("P1", log_data)
  pressure_logs_P2 <- process_pressure_logs("P2", log_data)

  # Return the pressure logs for P1 and P2 as a list
  returnlist <- list(Pressure_P1 = pressure_logs_P1, Pressure_P2 = pressure_logs_P2)

  return(returnlist)
}
