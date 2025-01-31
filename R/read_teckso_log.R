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
read_teckso_log <- function(logfile, header = FALSE, sep = "\n", col.names = "log_entry") {

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
  log_data$message <- gsub("\\[OV\\:PR\\]", "", log_data$message)
  log_data$message <- gsub("\\[ME:VPR\\]", "", log_data$message)

  # Reduce multiple spaces to a single space in the message
  for (i in 1:4) log_data$message <- gsub("\\s{2,}", " ", log_data$message)

  # No pressure ####
  nopressure_logs <- grep("mBar", log_data$message, invert = T)
  nopressure_datetime <- log_data$timestamp[nopressure_logs]
  nopressure_return <- data.frame(datetime = as.character(log_data$timestamp[ nopressure_logs ])
                                  , Status = log_data$message[ nopressure_logs ])


  # Which timeline is running?
  TLtext <- nopressure_return$Status[ max(grep("TL", nopressure_return$Status, ignore.case = F)) ]
  Timeline <- as.numeric( substr(TLtext, unlist(gregexpr("TL", TLtext)) + 2, unlist(gregexpr("TL", TLtext)) + 3) )

  # Position of the MPV1
  MPV1 <- nopressure_return$Status[ max(grep("MPVS 1", nopressure_return$Status)) ]
  MPV1 <- as.numeric(substr(MPV1, unlist(gregexpr("\\=GO", MPV1)) + 3, nchar(MPV1)))

  # Process logs for Pressure P1 and P2
  pressure_logs_P1 <- process_pressure_logs("P1", log_data)
  pressure_logs_P2 <- process_pressure_logs("P2", log_data)

  # Return the pressure logs for P1 and P2 as a list
  returnlist <- list(Pressure_P1 = pressure_logs_P1
                     , Pressure_P2 = pressure_logs_P2
                     , Status = nopressure_return
                     , MPV1 = MPV1
                     , Timeline = Timeline)

  return(returnlist)
}

read_teckso_log_fast <- function(logfile, header = FALSE, sep = "\n", col.names = "log_entry") {

  # Skip unnecessary reads
  total_lines <- length(readLines(logfile, warn = FALSE))
  skip_lines <- max(0, total_lines - 2500)

  # Load the last 2500 lines efficiently
  log_data <- fread(logfile, header = header, sep = sep, col.names = col.names, skip = skip_lines, nrows = 2500)

  log_data[, timestamp := sub("\\].*", "", log_entry)]
  log_data[, timestamp := sub("\\[", "", timestamp)]  # Remove `[`
  log_data[, elapsed_time := sub(".*\\((.*?)\\).*", "\\1", log_entry)]
  log_data[, message := sub(".*\\): ", "", log_entry)]

  # remove rows ####
  if(length(grep("OG:D", log_data$message)) > 0) log_data <- log_data[ - grep("OG:D", log_data$message) , ]

  # Convert timestamp to POSIXct and format to include milliseconds
  options(digits.secs = 3)
  log_data[, timestamp := as.POSIXct(timestamp, format = "%Y.%m.%d-%H:%M:%S")]

  # Clean up messages by removing specific tags efficiently
  log_data[, message := gsub("\\[TAG\\=TERMINAL\\,PRIO=MSG\\]|\\[OV\\:PR\\]", "", message)]

  # Reduce multiple spaces to a single space
  log_data[, message := gsub("\\s{2,}", " ", message)]

  # Identify non-pressure logs efficiently
  nopressure_logs <- log_data[!grepl("mBar", message)]
  nopressure_return <- nopressure_logs[, .(datetime = timestamp, Status = message)]

  # Identify Timeline from logs
  timeline_match <- nopressure_return[grepl("TL", Status), .SD[.N], by = .(Status)]$Status
  Timeline <- as.numeric(substr(timeline_match, regexpr("TL", timeline_match) + 2, regexpr("TL", timeline_match) + 3))
  Timeline <- Timeline[ length(Timeline) ]

  # Identify MPV1 Position
  MPV1_match <- nopressure_return[grepl("MPVS 1", Status), .SD[.N], by = .(Status)]$Status
  MPV1_match <- MPV1_match[ length(MPV1_match) ]
  MPV1 <- as.numeric(substr(MPV1_match, regexpr("GO", MPV1_match) + 2, nchar(MPV1_match)))

  # Process logs for Pressure P1 and P2
  pressure_logs_P1 <- process_pressure_logs("P1", log_data)
  pressure_logs_P2 <- process_pressure_logs("P2", log_data)

  # Return the processed data as a list
  return(list(
    Pressure_P1 = pressure_logs_P1,
    Pressure_P2 = pressure_logs_P2,
    Status = nopressure_return,
    MPV1 = MPV1,
    Timeline = Timeline
  ))
}

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

get_latest_file <- function(file_ending = "\\.log", file_pattern = "Terminal") {
  log_files <- list.files(pattern = file_ending)
  log_files <- grep(pattern = file_pattern, x = log_files, value = T)
  if (length(log_files) == 0) return(NULL)
  latest_file <- log_files[which.max(file.info(log_files)$mtime)]
  return(latest_file)
}
