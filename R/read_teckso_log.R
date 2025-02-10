library(data.table)

#' Read and Process Multiple Teckso Log Files
#'
#' This function reads multiple Teckso log files, extracts timestamps, elapsed time, messages,
#' and processes pressure logs for P1 and P2 (in mBar). It returns structured pressure readings,
#' status logs, MPV positions, timelines, and computed values.
#'
#' @param logfile A vector of file paths to Teckso log files.
#' @param names A character vector specifying labels for each log file. Default is `c("DNPH", "Waschflaschen")`.
#' @param n Number of lines to read from each file (to limit memory usage). Default is `2500`. Can be '`NA`, then all lines are read
#'
#' @return A list containing:
#' - `Pressure_P1`: Pressure readings for P1 across all logs.
#' - `Pressure_P2`: Pressure readings for P2 across all logs.
#' - `Status`: Status messages for each log.
#' - `MPV1`: Most recent MPV1 position from logs.
#' - `Timeline`: The most recent active timeline.
#' - `Wert`: Computed values extracted from logs.
#'
#' @export
read_teckso_log <- function(logfile
                            , names = c("DNPH", "Waschflaschen")
                            , n = 2500) {
  # Ensure file paths are provided
  if (length(logfile) == 0) stop("No log files provided.")

  if (!is.na(n)) {
    # Determine the number of lines to skip
    total_lines <- lapply(logfile, function(x) length(readLines(x, warn = FALSE)))
    skip_lines <- lapply(total_lines, function(x) max(0, x - n))

    # Load only the last `n` lines for efficiency
    log_data <- mapply(function(x, y) fread(x, header = F, sep = "\n", col.names = "log_entry", skip = y, nrows = n),
                       x = logfile, y = skip_lines, SIMPLIFY = FALSE)
  } else {
    # Load entire file if `n` is NA
    log_data <- lapply(logfile, function(x) fread(x, header = F, sep = "\n", col.names = "log_entry"))
  }

  # Assign proper names
  names(log_data) <- names

  # Process timestamps and messages
  log_data <- lapply(log_data, function( x ) x[, timestamp := sub("\\].*", "", log_entry)])
  log_data <- lapply(log_data, function( x ) x[, timestamp := sub("\\[", "", timestamp)])  # Remove `[`
  log_data <- lapply(log_data, function( x ) x[, elapsed_time := sub(".*\\((.*?)\\).*", "\\1", log_entry)])
  log_data <- lapply(log_data, function( x ) x[, message := sub(".*\\): ", "", log_entry)])

  # Convert timestamp to POSIXct and format to include milliseconds
  options(digits.secs = 3)
  log_data <- lapply(log_data, function( x ) x[, timestamp := as.POSIXct(timestamp, format = "%Y.%m.%d-%H:%M:%S")])

  # Remove irrelevant messages
  log_data <- lapply(log_data, function(df) {
    patterns <- c("OG:D", "OV:PR", "MT:MET_RUN", "OT:Icon")
    for (p in patterns) df <- df[!grepl(p, message), ]
    df
  })

  # ii <- seq_along(log_data)
  # for(i in ii)  if(length(grep("OG:D", log_data[[ i ]]$message)) > 0) log_data[[ i ]] <- log_data[[ i ]][ - grep("OG:D", log_data[[ i ]]$message) , ]
  # for(i in ii)  if(length(grep("OV:PR", log_data[[ i ]]$message)) > 0) log_data[[ i ]] <- log_data[[ i ]][ - grep("OV:PR", log_data[[ i ]]$message) , ]
  # for(i in ii)  if(length(grep("MT:MET_RUN", log_data[[ i ]]$message)) > 0) log_data[[ i ]] <- log_data[[ i ]][ - grep("MT:MET_RUN", log_data[[ i ]]$message) , ]
  # for(i in ii)  if(length(grep("OT:Icon", log_data[[ i ]]$message)) > 0) log_data[[ i ]] <- log_data[[ i ]][ - grep("OT:Icon", log_data[[ i ]]$message) , ]
  #

  # Convert timestamp to POSIXct
  options(digits.secs = 3)
  log_data <- lapply(log_data, function(df) {
    df[, timestamp := as.POSIXct(timestamp, format = "%Y.%m.%d-%H:%M:%S")]
    df
  })

  # Clean messages
  log_data <- lapply(log_data, function(df) {
    df[, message := gsub("\\[TAG\\=TERMINAL\\,PRIO=MSG\\]|\\[OV\\:PR\\]", "", message)]
    df[, message := gsub("\\s{2,}", " ", message)]
    df
  })

  # Extract non-pressure logs
  nopressure_logs <- lapply(log_data, function(df) df[!grepl("mBar", message)])
  nopressure_return <- lapply(nopressure_logs, function(df) df[, .(datetime = timestamp, Status = message)])

  # Extract latest timeline
  Timeline <- lapply(nopressure_return, function(df) last(grep("TL", df$Status, value = TRUE)))

  # Extract latest MPV1 Position
  MPV1 <- lapply(nopressure_return, function(df) last(grep("MPVS 1=2", df$Status, value = TRUE)))
  MPV1 <- lapply(MPV1, function(val) as.numeric(substr(val, regexpr("GO", val) + 2, nchar(val))))

  # Extract latest Wert
  Wert <- lapply(nopressure_return, function(df) last(grep("Wert=", df$Status, value = TRUE)))
  Wert <- lapply(Wert, function(val) as.numeric(substr(val, regexpr("Wert=", val) + 5, nchar(val))) / 100)

  # Process pressure logs for P1 and P2
  pressure_logs_P1 <- lapply(log_data, function(df) process_pressure_logs("P1", df))
  pressure_logs_P2 <- lapply(log_data, function(df) process_pressure_logs("P2", df))

  # Return structured data
  return(list(
    Pressure_P1 = pressure_logs_P1,
    Pressure_P2 = pressure_logs_P2,
    Status = nopressure_return,
    MPV1 = MPV1,
    Timeline = Timeline,
    Wert = Wert
  ))
}

#' Process Pressure Logs
#'
#' Extracts pressure values and timestamps from log messages based on the specified pressure type (P1 or P2).
#'
#' @param pressure_type A string specifying which pressure type to extract (either "P1" or "P2").
#' @param log_data A data.table containing processed log data.
#'
#' @return A data frame containing `datetime`, `pressure`, and `unit` columns.
#'
#' @export
process_pressure_logs <- function(pressure_type, log_data) {
  pressure_logs <- grep("mBar", log_data$message)
  pressure_logs <- pressure_logs[grep(pressure_type, log_data$message[pressure_logs])]

  if (length(pressure_logs) == 0) return(data.frame(datetime = NA, pressure = NA, unit = NA))  # Return empty frame if no data

  pressure_grep <- gregexpr("mBar", log_data$message[pressure_logs])
  blank_grep <- gregexpr(" ", log_data$message[pressure_logs])

  # Extract pressure values
  pressure_values <- mapply(function(message, blank, pressure) {
    substr(message, blank[max(which(blank < pressure)) - 1], pressure - 1)
  }, message = log_data$message[pressure_logs], blank = blank_grep, pressure = pressure_grep, SIMPLIFY = FALSE)

  pressure_units <- gsub("\\s", "", substr(pressure_values, 7, nchar(pressure_values)))
  pressure_values <- as.numeric(pressure_values)

  # Extract timestamps
  pressure_timestamps <- log_data$timestamp[pressure_logs]

  return(data.frame(datetime = pressure_timestamps, pressure = pressure_values, unit = pressure_units))
}

#' Get Latest Log File
#'
#' Searches for the latest log file matching a specified pattern.
#'
#' @param file_ending A string specifying the file extension to look for (default is `".log"`).
#' @param file_pattern A regex pattern to filter filenames (default is `"Terminal"`).
#' @param n How many files should be shown(default is `1`).
#'
#' @return A string representing the most recently modified file matching the pattern.
#'
#' @export
get_latest_file <- function(file_ending = "\\.log", file_pattern = "Terminal", n = 1, path = wd$local$MultiTec5) {

  old.path <- getwd()
  setwd(path)

  log_files <- list.files(pattern = file_ending)
  log_files <- grep(pattern = file_pattern, x = log_files, value = TRUE)
  if (length(log_files) == 0) return(NULL)

  latest_file <- tail( log_files[ order(file.info(log_files)$mtime, decreasing = T) ], n)
  setwd(old.path)

  return(latest_file)
}
