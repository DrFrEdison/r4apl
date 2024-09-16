read.teckso.log <- function(logfile
                            , header = F
                            , sep = "\n"
                            , col.names = "log_entry"){
  
  log_data <- fread(logfile, header = header, sep = sep, col.names = col.names)
  
  # Extract timestamp, elapsed time, and message
  log_data[, `:=`(
    timestamp = sub("\\[(.*?)\\].*", "\\1", log_entry),
    elapsed_time = sub(".*\\((.*?)\\).*", "\\1", log_entry),
    message = sub(".*\\): (.*)", "\\1", log_entry)
  )]
  
  # Convert timestamp to POSIXct and to milliseconds
  options(digits.secs = 3)
  log_data[, timestamp := as.POSIXct(timestamp, format="%Y.%m.%d-%H:%M:%S")]
  log_data[, elapsed_time := strftime(as.POSIXct(elapsed_time, format="%H:%M:%OS"), format="%H:%M:%OS")]
  log_data$message <- gsub("\\[TAG\\=TERMINAL\\,PRIO=MSG\\]", "", log_data$message)
  for(i in 1:4)  log_data$message <- gsub("\\ \\ ", " ", log_data$message)
  # Pressure Log P1 ####
  pressure_logs <- log_data[grep("mBar", message)]
  
  pressure_logs_P1 <- grep("mBar", log_data$message)# which( grep("mBar", log_data$message) & grep("P1", log_data$message))
  pressure_logs_P1 <- pressure_logs_P1[ grep("P1", log_data$message[ pressure_logs_P1 ]) ]
  pressure_logs_P1_grep <- gregexpr("mBar", log_data$message[ pressure_logs_P1 ])
  pressure_logs_P1_datetime <- log_data$timestamp[ pressure_logs_P1 ]
  pressure_logs_P1 <- substr(log_data$message[ pressure_logs_P1 ]
                             , unlist( pressure_logs_P1_grep ) - 7
                             , unlist( pressure_logs_P1_grep ) + 3)
  pressure_logs_P1_unit <- substr(pressure_logs_P1, 7, nchar(pressure_logs_P1))
  pressure_logs_P1_unit <- gsub("\\ ", "", pressure_logs_P1_unit)
  
  pressure_logs_P1 <- substr(pressure_logs_P1, 1, 6)
  pressure_logs_P1 <- as.numeric(pressure_logs_P1)
  
  # Pressure Log P2 ####
  pressure_logs_P2 <- grep("mBar", log_data$message)# which( grep("mBar", log_data$message) & grep("P2", log_data$message))
  pressure_logs_P2 <- pressure_logs_P2[ grep("P2", log_data$message[ pressure_logs_P2 ]) ]
  pressure_logs_P2_grep <- gregexpr("mBar", log_data$message[ pressure_logs_P2 ])
  pressure_logs_P2_datetime <- log_data$timestamp[ pressure_logs_P2 ]
  pressure_logs_P2 <- substr(log_data$message[ pressure_logs_P2 ]
                             , unlist( pressure_logs_P2_grep ) - 7
                             , unlist( pressure_logs_P2_grep ) + 3)
  pressure_logs_P2_unit <- substr(pressure_logs_P2, 7, nchar(pressure_logs_P2))
  pressure_logs_P2_unit <- gsub("\\ ", "", pressure_logs_P2_unit)

  pressure_logs_P2 <- substr(pressure_logs_P2, 1, 6)
  pressure_logs_P2 <- as.numeric(pressure_logs_P2)
  
  returnlist <- list(data.frame(datetime = pressure_logs_P1_datetime, pressure = pressure_logs_P1, unit = pressure_logs_P1_unit)
                     , data.frame(datetime = pressure_logs_P2_datetime, pressure = pressure_logs_P2, unit = pressure_logs_P2_unit))
  names( returnlist ) <- c("Pressure_P1", "Pressure_P2")
  return( returnlist)
}
