#' Estimate Analysis Completion Time
#'
#' This function calculates the estimated completion time for ongoing sample analysis, given the duration of each sample and the number of samples remaining.
#'
#' @param sample.duration Numeric value representing the duration of each sample in minutes.
#' @param samples.to.go Integer representing the number of remaining samples.
#' @param time The starting time of analysis, default is the current system time.
#' @param unit Time unit for the output, can be "h" for hours or "min" for minutes. Default is "h".
#'
#' @return A message string describing the expected completion time.
#' @export
analysis.time <- function(sample.duration, samples.to.go, time = Sys.time(), unit = c("h", "min")) {
  unit <- match.arg(unit)
  
  samples.ready <- sample.duration * samples.to.go * 60 + time
  time.to.go <- as.numeric(difftime(samples.ready, time, units = ifelse(unit == "h", "hours", "mins")))
  
  time.to.go <- round(time.to.go, 1)
  
  if (as.Date(samples.ready) == as.Date(time)) {
    return(paste0("Fertig heute um ", strftime(samples.ready, format = "%H:%M"), ", noch ", time.to.go, " ", unit))
  } else if (as.Date(samples.ready) == as.Date(time) + 1) {
    return(paste0("Fertig morgen um ", strftime(samples.ready, format = "%H:%M"), ", noch ", time.to.go, " ", unit))
  } else if (as.Date(samples.ready) == as.Date(time) + 2) {
    return(paste0("Fertig übermorgen um ", strftime(samples.ready, format = "%H:%M"), ", noch ", time.to.go, " ", unit))
  } else {
    return(paste0("Fertig um ", strftime(samples.ready, format = "%H:%M"), " am ", format(as.Date(samples.ready), "%d.%m"), ", noch ", time.to.go, " ", unit))
  }
}

#' Estimate Analysis Fluid consumption
#'
#' This function calculates the estimated medium amount for ongoing sample analysis, given the flow of a medium and the duration of the total analysis.
#'
#' @param sample.duration Numeric value representing the duration of each sample in minutes.
#' @param samples.to.go Integer representing the number of remaining samples.
#' @param time The starting time of analysis, default is the current system time.
#' @param unit Time unit for the output, can be "h" for hours or "min" for minutes. Default is "h".
#'
#' @return A message string describing the expected completion time.
#' @export
analysis.amount <- function(sample.duration
                            , medium.flow
                            , unit.flow = "mL/min") {
  unit.flow <- match.arg(unit.flow)
  
  duration.time <- as.numeric(substr(sample.duration
                                     , unlist( gregexpr("noch ", sample.duration)) + 5
                                     , unlist(lapply(gregexpr(" ", sample.duration), max))))
  
  duration.unit <- substr(sample.duration
                          , unlist(lapply(gregexpr(" ", sample.duration), max)) + 1
                          , nchar(sample.duration))
  
  if(duration.unit == "h" & grep("min", unit.flow) > 0) duration.time <- duration.time * 60
  
  duration.volume <- duration.time * medium.flow
  
  
  if(grep("mL", unit.flow) > 0) duration.volume <- duration.volume / 1000
  if(grep("mL", unit.flow) > 0) unit.volume <- "L"
  duration.volume <- round(duration.volume, 1)
  
  return(paste0("Noch mindestens ", duration.volume, " ", unit.volume, " Medium benötigt"))
  
}
