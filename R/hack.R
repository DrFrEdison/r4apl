ueberstunden.x.weeks <- function(x = 20, dat = interflex ){
  lastxweeks <- tail(dat[ nchar(dat$Zeitkto) > 1 &
                            dat$Ist != 0 &
                            dat$Fehlgrund != "Dienstgang" &
                            dat$Fehlgrund != "Krank" &
                            dat$Fehlgrund != "Urlaub" &
                            dat$Fehlgrund != "Erkrankung Kind" &
                            dat$Fehlgrund != "Arbeitspause"], x)

  Ueberstunden <- as.numeric(gsub(",", ".", lastxweeks$Ist)) - 8
  returndat <- data.frame(Datum = lastxweeks$Datum
                          , Wochentag = weekdays.Date(lastxweeks$Datum)
                          , Diff = Ueberstunden)

  returndat <- list(returndat
                    , sum(returndat$Diff)
                    , tapply(returndat$Diff
                             , factor(returndat$Wochentag, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
                             , mean)
                    , mean(returndat$Diff))

  names( returndat ) <- c("Uebersicht", "Summe", "Ueberstunden pro Wochentag", "Mittelwert pro Tag")
  returndat$`Ueberstunden pro Wochentag` <- round(returndat$`Ueberstunden pro Wochentag`, 1)
  return(returndat)
}

#' Calculate Working Hours and Remaining Time
#'
#' This function calculates the total working hours, remaining time until a full workday (8 or 9 hours), and updates the "Zeitkonto" (time account) if provided.
#'
#' @param start_time Character, the starting time of the workday (format: "HH:MM").
#' @param ist_time Character or POSIXct, the current or end time of the workday. Default is `Sys.time()`.
#' @param pause_time Numeric, the length of the break (pause) in minutes. Default is NA.
#' @param Zeitkonto Numeric, the current "Zeitkonto" (time account balance). Default is NA.
#'
#' @return A data frame with the calculated times and remaining work time.
#'
#' @examples
#' \dontrun{
#' hack(start_time = "08:00", ist_time = "16:30", pause_time = 30, Zeitkonto = 1.5)
#' }
#'
#' @export
hack <- function(start_time
                 , ist_time = Sys.time()
                 , pause_time = NA
                 , Zeitkonto = NA) {

  # Convert times to POSIXct format for calculations
  start_time_obj <- as.POSIXct(start_time, format = "%H:%M")
  ist_time_obj <- as.POSIXct(ist_time, format = "%H:%M")

  #
  pause.8 <- 60 * 30
  pause.9 <- 60 * 15
  working.8 <- 60 * 60 * 8
  working.9 <- 60 * 60 * 9

  # Determine the end time based on pause time (default 30 minutes break if NA or <= 30)
  if (is.na(pause_time) || pause_time <= 30) {

    end_time.8_obj <- start_time_obj + working.8 + pause.8  # 8 hours + 30 mins pause
    end_time.9_obj <- start_time_obj + working.9 + pause.8 + pause.9  # 9 hours + 15 mins pause

  } else if (pause_time > 30 & pause_time <= 45) {

    end_time.8_obj <- start_time_obj + working.8 + 60 * pause_time
    end_time.9_obj <- start_time_obj + working.9 + pause.8 + pause.9

  } else if (pause_time > 45) {

    end_time.8_obj <- start_time_obj + working.8 + 60 * pause_time
    end_time.9_obj <- start_time_obj + working.9 + 60 * pause_time

  }

  warn.time <- start_time_obj + working.9 + pause.8 + pause.9 + 0.72 * 60 * 60
  max.time <- start_time_obj + working.9 + pause.8 + pause.9 + 1 * 60 * 60

  # Calculate working time in hours, subtracting pause time
  working_time <- as.numeric(difftime(ist_time_obj, start_time_obj, units = "hours"))

  if(working_time > 9) working_time <- working_time - .25
  if(working_time > 6) working_time <- working_time - .5
  if(is.na(pause_time) || pause_time > 30) working_time - pause_time / 60 - .5

  # Calculate remaining time for an 8-hour and 9-hour workday
  remaining_time.8 <- as.numeric(difftime(end_time.8_obj, ist_time_obj, units = "hours"))
  remaining_time.9 <- as.numeric(difftime(end_time.9_obj, ist_time_obj, units = "hours"))
  remaining_time.10.alarm <- as.numeric(difftime(warn.time, ist_time_obj, units = "hours"))
  remaining_time.10 <- as.numeric(difftime(max.time, ist_time_obj, units = "hours"))

  # Convert working time and remaining time to minutes if less than 2 hours
  working_time_unit <- if (working_time <= 2) "min" else "h"
  working_time <- if (working_time_unit == "min") round(working_time * 60, 0) else round(working_time, 1)

  remaining_time.8_unit <- if (abs(remaining_time.8) <= 2) "min" else "h"
  remaining_time.8 <- if (remaining_time.8_unit == "min") round(remaining_time.8 * 60, 0) else round(remaining_time.8, 1)

  # Determine if remaining time is overtime or time left to work
  remaining_time.8_text <- if (remaining_time.8 < 0) "Überstunden" else "zu hacken"

  # Create a data frame to return results
  hack.heimkomm <- data.frame(Hack = c(start_time,
                                       strftime(end_time.8_obj, format = "%H:%M"),
                                       working_time,
                                       abs(remaining_time.8)),
                              Unit = c("Uhr", "Uhr", working_time_unit, remaining_time.8_unit),
                              row.names = c("Gekommen", "8 Stunden", "gehackt", remaining_time.8_text))

  # Add pause time if provided
  if (!is.na(pause_time)) {
    hack.heimkomm <- rbind(hack.heimkomm, Pause = c(pause_time, "min"))
  }

  # Update Zeitkonto if provided
  if (!is.na(Zeitkonto)) {
    interflex_time <- as.numeric(substr(Zeitkto(Zeitkonto), 1, gregexpr(" h ", Zeitkto(Zeitkonto))[[1]])) * 60 +
      as.numeric(substr(Zeitkto(Zeitkonto), unlist(gregexpr(" und ", Zeitkto(Zeitkonto))) + nchar(" und "), gregexpr(" Minuten", Zeitkto(Zeitkonto))[[1]]))

    # Adjust Zeitkonto based on working time and remaining time
    if (working_time <= 6 && working_time_unit == "h") Zeitkonto <- remaining_time.8 * -1 + interflex_time + 30
    if (working_time > 6 && working_time_unit == "h") Zeitkonto <- remaining_time.8 * -1 + interflex_time

    Zeitkonto <- round(Zeitkonto / ifelse(remaining_time.8_unit == "min", 60, 1), 1)

    # Add Zeitkonto to the data frame
    hack.heimkomm <- rbind(hack.heimkomm, c(Zeitkonto, "h"))
    rownames(hack.heimkomm)[nrow(hack.heimkomm)] <- "Zeitkonto"
  }

  hack.heimkomm <- rbind(hack.heimkomm
                         , c(substr(warn.time, 12, 16), "Uhr")
                         , c(substr(max.time, 12, 16), "Uhr"))
  rownames(hack.heimkomm)[ nrow( hack.heimkomm ) - 1] <- "Warnung"
  rownames(hack.heimkomm)[ nrow( hack.heimkomm )] <- "10 h"

  return(hack.heimkomm)
}

#' Convert Zeitkonto to Hours and Minutes
#'
#' This function converts a time account (Zeitkonto) in numeric format to hours and minutes in a readable format.
#'
#' @param Zeitkonto Numeric, the time account balance to convert.
#'
#' @return A string in the format "X h und Y Minuten".
#'
#' @export
Zeitkto <- function(Zeitkonto) {

  # Handle numeric Zeitkonto values
  if (!is.na(as.numeric(Zeitkonto))) {

    if(unlist(gregexpr("\\.", Zeitkonto)) > 0){

      hours <- as.numeric(substr(Zeitkonto, 1, unlist(gregexpr("\\.", Zeitkonto)) - 1))
      minutes <- round(as.numeric(paste0("0.", substr(Zeitkonto, unlist(gregexpr("\\.", Zeitkonto)) + 1, nchar(Zeitkonto)))) * 60, 0)

    } else {

      hours <- as.numeric(Zeitkonto)
      minutes <- 0

    }

    return(paste0(hours, " h und ", minutes, " Minuten"))
  }

  return(NULL)
}
