#' Replace German Day Abbreviations with English Equivalents
#'
#' This function replaces German day abbreviations (e.g., "Mo.") with their English equivalents (e.g., "Mon").
#'
#' @param date_string A character string that contains German day abbreviations.
#'
#' @return A character string with German day abbreviations replaced by their English equivalents.
#'
#' @examples
#' replace_german_days("Mo. 12")  # returns "Mon 12"
#'
#' @export
replace_german_days <- function(date_string) {
  date_string <- gsub("Mo\\.", "Mon", date_string)
  date_string <- gsub("Di\\.", "Tue", date_string)
  date_string <- gsub("Mi\\.", "Wed", date_string)
  date_string <- gsub("Do\\.", "Thu", date_string)
  date_string <- gsub("Fr\\.", "Fri", date_string)
  date_string <- gsub("Sa\\.", "Sat", date_string)
  date_string <- gsub("So\\.", "Sun", date_string)
  return(as.character(date_string))
}

#' Process Interflex Data from Clipboard
#'
#' This function processes data copied from the clipboard (Interflex) and extracts relevant details such as dates, working times, and others.
#'
#' @param dat.cb A character vector containing data from the clipboard. Default is to read from the clipboard.
#'
#' @return A data frame with processed Interflex data.
#'
#' @export
interflex_Clipboard <- function(dat.cb = readClipboard()) {

  input_string <- dat.cb[grep("Journal", dat.cb)]

  if (length(input_string) == 0) return(NULL)

  pattern <- "([0-9]{2})/([0-9]{4})"
  month.year <- stri_match_first_regex(input_string, pattern)

  dat.trs <- dat.cb[grep("Datum", dat.cb):length(dat.cb)]
  dat.trs <- dat.trs[-grep("Fehlzeitenfenster ", dat.trs)]

  to.append <- list()
  for (i in 1:(length(dat.trs))) {

    # Check if i is a date and the next element is a KW
    if (i != length(dat.trs)) {
      if (all(grepl("^[A-Za-z]{2}\\. \\d{2}\\.$", dat.trs[i]), !grepl("^\\d+$", dat.trs[i + 1]))) {
        to.append[[i]] <- 1
      }
    }
  }

  for (i in rev(which(unlist(lapply(to.append, length)) > 0))) {
    dat.trs <- append(dat.trs, " ", after = i)
  }

  Feiertag <- c("Tag der Einheit\t", "Allerheiligen\t", "Neujahr\t", "1.Weihnachtstag\t", "2.Weihnachtstag\t")

  for(i in seq_along(Feiertag)){

    if( length( grep(Feiertag[ i ], dat.trs) ) > 0){
      for(j in 1:4)
        dat.trs <- append(dat.trs, " ", after = (grep(Feiertag[ i ], dat.trs) - 1))

      dat.trs <- gsub(Feiertag[ i ], gsub("\t", "", Feiertag[ i ]), dat.trs)
    }

  }


  # fehlt unentschuldigt ####
  for(i in rev(grep("fehlt unentschuldigt", dat.trs))){

    for(j in 1:4)
      dat.trs <- append(dat.trs, " ", after = i - 1)

  }

  lines <- unlist(strsplit(dat.trs, " "))
  columns <- dat.trs[1:grep("Zeitkto", dat.trs)]
  dat.trs <- dat.trs[-which(dat.trs %in% columns)]

  interflex.df <- matrix(dat.trs, ncol = 10, byrow = TRUE)
  interflex.df <- data.frame(interflex.df)
  colnames(interflex.df) <- columns

  # Convert numeric fields by replacing commas with dots
  interflex.df$Soll <- as.numeric(gsub(",", ".", interflex.df$Soll))
  interflex.df$Ist <- as.numeric(gsub(",", ".", interflex.df$Ist))
  interflex.df$Zeitkto <- as.numeric(gsub(",", ".", interflex.df$Zeitkto))

  # Datum processing
  Dateraw <- interflex.df$Datum[which(nchar(interflex.df$Datum) > 1)]
  Date <- paste(month.year[, 3], month.year[, 2], gsub("[^0-9]", "", interflex.df$Datum[which(nchar(interflex.df$Datum) > 1)]), sep = "-")
  Date <- as.Date(Date)
  interflex.df$datetime <- NA

  for (i in seq_along(Dateraw)) {
    if (i != max(seq_along(Dateraw))) {
      rangep <- which(interflex.df$Datum == Dateraw[i]):(which(interflex.df$Datum == Dateraw[i + 1]) - 1)
      interflex.df$datetime[rangep] <- as.character(Dateraw[i])
    } else {
      rangep <- which(interflex.df$Datum == Dateraw[i]):length(interflex.df$Datum)
      interflex.df$datetime[rangep] <- as.character(Dateraw[i])
    }
  }

  interflex.df$datetime <- as.character(sapply(interflex.df$datetime, replace_german_days))
  interflex.df$datetime <- as.Date(paste(month.year[, 3], month.year[, 2], interflex.df$datetime, sep = "-"), format = "%Y-%m-%a %d.")
  interflex.df$Datum <- interflex.df$datetime
  interflex.df$datetime <- NULL

  # Write Interflex data to a file
  Date <- max(interflex.df$Datum)
  fwrite(interflex.df, file.path(wd$data$interflex, paste0(substr(datetime(as.character(Date)), 1, 4), "_interflex.csv")), row.names = FALSE, sep = ";", dec = ",")
}


#' Process Nebenzeiteneintrag Entries in Interflex Files
#'
#' This function processes Nebenzeiteneintrag (additional time) entries from the Interflex files for a specific month and year.
#'
#' @param interflex.files A vector of Interflex file names.
#' @param month The month for which to extract Nebenzeiteneintrag. Default is the current month.
#' @param year The year for which to extract Nebenzeiteneintrag. Default is the current year.
#'
#' @return A list containing processed Nebenzeiteneintrag data, including the date and day of the week.
#'
#' @export
interflex_Nebenzeiteneintrag <- function(interflex.files = interflex.files, month = month(Sys.Date()), year = year(Sys.Date())) {

  year <- substr(year, 3, 4)
  month <- formatC(x = month, digits = 2, width = 2, flag = "0")

  interflex.files <- interflex.files[substr(interflex.files, 1, 4) %in% paste0(year, month)]
  interflex.files.read <- fread(file.path(wd$data$interflex, interflex.files))

  # Remove rows with empty "Ist" values or with the "Urlaub" (vacation) reason
  interflex.files.read <- interflex.files.read[nchar(interflex.files.read$Ist) > 0, ]
  interflex.files.read <- interflex.files.read[interflex.files.read$Fehlgrund != "Urlaub", ]
  interflex.files.read <- interflex.files.read[interflex.files.read$Fehlgrund != "Anforderung eAU", ]
  interflex.files.read <- interflex.files.read[interflex.files.read$Fehlgrund != "Krank", ]

  # Process Nebenzeiten data
  Nebenzeiten <- list()
  Nebenzeiten$date <- interflex.files.read$Datum
  Nebenzeiten$date.all <- Nebenzeiten$date[nchar(Nebenzeiten$date) > 4]
  Nebenzeiten$date <- Nebenzeiten$date[!is.na(interflex.files.read$Ist)]
  Nebenzeiten$Nebenzeiteneintrag <- interflex.files.read$Ist[!is.na(interflex.files.read$Ist)]

  Nebenzeiten$date <- Nebenzeiten$date[Nebenzeiten$Nebenzeiteneintrag != 0]
  Nebenzeiten$Nebenzeiteneintrag <- Nebenzeiten$Nebenzeiteneintrag[Nebenzeiten$Nebenzeiteneintrag != 0]

  # Apply the function to replace German day abbreviations
  Nebenzeiten$date <- as.character(sapply(Nebenzeiten$date, replace_german_days))
  Nebenzeiten$date.all <- as.character(sapply(Nebenzeiten$date.all, replace_german_days))

  interflex.files.read$datetime <- NA
  unique.date <- unique(interflex.files.read$Datum)
  unique.date <- unique.date[nchar(unique.date) > 2]

  for (i in seq_along(Nebenzeiten$date.all)) {
    if (i != max(seq_along(Nebenzeiten$date.all))) {
      rangep <- which(interflex.files.read$Datum == unique.date[i]):(which(interflex.files.read$Datum == unique.date[i + 1]) - 1)
      interflex.files.read$datetime[rangep] <- as.character(Nebenzeiten$date.all[i])
    } else {
      rangep <- which(interflex.files.read$Datum == unique.date[i]):length(interflex.files.read$Datum)
      interflex.files.read$datetime[rangep] <- as.character(Nebenzeiten$date.all[i])
    }
  }

  Nebenzeiten$weekdays <- factor(weekdays(as.Date(Nebenzeiten$date)), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

  returnlist <- list(gsub("\\.", ",", Nebenzeiten$Nebenzeiteneintrag), Nebenzeiten$date, Nebenzeiten$weekdays, interflex.files.read)
  names(returnlist) <- c("Nebenzeiteneintrag", "Datum", "Wochentage", "interflex.date")

  return(returnlist)
}

#' Rename Interflex Zeitkonto Files
#'
#' This function renames Interflex Zeitkonto (time account) files to a new naming convention based on provided data.
#' The function changes the name of files from "IST" (current state) to "SOLL" (target state) with the correct time.
#'
#' @param dat1 A data object that contains the 'Zeitkto' (time account) and 'Beginn' (start time) fields used for renaming.
#'
#' @return This function renames files in the specified directory but does not return a value.
#'
#' @details
#' The function looks for files in the directory specified by `wd$R$master`, and renames files following the pattern
#' "interflex_Zeitkto_IST" to the format "interflex_Zeitkto_<Zeitkto>_Beginn_<Time>" where `Zeitkto` and `Beginn` come
#' from the provided `dat1` argument.
#'
#' @examples
#' \dontrun{
#' interflex_Zeitkto_Beginn(dat1 = interflex.out)
#' }
#'
#' @export
interflex_Zeitkto_Beginn <- function(dat1 = interflex.out) {

  # Store the current working directory
  ist.path <- getwd()

  # Search for files in the specified directory with the pattern "interflex_Zeitkto_"
  Zeitkto_Beginn_IST <- dir(path = wd$R$master, pattern = "interflex_Zeitkto_")

  # Create the target filename by appending Zeitkto and processed 'Beginn' time
  Zeitkto_Beginn_SOLL <- file.path(wd$R$master, paste0("interflex_Zeitkto_", dat1$Zeitkto, "_Beginn_", gsub("\\:", "", dat1$Beginn)))

  # Change working directory to the master folder
  setwd(wd$R$master)

  # Rename files, but check if files exist first
  if (length(Zeitkto_Beginn_IST) > 0) {
    success <- file.rename(from = Zeitkto_Beginn_IST, to = Zeitkto_Beginn_SOLL)

    # Provide feedback if renaming fails
    if (!success) {
      warning("File renaming failed. Please check file paths and permissions.")
    }
  } else {
    warning("No files found matching the pattern 'interflex_Zeitkto_'")
  }

  # Return to the original working directory
  setwd(ist.path)
}




