.onLoad <- function(libname, pkgname) {
  package_version <- packageDescription(pkgname)$Version
  version_name <- packageDescription(pkgname)$VersionName

  message(paste(pkgname, "version", package_version, "--", version_name))
  message("Check the GitHub repository at: https://github.com/DrFrEdison/r4apl")

  # Load any other necessary initialization code here

  # Make sure to return(TRUE) at the end
  return(TRUE)
}

#' Format Datetime String
#'
#' This function formats a datetime object, either extracting the date portion or both date and time, with customizable formatting.
#'
#' @param datetime A datetime object. Default is the current system time.
#' @param time Logical, if TRUE, includes both date and time in the output. Default is FALSE (date only).
#'
#' @return A formatted string representing the date or datetime.
#' @export
datetime <- function(datetime = Sys.time(), time = FALSE){
  if(!time){
    return(gsub("\\-", "", substr(datetime, 3, 10)))
  }
  if(time){
    return(gsub("\\ ", "_", gsub("\\:", "", gsub("\\-", "", substr(datetime, 3, 19)))))
  }
}

#' Get ISO Week Number
#'
#' This function returns the ISO week number for a given date.
#'
#' @param date A date object. Default is the current system date.
#'
#' @return An integer representing the ISO week number.
#' @export
kw <- function(date = Sys.Date()) {
  data.table::isoweek(date)
}

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

#' Create a Subscripted Formula
#'
#' This function takes a formula and adds subscripts to any numeric values found at the end.
#' Optionally, the base of the formula can be bolded.
#'
#' @param formula A string representing the formula.
#' @param bold Logical, if TRUE, makes the base part bold. Default is TRUE.
#'
#' @return A parsed expression suitable for rendering in plots or other mathematical displays.
#' @export
subscript_formula <- function(formula, bold = TRUE) {
  if (grepl("\\d+$", formula)) {
    numbers <- str_extract(formula, "\\d+$")
    base <- str_remove(formula, "\\d+$")
    if (bold) return(parse(text = paste0("bold(", base, "[", numbers, "])")))
    else return(parse(text = paste0(base, "[", numbers, "]")))
  } else {
    return(formula)  # Return formula unchanged if no numbers found
  }
}


#' Display a Subset of Data
#'
#' Displays the first or last 10 rows and selected columns from a data frame or data table.
#'
#' @param x A data frame or data table.
#' @param nrowp Number of rows to display. Default is 10.
#' @param ncolp A vector of column indices to display, default is 1:10.
#' @param tail Logical, if TRUE, shows the last rows, otherwise shows the first rows. Default is FALSE.
#'
#' @return A subset of the data frame or data table.
#' @export
head10 <- function(x, nrowp = 10, ncolp = 1:10, tail = FALSE) {
  if (length(ncolp) == 1 && ncolp == "all") ncolp <- 1:ncol(x)
  ncolp <- sort(unique(ifelse(ncolp > ncol(x), ncol(x), ncolp)))
  nrowp <- min(nrowp, nrow(x))

  if (!tail) {
    if (is.data.table(x)) return(head(x[, ncolp, with = FALSE], nrowp))
    else return(head(x[, ncolp], nrowp))
  } else {
    if (is.data.table(x)) return(tail(x[, ncolp, with = FALSE], nrowp))
    else return(tail(x[, ncolp], nrowp))
  }
}


#' Adjust Range of Data with Percentile Changes
#'
#' This function calculates the range of a numeric vector and adjusts its limits based on percentage changes specified by the user.
#'
#' @param xy A numeric vector or matrix of data values.
#' @param p A two-element numeric vector specifying the percentage adjustments for the lower and upper limits. Default is c(5, 5).
#'
#' @return A numeric vector of length 2 representing the adjusted lower and upper limits of the range.
#' @export
rangexy <- function(xy, p = c(5, 5)) {

  # Calculate the range of the data, ignoring NA values
  range.xy <- range(xy, na.rm = TRUE)

  # Ensure p is a two-element vector
  if (length(p) == 1) p <- c(p, p)

  # Calculate the percentage change for the lower limit
  p[1] <- ifelse(range.xy[1] > 0, (100 - p[1]) / 100, (100 + p[1]) / 100)

  # Calculate the percentage change for the upper limit
  p[2] <- ifelse(range.xy[2] > 0, (100 + p[2]) / 100, (100 - p[2]) / 100)

  # Adjust the lower and upper limits of the range
  range.xy[1] <- range.xy[1] * p[1]
  range.xy[2] <- range.xy[2] * p[2]

  if(range.xy[ 1 ] == range.xy[ 2 ]) range.xy[ 2 ] <- range.xy[ 2 ] + 1
  return(range.xy)
}

#' Open a Directory in the File Explorer or Browser
#'
#' This function opens a specified directory in the system's file explorer (Windows) or in the default web browser (non-Windows systems).
#'
#' @param dir A string specifying the directory to open. Default is the current working directory.
#'
#' @export
opendir <- function(dir = getwd()) {
  if (.Platform['OS.type'] == "windows") {
    # For Windows, open the directory in File Explorer using shell.exec()
    shell.exec(gsub("/", "\\\\", dir))
  } else {
    # For non-Windows systems, open the directory in the default browser
    system(paste(Sys.getenv("R_BROWSER"), dir))
  }
}

#' Reorder Elements in a Vector
#'
#' This function moves elements within a vector based on a specified move command.
#'
#' @param invec A character vector representing the original order of elements.
#' @param movecommand A string specifying how elements should be moved, using keywords such as "before", "after", "first", and "last".
#'
#' @return A reordered character vector based on the move command.
#' @export
moveme <- function(invec, movecommand) {

  # Split the move command into separate actions
  movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]], ",|\\s+"), function(x) x[x != ""])

  # Create a list of elements to move and where to move them
  movelist <- lapply(movecommand, function(x) {
    Where <- x[which(x %in% c("before", "after", "first", "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })

  # Initialize the modified vector
  myVec <- invec

  # Process each move command
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])  # Remove the elements to be moved
    A <- movelist[[i]][[2]][1]  # Get the "before", "after", "first", "last" command

    # Determine the position to insert the elements
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      after <- if (A == "before") match(ba, temp) - 1 else match(ba, temp)
    } else if (A == "first") {
      after <- 0
    } else if (A == "last") {
      after <- length(myVec)
    }

    # Insert the elements at the calculated position
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }

  return(myVec)
}
