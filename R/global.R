#' Determine Rows and Columns for Plot Layout
#'
#' This function calculates the optimal number of rows and columns for a plotting layout based on the total number of plots.
#'
#' @param n Integer, the number of plots.
#'
#' @return A vector of two integers: the number of rows and columns for the plot layout.
#' @export
parmfrow <- function(n) {
  # Calculate the number of rows and columns to arrange n plots
  nrows <- ceiling(sqrt(n))
  ncols <- ceiling(n / nrows)
  return(c(nrows, ncols))
}


#' Package Load Initialization
#'
#' This function runs automatically when the package is loaded, displaying version information and additional details.
#'
#' @param libname The library name.
#' @param pkgname The package name.
#'
#' @return TRUE (ensures the function completes successfully).
#' @export
.onLoad <- function(libname, pkgname) {
  # Get the package version and version name
  package_version <- packageDescription(pkgname)$Version
  version_name <- packageDescription(pkgname)$VersionName

  # Display the package version information and GitHub repository link
  message(paste(pkgname, "version", package_version, "--", version_name))
  message("Check the GitHub repository at: https://github.com/DrFrEdison/r4apl")

  # Ensure the function returns TRUE to indicate successful loading
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
datetime <- function(datetime = Sys.time(), time = FALSE) {
  # Remove hyphens from the date and time string
  if (!time) {
    return(gsub("-", "", substr(datetime, 3, 10)))
  }
  if (time) {
    return(gsub(" ", "_", gsub(":", "", gsub("-", "", substr(datetime, 3, 19)))))
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
  # Use the data.table package's isoweek function to get the ISO week
  data.table::isoweek(date)
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
  # Check if the formula ends with a number and apply subscript formatting
  if (grepl("\\d+$", formula)) {
    numbers <- str_extract(formula, "\\d+$")
    base <- str_remove(formula, "\\d+$")
    if (bold) {
      return(parse(text = paste0("bold(", base, "[", numbers, "])")))
    } else {
      return(parse(text = paste0(base, "[", numbers, "]")))
    }
  } else {
    return(formula)  # Return the formula unchanged if no number is found
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
  # Adjust column indices if 'all' is provided or if indices exceed the number of columns
  if (length(ncolp) == 1 && ncolp == "all") ncolp <- 1:ncol(x)
  ncolp <- sort(unique(ifelse(ncolp > ncol(x), ncol(x), ncolp)))

  # Adjust the number of rows to avoid exceeding the number of rows
  nrowp <- min(nrowp, nrow(x))

  # Display either the head or tail of the selected columns
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

  # Ensure the range isn't collapsed to a single point
  if (range.xy[1] == range.xy[2]) range.xy[2] <- range.xy[2] + 1
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
  # Open directory using system-specific methods
  if (.Platform['OS.type'] == "windows") {
    # On Windows, open in File Explorer
    shell.exec(gsub("/", "\\\\", dir))
  } else {
    # On other systems, open in the default web browser
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
