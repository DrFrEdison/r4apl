#' Read ChemStation Version 87 PDF Reports
#'
#' This function reads and processes GC ChemStation B.04.03-SP1 (87) 2001-2012 PDF reports.
#' It extracts sample information and report data, and handles special formatting like removing "Type" columns.
#'
#' @param pdf.file Character, the path to the PDF file.
#' @param type.pattern Character vector, patterns to remove the "Type" column from the report. Default includes various known patterns.
#'
#' @return A list with two elements: "data" (data frames containing the extracted report information) and "unit" (the units for the corresponding columns).
#'
#' @examples
#' \dontrun{
#' result <- read.GC.ChemStation87.pdf("path/to/pdf.pdf")
#' }
#'
#' @export
read.GC.ChemStation87.pdf <- function(pdf.file
                                      , amount.unit = c("\\% \\("
                                                        , "ppm \\(")
                                      , type.pattern =c("BB", "BV", "BB N", "BB S", "BV S", "BBA", "BBAS"
                                                        , "HHA+"
                                                        , "FM"
                                                        , "MM", "MF", "MM N"
                                                        , "VV", "VB", "VB S", "VV S", "VBA")){


  # Ensure type patterns are ordered by length, decreasing
  type.pattern <- unique(type.pattern[ order(nchar(type.pattern), decreasing = T)])

  # Initialize the data list
  dat <- list()
  dat$pdf$read <- pdftools::pdf_text(pdf.file)

  # Locate occurrences of "External Standard Report" in the PDF text
  dat$raw$External.Standard.Report <- gregexpr("External Standard Report", dat$pdf$read)

  # Warning when External Standard Report is found more than once per page
  if( unique( unlist( lapply(dat$raw$External.Standard.Report, function( x ) which( x > 0)))) != 1)
    warning("More than one 'External Standard Report' per page found")

  # Initialize lists and data frames for processing PDF row data
  dat$trs$pdf <- list()

  # Process each page and split in Key (before :) and value (after :) columns
  for(i in 1 : length( dat$pdf$read )){

    data <- data.frame(Key = NA, Value = NA)
    rows <- strsplit(dat$pdf$read[[ i ]], "\n")[[1]]

    # Process each row
    for (row in rows) {
      # Trim whitespace from the row
      row <- str_trim(row)

      # Skip empty rows
      if (row == "") next

      # Skipy rows with file path
      if( length( grep("([A-Z])\\:\\\\", row)) == 1){
        if(length( grep("\\.M$", row)) != 1){
          filepath <- substr(row, 11, nchar(row))
          next
        }
      }

      # Check if the row contains key-value pairs
      if (grepl(":", row)) {
        key_value <- strsplit(row, ":")[[1]]
        key <- str_trim(key_value[1])
        value <- str_trim(paste(key_value[-1], collapse = ":"))

        # Add the key-value pair to the data frame
        data <- rbind(data, data.frame(Key = key, Value = value, stringsAsFactors = FALSE))
      } else {
        # Handle other types of rows here
        # For now, just add them as a new row with a generic key
        data <- rbind(data, data.frame(Key = "Other", Value = row, stringsAsFactors = FALSE))
      }
    }

    dat$trs$pdf[[ i ]] <- data
  }

  # merge pages which belong to one External Standard Report, i.e. to one sample ####
  # For this, grep External Standard Report from first column
  dat$trs$External.Standard.Report <- which(unlist(lapply(dat$trs$pdf, function ( x ) length(grep("External Standard Report", x[ , 2])))) == 1)

  # Combine pages that belong to one sample
  for(i in 1 : length( dat$trs$External.Standard.Report)){
    if( i != length( dat$trs$External.Standard.Report)){
      if(dat$trs$External.Standard.Report[ i + 1 ] - dat$trs$External.Standard.Report[ i ] <= 1){

        dat$trs$sample.page[[ i ]] <- rbindlist(dat$trs$pdf[ dat$trs$External.Standard.Report[ i ] : (dat$trs$External.Standard.Report[ i + 1 ]) ])

      } else{

        dat$trs$sample.page[[ i ]] <- rbindlist(dat$trs$pdf[ dat$trs$External.Standard.Report[ i ] : (dat$trs$External.Standard.Report[ i + 1 ] - 1) ])

      }

    }
    if( i == length( dat$trs$External.Standard.Report)) dat$trs$sample.page[[ i ]] <- rbindlist( dat$trs$pdf[ unique(dat$trs$External.Standard.Report[ i ] : length(dat$trs$pdf)) ])
  }

  # Remove duplicated rows and rows without types
  dat$trs$sample.page.no.dub <- lapply(dat$trs$sample.page, function(x) {
    to_remove <- duplicated(x$Key) & x$Key != "Other" & x$Key != "Totals"
    if (any(to_remove)) {
      x <- x[!to_remove, ]
    }
    return(x)
  })

  # Initialize final data storage
  dat$trs$sample.page.no.dub.no.type.values <- list()
  dat$final$df <- list()
  dat$final$compound <- list()
  dat$final$numeric <- list()
  dat$final$column.names <- list()
  dat$final$column.units <- list()
  dat$final$info <- list()
  dat$final$unit <- list()

  df.pattern <- list()

  # Define a regular expression to match numbers and words
  regex <- "(\\d+\\.\\d+(?:e[+-]?\\d+)?|[a-zA-Z0-9\\-]+(?:\\s[a-zA-Z0-9\\-]+)*)"

  # Loop through reports ####
  for(i in seq_along(dat$trs$sample.page.no.dub)){

    # remove Area Percent Report
    if(any(apply(dat$trs$sample.page.no.dub[[ i ]], 1, function( x ) length(grep("Area Percent Report", x))) == 1)){
      delete.x1 <- which(apply(dat$trs$sample.page.no.dub[[ i ]], 1, function( x ) length(grep("Area Percent Report", x))) > 0)
      delete.x2 <- which(apply(dat$trs$sample.page.no.dub[[ i ]], 1, function( x ) grep("Totals", x))>0)
      delete.x2 <- delete.x2[ which(delete.x2 > delete.x1)]

      dat$trs$sample.page.no.dub[[ i ]] <- dat$trs$sample.page.no.dub[[ i ]][ - c(delete.x1 : delete.x2) , ]
      rm(delete.x1, delete.x2)
    }

    # Locate the start and end of the table
    x1 <- which(apply(dat$trs$sample.page.no.dub[[ i ]], 1, function( x ) length(grep("RetTime", x))) > 0)[ 1 ]
    x2 <- which(apply(dat$trs$sample.page.no.dub[[ i ]], 1, function( x ) length(grep("Totals", x))) > 0)[ 1 ]
    x1x2 <- x1:x2
    x1x2 <- x1x2[ which(dat$trs$sample.page.no.dub[[ i ]]$Key[ x1x2 ] == "Other" & !is.na(dat$trs$sample.page.no.dub[[ i ]]$Key[ x1x2 ]))]
    x1x2 <- x1x2[ !duplicated(dat$trs$sample.page.no.dub[[ i ]][x1x2 , 2]) ]

    # Extract column names and units
    dat$final$column.names[[ i ]] <- strsplit(as.character(dat$trs$sample.page.no.dub[[ i ]][x1x2[ 1 ],2]), " ")[[ 1 ]]
    dat$final$column.names[[ i ]] <- dat$final$column.names[[ i ]][ which( nchar( dat$final$column.names[[ i ]] ) > 0) ]
    # \\s: Matches any whitespace character (spaces, tabs, newlines, etc.). {1,}: Indicates that the preceding element (in this case, the whitespace character \\s) should appear at least 1 times, but can appear more.
    dat$final$column.units[[ i ]] <- strsplit( as.character( dat$trs$sample.page.no.dub[[ i ]][x1x2[2] , 2]), "\\s{2,}")
    x1x2 <- x1x2[ - c(1,2)]

    # Remove rows with many dashes
    x1x2 <- x1x2[ which(apply(dat$trs$sample.page.no.dub[[ i ]][x1x2 , 2]
                              , 1
                              , function( x ) length(gregexpr("-", as.character( x))[[ 1 ]])) < 30)]

    # Remove rows with RetTime (if more than once), this is the case when the table has a page break
    if(any( unlist(apply(dat$trs$sample.page.no.dub[[ i ]][x1x2 , 2]
                         , 1
                         , function( x ) gregexpr("RetTime", as.character( x)))) > 0))

      x1x2 <- x1x2[ -which(unlist(apply(dat$trs$sample.page.no.dub[[ i ]][x1x2 , 2]
                                        , 1
                                        , function( x ) gregexpr("RetTime", as.character( x)))) > 0)]

    # remove rows without any number
    x1x2 <- x1x2[ apply(dat$trs$sample.page.no.dub[[ i ]][x1x2 , 2]
                        , 1
                        , function( x ) grepl("\\d", x))]

    # remove rows with [ & ] (duplicated unit)
    x1x2 <- x1x2[ which(apply(dat$trs$sample.page.no.dub[[ i ]][x1x2 , 2]
                              , 1
                              , function( x ) length(which(!grepl("\\[", x) & !grepl("\\]", x))))>0)]

    # remove type from column ####
    list.pattern <- list()
    df.pattern[[ i ]] <- matrix(nrow = length(type.pattern), ncol = length( x1x2 ))
    for(pattern in seq_along(type.pattern)){

      list.pattern[[ pattern ]] <- apply(dat$trs$sample.page.no.dub[[ i ]][x1x2 , 2]
                                         , 1
                                         , function( x ) gregexpr( type.pattern[ pattern ], x))


      df.pattern[[ i ]][pattern, ] <- unlist(lapply( list.pattern[[ pattern ]], function( x ) x[[ 1 ]]))

      dat$trs$sample.page.no.dub[[ i ]][x1x2 , 2] <- apply(dat$trs$sample.page.no.dub[[ i ]][x1x2 , 2]
                                                           , 1
                                                           , function( x ) gsub( type.pattern[ pattern ], "", x))

    }
    df.pattern[[ i ]] <- type.pattern[ apply(df.pattern[[ i ]], 2, function ( x ) which( x > 0)[ 1 ]) ]

    # Extract numeric values and compound names ####
    dat$trs$sample.page.no.dub.no.type.values[[ i ]] <- apply(dat$trs$sample.page.no.dub[[ i ]][x1x2 , 2]
                                                              , 1
                                                              , function( x ) str_match_all(x, regex)[[1]][ , 1])

    if(is.null(ncol(dat$trs$sample.page.no.dub.no.type.values[[ i ]]))){
      dat$final$compound[[ i ]] <- lapply(dat$trs$sample.page.no.dub.no.type.values[[ i ]]
                                          , function ( x ) as.character(x[ which(dat$final$column.names[[ i ]][ !dat$final$column.names[[ i ]] %in% c("Grp", "Type")] == "Name")]))

      suppressWarnings(
        dat$final$numeric[[ i ]] <- lapply(dat$trs$sample.page.no.dub.no.type.values[[ i ]]
                                           , function ( x ) as.numeric(x[ which(dat$final$column.names[[ i ]][ !dat$final$column.names[[ i ]] %in% c("Grp", "Type")] != "Name")]))
      )
      dat$final$numeric[[ i ]] <- do.call(cbind, dat$final$numeric[[ i ]])

    } else{
      dat$final$compound[[ i ]] <- apply(dat$trs$sample.page.no.dub.no.type.values[[ i ]]
                                         , 2
                                         , function( x ) as.character(x[ which(dat$final$column.names[[ i ]][ !dat$final$column.names[[ i ]] %in% c("Grp", "Type")] == "Name")]))

      suppressWarnings(
        dat$final$numeric[[ i ]] <- apply(dat$trs$sample.page.no.dub.no.type.values[[ i ]]
                                          , 2
                                          , function ( x ) as.numeric(x[ which(dat$final$column.names[[ i ]][ !dat$final$column.names[[ i ]] %in% c("Grp", "Type")] != "Name")]))
      )
    }

    # Create the final data frame
    dat$final$df[[ i ]] <- data.frame(unlist(dat$final$compound[[ i ]])
                                      , t(dat$final$numeric[[ i ]])
                                      , type = df.pattern[[ i ]])
    colnames(dat$final$df[[ i ]]) <- c("Compound", dat$final$column.names[[ i ]][ !dat$final$column.names[[ i ]] %in% c("Grp", "Type", "Name")], "Type")

    # Add column units

    dat$final$unit[[ i ]] <- rep("", ncol(dat$final$df[[ i ]]))
    dat$final$unit[[ i ]][ which( colnames(dat$final$df[[ i ]]) %in% "Area")] <- dat$final$column.units[[ i ]][[ 1 ]][ grep("\\*", dat$final$column.units[[ i ]][[ 1 ]])]
    dat$final$unit[[ i ]][ which( colnames(dat$final$df[[ i ]]) %in% "Amount")] <- dat$final$column.units[[ i ]][[ 1 ]][ grep(amount.unit, dat$final$column.units[[ i ]][[ 1 ]])]
    dat$final$unit[[ i ]][ which( colnames(dat$final$df[[ i ]]) %in% "RetTime")] <- dat$final$column.units[[ i ]][[ 1 ]][ grep("min", dat$final$column.units[[ i ]][[ 1 ]])]

    # get sample info ####
    sample.name <- dat$trs$sample.page.no.dub[[ i ]][ , 2][ which(apply(dat$trs$sample.page.no.dub[[ i ]][ , 2], 1, function( x ) length(grep("\\.D\\)$", x)))>0) ]
    sample.name <- sample.name[1]
    sample.name <- substr(sample.name, unlist(gregexpr("\\(", sample.name))+1, unlist(gregexpr("\\)", sample.name))-1)
    sample.name <- basename(sample.name)

    sample.pattern <- c("Sample Name", "Acq. Operator", "Injection Date", "Inj Volume", "Method")
    sample.info <- lapply(sample.pattern
                          , function( x ) dat$trs$sample.page.no.dub[[ i ]][ , 2][ which(apply(dat$trs$sample.page.no.dub[[ i ]][ , 1], 1, function( y ) length(grep(x, y)))>0)[ 1 ]])

    sample.pattern[ 1 ] <- "Sample Info"

    dat$final$info[[ i ]] <- data.frame(sample.name, do.call(c, sample.info))
    colnames(dat$final$info[[ i ]]) <- c("Sample Name", sample.pattern)

    dat$final$df[[ i ]]$Sample.Name <- dat$final$info[[ i ]]$`Sample Name`
    dat$final$df[[ i ]]$Sample.Info <- dat$final$info[[ i ]]$`Sample Info`
    dat$final$df[[ i ]]$Operator <- dat$final$info[[ i ]]$`Acq. Operator`
    dat$final$df[[ i ]]$Injection.Date <- strptime(dat$final$info[[ i ]]$`Injection Date`, format="%m/%d/%Y %I:%M:%S %p", tz = "UTC")
    dat$final$df[[ i ]]$Injection.Volume <- dat$final$info[[ i ]]$`Inj Volume`
    dat$final$df[[ i ]]$Method = dat$final$info[[ i ]]$Method

  }

  returnlist <- list(dat$final$df, dat$final$unit)
  names( returnlist ) <- c("data", "unit")
  return( returnlist)
}
