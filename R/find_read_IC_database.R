find.read.IC.database <- function(dir = wd$team_labor$datensicherung$IC
                                  , date = NA # set manual date
                                  , time = NA # set manual time
                                  , order = NA #set manual order postion from newest to olders
                                  , subdat = T # return a smaller file
                                  , subdate = "2024-07-01" # return data which is >= subdate
){
  
  setwd(dir)
  setwd("./csv_Datenbank")
  
  files <- dir(pattern = "*IC_Datenbank.csv$")
  files.ctime <- file.info(files)$ctime
  files.order <- files[ order(file.info(files)$ctime) ]
  
  if( !is.na(date) & is.na(time)){
    
    files <- files[ substr(files, 1, 6) %in% date ]
    files.ctime <- files.ctime[ substr(files, 1, 6) %in% date ]
    files.order <- files.order[ substr(files, 1, 6) %in% date ]
    
  }
  
  if( !is.na(date) & !is.na(time)){
    
    files.order <- files[ substr(files, 1, 13) %in% paste0(date, "_", time) ]
    
  }
  
  if( !is.na(order)){
    
    files.order <- rev(files.order)
    files.order <- files.order[ order ]
    
  }
  
  # read database ####
  dat <- fread(files.order[ length( files.order )]
               , sep = ";", dec = ",", encoding = "Latin-1"
               , na.strings = "")
  dat$Bestimmungsstart <- as.POSIXct(dat$Bestimmungsstart, tz = "UTC")
  dat <- dat[ order(dat$Bestimmungsstart) , ]
  
  # Change column names ####
  colnames(dat) <- gsub("Anionen.", "", colnames(dat))
  colnames(dat) <- gsub("AHWP.", "", colnames(dat))
  
  if(subdat){
    subcol <- c("Bestimmungsstart","Ident", "Probentyp", "Methodenname"
                # , "Volumen"
                , "Verdünnung"
                , "Bestimmungs-ID", "Bestimmungsdauer [min]" #, "Nachbearbeitungsdatum"
                , grep("Info", colnames(dat), value = T)[ 1 ]
                # , grep("Wert", colnames(dat), value = T)
                , grep("Konzentration", colnames(dat), value = T)
                , grep("Kalibrierpunkte", colnames(dat), value = T)
                # , "Änderungskommentar Bestimmung"
    )
    subcol <- grep("mittelwert", subcol, invert = T, value = T)
    subcol <- grep("anteil", subcol, invert = T, value = T)
    
    if(any(!subcol %in% colnames( dat ))){
      warning(paste0("Column ", subcol[ !subcol %in% colnames( dat )], "not found in ", files.order[ length( files.order )]))
      subcol <- subcol[ subcol %in% colnames( dat )]
    }
    
    dat <- dat[ , ..subcol]
    
  }
  
  if(!is.na(subdate)){
    
    dat <- dat[ as.Date(dat$Bestimmungsstart) >= subdate , ]
    
  }
  
  # filter database ####
  # Convert character columns with numeric values to numeric
  numeric_columns <- dat[, lapply(.SD, function(col) all(grepl("^[0-9.]+$", col)))]
  numeric_columns[ , grep("Konzentration", colnames(dat), value = T)] <- T
  numeric_columns[ , grep("Kalibrierpunkte", colnames(dat), value = T)] <- T
  
  numeric_columns <- names(dat)[as.logical(unlist(numeric_columns))]
  
  dat[, (numeric_columns) := lapply(.SD, as.numeric), .SDcols = numeric_columns]
  
  # Identify columns that are not entirely empty strings
  non_empty_string_cols <- sapply(dat, function(col) !all( as.character(col) == ""))
  # Identify columns that are not entirely NA
  non_na_cols <- sapply(dat, function(col) !all(is.na(col)))
  
  # Combine the conditions to determine columns to keep
  cols_to_keep <- non_empty_string_cols & non_na_cols
  cols_to_keep <- names(cols_to_keep)
  # Subset the data.table to keep only the desired columns
  dat <- dat[, ..cols_to_keep]
  
  if(any(colnames(dat) %in% "Probentyp")) dat$Probentyp <- factor( dat$Probentyp )
  if(any(colnames(dat) %in% "Methodenname")) dat$Methodenname <- factor( dat$Methodenname )
  
  # return
  return(dat)
}