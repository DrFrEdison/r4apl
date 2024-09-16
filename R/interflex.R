# Create a function to replace German day abbreviations with English equivalents
replace_german_days <- function(date_string) {
  date_string <- gsub("Mo\\.", "Mon", date_string)
  date_string <- gsub("Di\\.", "Tue", date_string)
  date_string <- gsub("Mi\\.", "Wed", date_string)
  date_string <- gsub("Do\\.", "Thu", date_string)
  date_string <- gsub("Fr\\.", "Fri", date_string)
  date_string <- gsub("Sa\\.", "Sat", date_string)
  date_string <- gsub("So\\.", "Sun", date_string)
  return( as.character(date_string))
}

interflex.Clipboard <- function(dat.cb = readClipboard()){
  
  input_string <- dat.cb[ grep("Journal", dat.cb) ]
  
  if(length( input_string ) == 0) return(NULL)
  
  pattern <- "([0-9]{2})/([0-9]{4})"
  month.year <- stri_match_first_regex(input_string, pattern)
  
  dat.trs <- dat.cb[ grep("Datum", dat.cb) : length( dat.cb)]
  dat.trs <- dat.trs[ - grep("Fehlzeitenfenster ", dat.trs) ]
  
  to.append <- list()
  for(i in 1 : (length(dat.trs))){
    
    # Check if i is a date in the form of "Mo. 01" and the next element is a KW, e.g. 27
    # See Interflex for e.g. first row in a month. This is not correctly read with readClipboard
    if(i != length(dat.trs)) if( all(grepl("^[A-Za-z]{2}\\. \\d{2}\\.$", dat.trs[ i ])
                                     , !grepl("^\\d+$", dat.trs[ i + 1 ] ))){
      
      to.append[[ i ]] <- 1
    }
  }
  
  for(i in rev(which(unlist(lapply(to.append, length)) > 0))){
    dat.trs <- append(dat.trs, " ", after = i)
  }
  
  lines <- unlist(strsplit(dat.trs, "\ "))
  columns <- dat.trs[ 1 : grep("Zeitkto", dat.trs)]
  dat.trs <- dat.trs[ - which(dat.trs %in% columns) ]
  
  interflex.df <- matrix(dat.trs, ncol = 10, byrow = T)
  interflex.df <- data.frame(interflex.df)
  colnames(interflex.df) <- columns
  
  interflex.df$Soll <- as.numeric( gsub("\\,", "\\.", interflex.df$Soll))
  interflex.df$Ist <- as.numeric( gsub("\\,", "\\.", interflex.df$Ist))
  interflex.df$Zeitkto <- as.numeric( gsub("\\,", "\\.", interflex.df$Zeitkto))
  
  # Datum ####
  Dateraw <- interflex.df$Datum[ which(nchar(interflex.df$Datum)>1) ]
  Date <- paste(month.year[ , 3]
                , month.year[ , 2]
                , gsub("[^0-9]", "", interflex.df$Datum[ which(nchar(interflex.df$Datum)>1) ])
                , sep = "-")
  Date <- as.Date(Date)
  interflex.df$datetime <- NA
  for(i in seq_along(Dateraw)){
    
    
    if(i != max(seq_along(Dateraw))){
      
      rangep <- which(interflex.df$Datum == Dateraw[ i ]) : 
        (which(interflex.df$Datum == Dateraw[ i + 1 ]) - 1)
      
      interflex.df$datetime[ rangep ] <- as.character(Dateraw[ i ])
      
    }else{
      
      rangep <- which(interflex.df$Datum == Dateraw[ i ]) : 
        ( length(interflex.df$Datum))
      
      interflex.df$datetime[ rangep ] <- as.character(Dateraw[ i ])
      
    }
  }
  
  interflex.df$datetime <- as.character( sapply(interflex.df$datetime, replace_german_days) )
  interflex.df$datetime <- as.Date(paste(month.year[ , 3]
                                         , month.year[ , 2]
                                         , interflex.df$datetime
                                         , sep = "-"), format = "%Y-%m-%a %d.")
  interflex.df$Datum <- interflex.df$datetime
  interflex.df$datetime <- NULL
  # write interflex
  Date <- max(interflex.df$Datum)
  
  fwrite(interflex.df
         , paste0(wd$D$data$interflex, substr(datetime(as.character(Date)), 1, 4), "_interflex.csv")
         , row.names = F, sep = ";", dec = ",")
}

# Nebenzeiteneintrag ####
interflex.Nebenzeiteneintrag <- function(interflex.files = interflex.files
                                         , month = month(Sys.Date())
                                         , year = year(Sys.Date())){
  
  year <- substr(year, 3, 4)
  month <- formatC(x = month, digits = 2, width = 2, flag = 0)
  interflex.files <- interflex.files[ substr(interflex.files, 1, 4) %in% paste0(year, month)]
  interflex.files.read <- fread(paste0(wd$D$data$interflex, interflex.files))
  interflex.files.read <- interflex.files.read[ nchar(interflex.files.read$Ist) > 0 , ]
  
  interflex.files.read <- interflex.files.read[ interflex.files.read$Fehlgrund != "Urlaub", ]
  interflex.files.read <- interflex.files.read[ interflex.files.read$Ist != "0" , ]
  
  Nebenzeiten <- list()
  Nebenzeiten$date <- interflex.files.read$Datum
  Nebenzeiten$date.all <- Nebenzeiten$date[ nchar(Nebenzeiten$date) > 4]
  Nebenzeiten$date <- Nebenzeiten$date[ which( !is.na( interflex.files.read$Ist)) ]
  Nebenzeiten$Nebenzeiteneintrag <- interflex.files.read$Ist[ which( !is.na( interflex.files.read$Ist)) ]
  Nebenzeiten$date <- Nebenzeiten$date[ !Nebenzeiten$Nebenzeiteneintrag %in% 0 ]
  Nebenzeiten$Nebenzeiteneintrag <- Nebenzeiten$Nebenzeiteneintrag[ !Nebenzeiten$Nebenzeiteneintrag %in% 0 ]
  
  # Apply the function to replace day abbreviations
  Nebenzeiten$date <- as.character( sapply(Nebenzeiten$date, replace_german_days) )
  Nebenzeiten$date.all <- as.character( sapply(Nebenzeiten$date.all, replace_german_days) )
  
  interflex.files.read$datetime <- NA
  unique.date <- unique(interflex.files.read$Datum)
  unique.date <- unique.date[ nchar(unique.date) > 2]
  
  for(i in seq_along(Nebenzeiten$date.all)){
    
    
    if(i != max(seq_along(Nebenzeiten$date.all))){
      
      rangep <- which(interflex.files.read$Datum == unique.date[ i ]) : 
        (which(interflex.files.read$Datum == unique.date[ i + 1 ]) - 1)
      
      interflex.files.read$datetime[ rangep ] <- as.character(Nebenzeiten$date.all[ i ])
      
    }else{
      
      rangep <- which(interflex.files.read$Datum == unique.date[ i ]) : 
        ( length(interflex.files.read$Datum))
      
      interflex.files.read$datetime[ rangep ] <- as.character(Nebenzeiten$date.all[ i ])
      
    }
  }
  
  Nebenzeiten$weekdays <- factor(weekdays(as.Date(Nebenzeiten$date.all)), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  returnlist <- list( gsub("\\.", "\\,", Nebenzeiten$Nebenzeiteneintrag)
                      , Nebenzeiten$date.all
                      , Nebenzeiten$weekdays
                      , interflex.files.read)
  names(returnlist) <- c("Nebenzeiteneintrag", "Datum", "Wochentage", "interflex.date")
  
  return( returnlist )
}

interflex_Zeitkto_Beginn <- function(dat1 = interflex.out){
  
  ist.path <- getwd()
  Zeitkto_Beginn_IST = dir(path = wd$r4apl$master, pattern = "interflex_Zeitkto_")
  Zeitkto_Beginn_SOLL =   paste0("interflex_Zeitkto"
                                 , paste0("_", dat1$Zeitkto, "_")
                                 , "Beginn_"
                                 , gsub("\\:", "", dat1$Beginn))
  setwd(wd$r4apl$master)
  file.rename(from = Zeitkto_Beginn_IST
              , to = Zeitkto_Beginn_SOLL)
  setwd(ist.path)
}


