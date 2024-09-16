# Function to calculate working hours
hack <- function(start_time
                 , ist_time = Sys.time()
                 , pause_time = NA
                 , Zeitkonto = NA) {
  
  # Convert times to POSIXct format for time calculations
  start_time_obj <- as.POSIXct(start_time, format = "%H:%M")
  ist_time_obj <- as.POSIXct(ist_time, format = "%H:%M")
  
  # pause
  if( is.na(pause_time) | pause_time <= 30){
    
    end_time.8_obj <- as.POSIXct(start_time, format = "%H:%M") + 60*60*8+60*30
    end_time.9_obj <- as.POSIXct(start_time, format = "%H:%M") + 60*60*9+60*15
    
    
  }
  
  if( !is.na(pause_time) & pause_time > 30) end_time.8_obj <- as.POSIXct(start_time, format = "%H:%M") + 60*60*8+60*pause_time
  if( !is.na(pause_time) & pause_time > 45) end_time.9_obj <- as.POSIXct(start_time, format = "%H:%M") + 60*60*9+60*pause_time
  if( !is.na(pause_time) & pause_time < 45) end_time.9_obj <- as.POSIXct(start_time, format = "%H:%M") + 60*60*9+60*15
  
  # difference in time in hours
  working_time <- as.numeric(difftime(ist_time_obj, start_time_obj, units = "h")) - ifelse(is.na(pause_time), 0, pause_time / 60)
  
  remaining_time.8 <- as.numeric(difftime(end_time.8_obj, ist_time_obj, units = "h"))
  remaining_time.9 <- as.numeric(difftime(end_time.9_obj, ist_time_obj, units = "h"))
  
  # 
  if(working_time <= 2){
    working_time <- working_time * 60
    working_time <- round(working_time, 0)
    working_time_unit <- "min"
  } else{ working_time_unit <- "h"}
  
  # 
  if(remaining_time.8 <= 2 & remaining_time.8 >= 0){
    remaining_time.8 <- remaining_time.8 * 60
    remaining_time.8 <- round(remaining_time.8, 0)
    remaining_time.8_unit <- "min"
  }else{ remaining_time.8_unit <- "h"}
  
  if(remaining_time.8 < 0){
    
    if(remaining_time.8 >= -2){
      remaining_time.8 <- remaining_time.8 * 60
      remaining_time.8 <- round(remaining_time.8, 0)
      remaining_time.8_unit <- "min"} else{ remaining_time.8_unit <- "h" }
    
    remaining_time.8_text <- "Überstunden"
  }else{ 
    remaining_time.8_text <- "zu hacken"}
  
  hack.heimkomm <- data.frame(Hack = c(start_time
                                       , strftime(end_time.8_obj, format = "%H:%M")
                                       , round(working_time, 1)
                                       , abs(round(remaining_time.8, 1)))
                              , . = c("Uhr", "Uhr", working_time_unit, remaining_time.8_unit))
  
  rownames(hack.heimkomm) <- c("Gekommen", "8 Stunden", "gehackt", remaining_time.8_text)
  
  if(!is.na(pause_time)){
    
    hack.heimkomm <- rbind(hack.heimkomm, Pause = c(pause_time, "min"))
    
  }
  
  
  if( !is.na(Zeitkonto)){
    
    interflex.time <- as.numeric(substr(Zeitkto(Zeitkonto), 1, gregexpr(" h ", Zeitkto(Zeitkonto)))) * 60 + as.numeric(substr(Zeitkto(Zeitkonto)
                                                                                                                              , unlist(gregexpr(" und ", Zeitkto(Zeitkonto))) + nchar(" und ")
                                                                                                                              , gregexpr(" Minuten", Zeitkto(Zeitkonto))))
    if(remaining_time.8_unit == "min"){ 
      
      if(working_time <= 6 & working_time_unit == "h") Zeitkonto <- remaining_time.8 * -1 + interflex.time + 30
      if(working_time > 6 & working_time_unit == "h") Zeitkonto <- remaining_time.8 * -1 + interflex.time
      
      if(working_time <= 60*6 & working_time_unit == "min") Zeitkonto <- remaining_time.8 * -1 + interflex.time / 60 + .5
      if(working_time > 60*6 & working_time_unit == "min") Zeitkonto <- remaining_time.8 * -1 + interflex.time / 60
      
      Zeitkonto <- round(Zeitkonto, 0)
    }
    if(remaining_time.8_unit == "h"){ 
      
      if(working_time <= 6 & working_time_unit == "h") Zeitkonto <- remaining_time.8 * -1 + interflex.time / 60 + .5
      if(working_time > 6 & working_time_unit == "h") Zeitkonto <- remaining_time.8 * -1 + interflex.time / 60
      
      if(working_time <= 60*6 & working_time_unit == "min") Zeitkonto <- remaining_time.8 * -1 + interflex.time / 60 + .5
      if(working_time > 60*6 & working_time_unit == "min") Zeitkonto <- remaining_time.8 * -1 + interflex.time / 60
      
    }
    
    if(Zeitkonto > 120 & remaining_time.8_unit == "min"){
      remaining_time.8_unit <- "h"
      Zeitkonto <- Zeitkonto / 60
    }
    
    hack.heimkomm <- rbind(hack.heimkomm, c(round(Zeitkonto,1), remaining_time.8_unit))
    rownames(hack.heimkomm)[ nrow( hack.heimkomm) ] <- "Zeitkonto"
  }
  
  return(hack.heimkomm)
}

# Zeitkto ####
Zeitkto <- function(Zeitkonto){
  
  
  if(!is.na(as.numeric(Zeitkonto))){
    if(nchar(Zeitkonto) == 1) return( paste0(as.numeric( Zeitkonto ), " h und 0 Minuten"))
    
    if(nchar(Zeitkonto) > 1) return(paste0(as.numeric(substr(Zeitkonto, 1, unlist(gregexpr("\\.", Zeitkonto)) - 1))
                                           , " h und "
                                           , round(as.numeric( paste0("0.", substr(Zeitkonto, unlist(gregexpr("\\.", Zeitkonto)) + 1, nchar(Zeitkonto)))) * 60, 0)
                                           , " Minuten"))
  }
  
  return( )
  if(unlist(gregexpr("\\,", Zeitkonto)) > 0)return(paste0(as.numeric(substr(Zeitkonto, 1, unlist(gregexpr("\\,", Zeitkonto)) - 1))
                                                          , " h und "
                                                          , round(as.numeric( paste0("0.", substr(Zeitkonto, unlist(gregexpr("\\,", Zeitkonto)) + 1, nchar(Zeitkonto)))) * 60, 0)
                                                          , " Minuten"))
  
  if(unlist(gregexpr("\\.", Zeitkonto)) > 0)return(paste0(as.numeric(substr(Zeitkonto, 1, unlist(gregexpr("\\.", Zeitkonto)) - 1))
                                                          , " h und "
                                                          , round(as.numeric( paste0("0.", substr(Zeitkonto, unlist(gregexpr("\\.", Zeitkonto)) + 1, nchar(Zeitkonto)))) * 60, 0)
                                                          , " Minuten"))
}
