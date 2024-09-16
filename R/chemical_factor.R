chemical_factor <- function(chemicals
                            , retention.time = NA
                            , xlsx = chemicals.master){
  
  if(length(retention.time) != 1 && !any(is.na(retention.time))){
    retention.time.unique <- unique(retention.time)
    chemicals.order <- chemicals[ order(retention.time)]
    chemicals.level.order <- unique( chemicals.order )
  }
  
  chemicals.unique <- unique(chemicals)
  
  level.position <- c()
  
  if(!length(retention.time) != 1 && any(is.na(retention.time))){
    for(i in 1:length( chemicals.unique )){
      
      if( length( which( xlsx$Formula %in% chemicals.unique[ i ])) == 1){
        level.position[ i ] <- which( xlsx$Formula %in% chemicals.unique[ i ])
      }
      
      if( length( which( xlsx$Compound %in% chemicals.unique[ i ])) == 1){
        level.position[ i ] <- which( xlsx$Compound %in% chemicals.unique[ i ])
      }
      
      
      if( length( which( xlsx$German.Name %in% chemicals.unique[ i ])) == 1){
        level.position[ i ] <- which( xlsx$German.Name %in% chemicals.unique[ i ])
      }
      
      if( length( which( xlsx$Compound_Alternative %in% chemicals.unique[ i ])) == 1){
        level.position[ i ] <- which( xlsx$Compound_Alternative %in% chemicals.unique[ i ])
      }
      
      if(is.null(level.position))
        level.position[ i ] <- i + nrow( xlsx )
      
      if(is.na(level.position[ i ]))
        level.position[ i ] <- i + nrow( xlsx )
    }
    
    # Case if chemical is not found in excel file
    # send warning and put it to the end
    if( length( to.long <- which( level.position > nrow( xlsx )) ) > 0){
      warning( paste0(paste(chemicals.unique[ to.long ], collapse = " and ")
                      , " not found in "
                      , basename( chemicals.master.file )))
      
      if( length( seq_along(level.position[ to.long ]) ) > 1)
        for(k in seq_along(level.position[ to.long ])){
          level.position[ to.long ][ order(level.position[ to.long ])[ k ]] <- max(level.position[ -to.long ]) + k
          
        }
    }  
    chemicals.level.order <- chemicals.unique[ order(level.position) ]
  }
  chemicals <- factor(chemicals, levels = chemicals.level.order)
  
  return(chemicals)
  
}


