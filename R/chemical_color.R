chemical_color <- function(chemicals, xlsx = chemicals.master){
  
  if(!is.factor(chemicals)) stop("Compound list is not a factor")
  
  chemicals.levels <- levels( chemicals)
  
  col.legend <- matrix(NA, length( chemicals.levels), 1)
  col.single <- matrix(NA, length( chemicals), 1)
  
  for(i in 1 : length( chemicals.levels)){
    
    if( length( which( xlsx$Formula %in% chemicals.levels[ i ])) == 1){
      
      col.legend[ i, 1] <- xlsx$color[ which( xlsx$Formula %in% chemicals.levels[ i ]) ]
      col.single[ which(chemicals %in% chemicals.levels[ i ]), 1] <- col.legend[ i ]
      
    }
    
    if( length( which( xlsx$Compound %in% chemicals.levels[ i ])) == 1){
      
      col.legend[ i, 1 ] <- xlsx$color[ which( xlsx$Compound %in% chemicals.levels[ i ]) ]
      col.single[ which(chemicals %in% chemicals.levels[ i ]), 1 ] <- col.legend[ i ]
      
    }
    
    
    if( length( which( xlsx$German.Name %in% chemicals.levels[ i ])) == 1){
      
      col.legend[ i, 1 ] <- xlsx$color[ which( xlsx$German.Name %in% chemicals.levels[ i ]) ]
      col.single[ which(chemicals %in% chemicals.levels[ i ]), 1 ] <- col.legend[ i ]
      
    }
    
    if( length( which( xlsx$Compound_Alternative %in% chemicals.levels[ i ])) == 1){
      
      col.legend[ i, 1 ] <- xlsx$color[ which( xlsx$Compound_Alternative %in% chemicals.levels[ i ]) ]
      col.single[ which(chemicals %in% chemicals.levels[ i ]), 1 ] <- col.legend[ i ]
      
    }
    
    if(is.na(col.legend[ i, 1 ])){
      col.legend[ i, 1 ]<- sample(rainbow(100), 1)
      col.single[ which(chemicals %in% chemicals.levels[ i ]), 1 ] <- col.legend[ i ]
    } 
    
  }
  
  returnlist <- list(c(col.legend), c(col.single))
  names(returnlist) <- c("legend", "single")
  return(returnlist)
}

