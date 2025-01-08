identify_compound <- function(file.name){
  
  ident <- list()
  ident[[ 1 ]] <- unlist( lapply(chemicals.master$Compound, function( x ) length(grep(x, file.name))))
  ident[[ 1 ]] <- ident[[ 1 ]][ which( !is.na(chemicals.master$Compound)) ]
  
  ident[[ 2 ]] <- unlist( lapply(chemicals.master$Compound_Alternative, function( x ) length(grep(x, file.name))))
  ident[[ 2 ]] <- ident[[ 2 ]][ which( !is.na(chemicals.master$Compound_Alternative)) ]
  
  ident[[ 3 ]] <- unlist( lapply(chemicals.master$Compound_Alternative_2, function( x ) length(grep(x, file.name))))
  ident[[ 3 ]] <- ident[[ 3 ]][ which( !is.na(chemicals.master$Compound_Alternative_2)) ]
  
  ident[[ 4 ]] <- unlist( lapply(chemicals.master$German.Name, function( x ) length(grep(x, file.name))))
  ident[[ 4 ]] <- ident[[ 4 ]][ which( !is.na(chemicals.master$German.Name)) ]
  
  ident[[ 5 ]] <- unlist( lapply(chemicals.master$Formula, function( x ) length(grep( paste0("_", x, "\\."), file.name))))
  ident[[ 5 ]] <- ident[[ 5 ]][ which( !is.na(chemicals.master$Formula)) ]
  
  rowp <- list()
  rowp[[ 1 ]] <- if(any(ident[[ 1 ]]) > 0) which( !is.na(chemicals.master$Compound))[ which(ident[[ 1 ]] > 0) ]
  rowp[[ 2 ]] <- if(any(ident[[ 2 ]]) > 0) which( !is.na(chemicals.master$Compound_Alternative))[ which(ident[[ 2 ]] > 0) ]
  rowp[[ 3 ]] <- if(any(ident[[ 3 ]]) > 0) which( !is.na(chemicals.master$Compound_Alternative_2))[ which(ident[[ 3 ]] > 0) ]
  rowp[[ 4 ]] <- if(any(ident[[ 4 ]]) > 0) which( !is.na(chemicals.master$German.Name))[ which(ident[[ 4 ]] > 0) ]
  rowp[[ 5 ]] <- if(any(ident[[ 5 ]]) > 0) which( !is.na(chemicals.master$Formula))[ which(ident[[ 5]] > 0) ]
  
  rowp <- unique( unlist( rowp))
  
  return( chemicals.master[rowp, ] )
}