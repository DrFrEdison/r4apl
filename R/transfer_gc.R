transfer.ms <- function(dat
                        , mass.pattern = "m[0-9]"
                        , time.pattern = "time_MS"){
  
  mass.col <- grep(mass.pattern, colnames(dat), value = F)
  mass.abs <- grep(mass.pattern, colnames(dat), value = T)
  mass.abs <- as.numeric(gsub(gsub("\\[0\\-9\\]", "", mass.pattern), "", mass.abs))
  
  time.abs <- dat[ , get(time.pattern)]
  
  returnlist <- list()
  
  returnlist$time <- time.abs
  returnlist$mass <- mass.abs
  returnlist$mz <- dat[ , mass.col, with = F]
  returnlist$mz.mtx <- as.matrix(returnlist$mz)
  returnlist$mz.log10.p1.mtx <- log10(as.matrix(returnlist$mz.mtx)+1)
  returnlist$normalized.by.row <- t(apply(returnlist$mz.mtx, 1, function(x) x / max(x, na.rm = TRUE)))
  mz_mtx_norm <- t(apply(returnlist$mz.mtx, 1, function(x) x / max(x, na.rm = TRUE)))
  
  return(returnlist)
  
}

transfer.tcd <- function(tcd1A = NA
                         , tcd2B = NA
                         , colnames = c("time", "TCD1A", "TCD2B")){
  
  
  # merge TCD data when both have a time difference of 0 and the same number of rows
  if(nrow(tcd1A) == nrow(tcd2B)){
    
    if(unique(tcd1A$time_TCD1A - tcd2B$time_TCD2B) == 0){
      
      tcd.merge <- cbind(tcd1A, tcd2B$signal_TCD2B)
      colnames(tcd.merge) <- colnames
      return(tcd.merge)
      
    }
  }
  
  if(nrow(tcd1A) != nrow(tcd2B)){
    
    setkey(tcd1A, time_TCD1A)
    setkey(tcd2B, time_TCD2B)
    
    # Perform a rolling join using the nearest time points
    tcd.merge <- tcd1A[tcd2B, roll = "nearest"]
    
    colnames(tcd.merge) <- colnames
    return(tcd.merge)
    
  }
}
