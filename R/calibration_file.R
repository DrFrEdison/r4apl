calibration.file <- function(instrument = instrument
                             , type = type
                             , date = datep
                             , path = wd$cal){
  
  path <- grep(instrument, path, value = T)
  cal.file <- dir(pattern = "\\.rds$", path = path)
  cal.file <- grep(type, cal.file, value = T)
  cal.file <- grep(date, cal.file, value = T)
  return( readRDS( file.path(path, cal.file)) )
  
}

calibration.matrix <- function(cal.file = cal.file){
  
  cal.model <- do.call(cbind, lapply(cal.file, function( x ) x$model))
  cal.matrix <- matrix(rep( 1:nrow(cal.model), ncol(cal.model) / 2)
                       , nrow = nrow(cal.model)
                       , ncol = ncol(cal.model) / 2)
  colnames(cal.matrix) <- names(cal.file)
  return(cal.matrix)
  
}
