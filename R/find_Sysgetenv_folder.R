find.Sys.getenv.folder <- function(x = "LOCALAPPDATA", pattern = "Teckso", pattern2 = "GmbH"){
  
  dir.x <- Sys.getenv(x)
  x.dir <- dir(path = dir.x, pattern = pattern)
  x.dir <- paste0(dir.x, "\\", x.dir)
  
  if( length( x.dir ) == 1) return(x.dir)
  
  if( length( x.dir ) > 1){
    
    x.dir <- grep(pattern2, x.dir, value = T)[ 1 ]
    return(x.dir)
    
  }
  if( length( x.dir ) == 0){
    
    warning("No folder found")
    
  }
}