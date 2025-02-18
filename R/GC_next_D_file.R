next_D_file <- function(path = "D:/BDLab/", last = F, dirs = 5, return_dir = F){
  
  last_x_dirs <- tail(list.dirs(path, recursive = F), n = dirs)
  last_D_filepaths <- unlist(lapply(last_x_dirs, function( x ) list.dirs( x, recursive = F)))
  last_D_files <- basename(last_D_filepaths)
  
  last_D_files <- grep("\\.D$", last_D_files, value = T)
  last_D_filepaths <- grep("\\.D$", last_D_filepaths, value = T)
  
  last_D_IDs <- substr(last_D_files, 6, nchar(last_D_files) - 2)
  last_D_IDs <- sort(last_D_IDs)
  
  if(!last){ last_D_IDs_max <- max(substr(last_D_files, 6, nchar(last_D_files) - 2))
  next_D_ID <- formatC(as.numeric(last_D_IDs_max) + 1
                       , width = 4
                       , format = "d"
                       , flag = "0")
  next_D_file_name <- paste0(year(Sys.time()), "_", next_D_ID)
  return <- next_D_file_name
  }
  
  if(is.numeric(last) & !return_dir) return <- tail(last_D_files, last)
  if(is.numeric(last) & return_dir) return <- tail(last_D_filepaths, last)
  
  return(return)
}
