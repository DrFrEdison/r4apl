read_GC_TCD_calibration <- function(date.pattern, cal.dir, sub = T){

  is.dir <- getwd()
  setwd(cal.dir)

  files <- dir( pattern = "\\.rda$")
  files <- grep(date.pattern, files, value = T)

  date <- as.Date( substr(files, 1, 6), format = "%y%m%d")

  detector <- substr(files
                     , lapply(gregexpr("_", files), function( x ) x[[1]] + 1)
                     , lapply(gregexpr("_", files), function( x ) x[[2]] - 1))

  substance <- substr(files
                      , lapply(gregexpr("_", files), function( x ) x[[2]] + 1)
                      , lapply(gregexpr("\\.rda", files), function( x ) x[[1]] - 1))
  substance <- gsub("\\-", "\\.", substance)

  calibration <- list()

  for(i in seq_along(substance)){

    load(files[ i ])

    calibration[[ i ]] <- export

    names(calibration[[ i ]]) <- c("lm", "parameter")
    rm(export)
  }
  names( calibration ) <- substance

  setwd(is.dir)

  tibble.calibration <- tibble(do.call(rbind, lapply(calibration, \(x) x$parameter)))
  tibble.calibration <- tibble.calibration %>% mutate(lm = lapply(calibration, \(x) x$lm))
  tibble.calibration$detector <- factor(tibble.calibration$detector)

  if(sub) tibble.calibration <- tibble.calibration[ , !grepl("S[0-9]$", colnames(tibble.calibration))]
  if(sub) tibble.calibration <- tibble.calibration[ , !grepl("A[0-9]$", colnames(tibble.calibration))]

  tibble.calibration <- tibble.calibration %>% arrange(detector, RT)
  return(tibble.calibration)
}
