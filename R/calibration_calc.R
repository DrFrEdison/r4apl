# instrument = "IC"
# dir(wd$cal[[ grep(instrument, wd$cal) ]], pattern = "\\.rds$")

calibration.calc <- function(y
                             , instrument = c("IC", "HPLC", "GC-MS")
                             , substance
                             , type = c("area", "heigth")
                             , Verdünnung = 1
                             , date
                             , sub = NA){
  
  # set date to datetime()
  if(  nchar(date) == 10) date = datetime(date)
  
  cal.rds <- dir(wd$cal[[ grep(instrument, wd$cal) ]], pattern = "\\.rds$")
  cal.rds <- grep(date, cal.rds, value = T)[ 1 ]
  
  cal.mod <- readRDS( paste0(wd$cal[[ grep(instrument, wd$cal) ]], "/", cal.rds))
  cal.mod <- cal.mod[[ which( names(cal.mod) == substance ) ]]
  
  if( any( !is.na(sub))){
    cal.mod <- lm(cal.mod$model$area[ sub ] ~ cal.mod$model$conc[ sub ] )
  }
  
  
  final.concentration <- (y - as.numeric(cal.mod$coefficients[ 1 ])) / as.numeric(cal.mod$coefficients[ 2 ]) * Verdünnung
  return(final.concentration)
  
}