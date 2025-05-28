fastplot_gc <- function(ID
                        , detector = c("MS", "TCD1A", "TCD2B")
                        , date = NULL
                        , dir = NULL
                        , method = NULL
                        , xlim = list(c(0, 20), c(6, 10), c(6, 20))
                        , ylim = list(NULL, NULL, NULL)){

  db <- read_instrument_db(instrument = instrument
                           , subdate = "2024-01-01")
  db <- db[ !duplicated(db) , ]

  if(!is.null(dir)){
    w.subset <- which(db$dir == dir)
    db <- db[ w.subset, ]
  }
  if(!is.null(date)){
    w.subset <- which(as.Date(db$datetime) == date)
    db <- db[ w.subset, ]
  }
  if(!is.null(method)){
    w.subset <- which(db$method == method)
    db <- db[ w.subset, ]
  }

  if(length( which(db$data_file == ID) ) > 1) warning("Duplicated ID, date | method | dir needed, first match is plotted")

  db <- db[ which(db$data_file == ID)[ 1 ] , ]
  raw <- read_gc_chromatogram(db, instrument = "GC_MS", detector = detector)
  trs <- transfer_gc_data(raw)[[ 1 ]]

  par(mfrow = parmfrow(length(detector)))
  for(i in 1 : length(detector)){

    if( length(grep("MS", detector[ i ])) > 0){
      RT <- trs[[ i ]]$time
      Intensity <- apply(trs$MS$mz, 1, sum)
    }

    if( length(grep("TCD1A", detector[ i ])) > 0){

      RT <- trs[[ grep("TCD", names(trs)) ]]$RT
      Intensity <- trs[[ grep("TCD", names(trs)) ]]$TCD1A
    }

    if( length(grep("TCD2B", detector[ i ])) > 0){
      RT <- trs[[ grep("TCD", names(trs)) ]]$RT
      Intensity <- trs[[ grep("TCD", names(trs)) ]]$TCD2B
    }


    plot(RT, Intensity, type = "l"
         , main = detector[ i ]
         , xlim = xlim[[ i ]]
         , ylim = ylim[[ i ]])

  }

}
