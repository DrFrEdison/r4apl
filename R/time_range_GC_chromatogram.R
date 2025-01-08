time.range.GC.chromatogram <- function(dat = GC_MS$raw){

  timerange <- dat %>%
    lapply(function(gc) {
      gc %>%
        lapply(function(time) {
          time %>%
            # Extract time column based on its name
            colnames() %>%
            grep("time", ., value = TRUE) %>%
            { time[, get(.)] } %>%
            # Calculate time range, convert to minutes, and round the difference
            { range(.) / 60 } %>%
            diff() %>%
            round(2)
        }) %>%
        # Flatten the list and return unique time differences
        unlist() %>%
        unique() %>%
        sort()
    })

  return(timerange)

}

clean_gc_data <- function(GC_MS, time_range = list(GC_MS = 0.2, GC_TCD1A = 0.25, GC_TCD2B = 0.25)) {

  # Check and remove invalid GC_MS entries
  if(!is.null(GC_MS$raw$GC_MS)){

    remove_ms <- which(unlist(lapply(GC_MS$raw$GC_MS, function(x) {
      round(diff(range(x$time_MS / 60)), 2) < time_range$GC_MS
    })))
    if (length(remove_ms) > 0) {
      GC_MS$database <- GC_MS$database[-which(GC_MS$database$detector == "GC_MS")[remove_ms], ]
      GC_MS$raw$GC_MS <- GC_MS$raw$GC_MS[-remove_ms]
    }
  }

  # Check and remove invalid GC_TCD1A entries
  if(!is.null(GC_MS$raw$GC_TCD1A)){

    remove_TCD1A <- which(unlist(lapply(GC_MS$raw$GC_TCD1A, function(x) {
      round(diff(range(x$time_TCD1A / 60)), 2) < time_range$GC_TCD1A
    })))
    if (length(remove_TCD1A) > 0) {
      GC_MS$database <- GC_MS$database[-which(GC_MS$database$detector == "GC_TCD1A")[remove_TCD1A], ]
      GC_MS$raw$GC_TCD1A <- GC_MS$raw$GC_TCD1A[-remove_TCD1A]
    }

  }
  # Check and remove invalid GC_TCD2B entries
  if(!is.null(GC_MS$raw$GC_TCD2B)){

    remove_TCD2B <- which(unlist(lapply(GC_MS$raw$GC_TCD2B, function(x) {
      round(diff(range(x$time_TCD2B / 60)), 2) < time_range$GC_TCD2B
    })))
    if (length(remove_TCD2B) > 0) {
      GC_MS$database <- GC_MS$database[-which(GC_MS$database$detector == "GC_TCD2B")[remove_TCD2B], ]
      GC_MS$raw$GC_TCD2B <- GC_MS$raw$GC_TCD2B[-remove_TCD2B]
    }
  }

  return(GC_MS)
}
