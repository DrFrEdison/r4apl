transfer_gc_data <- function(gc_data){

  for (i in seq_along(gc_data)) {
    name_sub <- names(gc_data[[i]])

    if (any(name_sub %in% "MS")) {
      gc_data[[i]][[which(name_sub == "MS")]] <- transfer_ms(dat = gc_data[[i]][[which(name_sub == "MS")]])
    }
    if (any(name_sub %in% "SIM")) {
      gc_data[[i]][[which(name_sub == "SIM")]] <- transfer_ms(dat = gc_data[[i]][[which(name_sub == "SIM")]])
    }

    if (any(name_sub %in% "TCD1A") & any(name_sub %in% "TCD2B")) {
      gc_data[[i]]$TCD <- transfer_tcd(
        TCD1A = gc_data[[i]][[which(name_sub == "TCD1A")]],
        TCD2B = gc_data[[i]][[which(name_sub == "TCD2B")]]
      )

      gc_data[[i]]$TCD1A <- NULL
      gc_data[[i]]$TCD2B <- NULL

    }

    if (!any(name_sub %in% "TCD1A") & any(name_sub %in% "TCD2B")) {
      gc_data[[i]]$TCD <- gc_data[[i]][[which(name_sub == "TCD2B")]] <- transfer_tcd(
        TCD1A = NA,
        TCD2B = gc_data[[i]][[which(name_sub == "TCD2B")]]
      )
      gc_data[[i]]$TCD1A <- NULL
      gc_data[[i]]$TCD2B <- NULL
    }
  }

  return(gc_data)
}
