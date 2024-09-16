datetime <- function(datetime = Sys.time(), time = F){
  if(!time) return(gsub("\\-", "", substr(datetime, 3, 10)))
  if(time) return(gsub("\\ ", "_", gsub("\\:", "", gsub("\\-", "", substr(datetime, 3, 19)))))
  
}

kw <- function(date = Sys.Date()){ data.table::isoweek(date)}

analysis.time <- function(sample.duration
                          , samples.to.go
                          , time = Sys.time()
                          , unit = "h"){
  
  samples.ready <- sample.duration * samples.to.go * 60 + time
  if( unit == "h") time.to.go <- as.numeric(difftime(samples.ready, time, units = "hours"))
  if( unit == "min") time.to.go <- as.numeric(difftime(samples.ready, time, units = "min"))
  if(unit != "h" & unit != "min") time.to.go <- as.numeric(difftime(samples.ready, time, units = unit))
  
  time.to.go <- round(time.to.go, 1)
  
  if(as.Date(samples.ready) == as.Date(time)) return(paste0("Fertig heute um ", strftime(samples.ready, format = "%H:%M"), ", noch ", time.to.go, " ", unit))
  if(as.Date(samples.ready) == as.Date(time) + 1) return(paste0("Fertig morgen um ", strftime(samples.ready, format = "%H:%M"), ", noch ", time.to.go, " ", unit))
  if(as.Date(samples.ready) == as.Date(time) + 2) return(paste0("Fertig übermorgen um ", strftime(samples.ready, format = "%H:%M"), ", noch ", time.to.go, " ", unit))
  return(paste0("Fertig um ", strftime(samples.ready, format = "%H:%M"), " am ", format(as.Date(samples.ready), "%d.%m"), ", noch ", time.to.go, " ", unit))
  
}

subscript_formula <- function(formula, bold = T) {
  # Simple case: only numbers at the end
  if (grepl("\\d+$", formula)) {
    numbers <- str_extract(formula, "\\d+$")
    base <- str_remove(formula, "\\d+$")
    if(bold) return(parse(text = paste0("bold(", base, "[", numbers, "])")))
    if(!bold) return(parse(text = paste0(base, "[", numbers, "]")))
  } else {
    # More complex cases: handle as needed
    # For example, consider using chemical nomenclature libraries or custom rules
    return(formula)
  }
}

head10 <- function(x, nrowp = 10, ncolp = 1:10, tail = F){
  
  if(length( ncolp ) == 1) if(ncolp == "all") ncolp <- 1 : ncol( x )
  if(length(ncolp) == 1) ncolp <- 1 : ncolp
  
  if(nrowp > nrow(x)) nrowp <- nrow(x)

  if(min(ncolp) > ncol(x)) ncolp[ 1 ] <- ncol(x)
  if(max(ncolp) > ncol(x)) ncolp[ which(ncolp > ncol(x)) ] <- ncol(x)
  ncolp <- sort( unique( ncolp))
  
  if(!tail){
    
    if(is.data.table(x)) return( head(x[ , ncolp, with = F], nrowp) )
    if(!is.data.table(x)) return( head(x[ , ncolp], nrowp) )
    
  }
  if(tail){
    
    if(is.data.table(x)) return( tail(x[ , ncolp, with = F], nrowp) )
    if(!is.data.table(x)) return( tail(x[ , ncolp], nrowp) )
    
  }
}

rangexy <- function(xy, p = c(5, 5)) {
  
  # Calculate the range of the data
  range.xy <- range(xy, na.rm = TRUE)
  
  # Ensure p is a two-element vector
  if (length(p) == 1) p <- c(p, p)
  
  # Calculate the percentage change for the lower limit
  p[1] <- ifelse(range.xy[1] > 0, (100 - p[1]) / 100, (100 + p[1]) / 100)
  
  # Calculate the percentage change for the upper limit
  p[2] <- ifelse(range.xy[2] > 0, (100 + p[2]) / 100, (100 - p[2]) / 100)
  
  # Adjust the lower limit
  range.xy[1] <- range.xy[1] * p[1]
  
  # Adjust the upper limit
  range.xy[2] <- range.xy[2] * p[2]
  
  return(range.xy)
}

opendir <- function(dir = getwd()){
  if (.Platform['OS.type'] == "windows"){
    # if( length( grep("DC-01", dir) > 0 )) dir <- paste0(Sys.getenv("OneDriveCommercial"), substr(dir, gregexpr( "/", dir)[[ 1 ]][[ 3 ]], nchar(dir)))
    shell.exec(gsub("\\/", "\\\\", dir))
  } else {
    system(paste(Sys.getenv("R_BROWSER"), dir))
  }
}

.onLoad <- function(libname, pkgname) {
  package_version <- packageDescription(pkgname)$Version
  version_name <- packageDescription(pkgname)$VersionName
  
  message(paste(pkgname, "version", package_version, "--", version_name))
  message("Check the GitHub repository at: https://github.com/DrFrEdison/r4dt")
  
  # Load any other necessary initialization code here
  
  # Make sure to return(TRUE) at the end
  return(TRUE)
}

# moveme function ####
moveme <- function (invec, movecommand) {
  movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]],
                                 ",|\\s+"), function(x) x[x != ""])
  movelist <- lapply(movecommand, function(x) {
    Where <- x[which(x %in% c("before", "after", "first",
                              "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  myVec <- invec
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])
    A <- movelist[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp) - 1
      }
      else if (A == "after") {
        after <- match(ba, temp)
      }
    }
    else if (A == "first") {
      after <- 0
    }
    else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }
  myVec
}