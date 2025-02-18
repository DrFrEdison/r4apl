plot.MS <- function(dat, c = 0, type = c("relative", "absolute"), MS = c("MS", "SIM")){
  
  MS <- dat[[ grep(MS, names(dat)) ]]
  x <- MS$time
  
  y <- apply(MS$mz, 1, function( x ) sum(x[ MS$mass >= c] ))
  if(type == "relative") y <- y / max(y) * 100
  plot(x,y, type = "l")
  
}

plot.MS <- function(dat, c = 0, type = c("relative", "absolute")){
  
  MS <- dat[[ grep("SIM", names(dat)) ]]
  x <- MS$time
  
  y <- apply(MS$mz, 1, function( x ) sum(x[ MS$mass >= c] ))
  if(type == "relative") y <- y / max(y) * 100
  plot(x,y, type = "l")
  
}