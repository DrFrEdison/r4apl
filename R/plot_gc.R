marp <- c(6,10,4,10)
linep.x <- 3.5
linep.y <- 7
linep.main <- 1
cex.axisp <- 1.6
par(cex = 2)

plot_mz_at_time <- function(ms.dat
                            , time){

  time.select <- which.min( abs( ms.dat$time - time))

  par(mar = marp)
  plot(x1 <- ms.dat$mass
       , y1 <- as.numeric(ms.dat$mz[ time.select])
       , type = "n", xlab = "", ylab = "", axes = F)

  options(scipen = 999)
  y1 <- ifelse(y1 == 0, NA, y1)
  segments(x1, 0, x1, y1
           , col = r4apl$col[ 1 ])
  box()
  axis(1, cex.axis = cex.axisp)
  mtext("m/z", 1, linep.x)
  axis(2, las = 2, cex.axis = cex.axisp)
  mtext("Ion Current", 2, linep.y)

  mtext(paste0("Mass spectra at time = ", time, " min"), side = 3, linep.main)
}

plot_mz_over_time <- function(ms.dat
                              , mz
                              , substance = NA){

  # if(is.na(substance)) substance = mz_identify(mz)

  colp <- r4apl$col[1:length(mz)]

  mz <- mz[ mz %in% ms.dat$mass ]

  y <- data.table(ms.dat$mz.mtx[ , which(ms.dat$mass %in% mz)])
  ylimp <- rangexy(y)

  par(mar = marp)
  plot(x <- ms.dat.trs$time
       , as.numeric( unlist( y[ , 1, with = F]))
       , type = "n"
       , xlab = "", ylab = "", axes = F
       , ylim = ylimp)


  box()
  axis(1, cex.axis = cex.axisp)
  mtext("Retention time in Minutes", 1, linep.x)
  axis(2, las = 2, cex.axis = cex.axisp)
  mtext("Ion Current", 2, linep.y)

  for(i in 1 : ncol(y)){

    lines(x, as.numeric( unlist( y[ , i, with = F])), col = colp[ i ])

  }

  xxx <- seq(1, length(x), len = length(mz) + 2)[1 : length(mz)]

  for(i in seq_along(mz))
  mtext(bquote("mz =" ~ .(mz[i]))
        , side = 3
        , line = linep.main
        , at = x[ xxx[ i ]], col = colp[i], cex = cex.axisp)  # First part in green


  # legend(par("usr")[2], par("usr")[4]
  #        , paste0("m/z = ", as.character(mz))
  #        , lty = 1, col = colp, xpd = T, bty = "n")
  #

}


plot_tcd <- function(tcd.dat
                     , signal1 = "TCD1A"
                     , signal2 = "TCD2B"){

  par(mar = marp)

  x1 <- tcd.dat$time
  y1 <- as.numeric(unlist( tcd.dat[ , 2] ))
  y2 <- as.numeric(unlist( tcd.dat[ , 3] ))

  plot(x1, y1
       , type = "l"
       , col = r4apl$col[2]
       , ylim = c(0, rangexy(y1, 20)[2])
       , xlab = "", ylab = "", axes = F)

  box()
  mtext("Retention Time", 1, 3)
  axis(2, las = 2, col = r4apl$col[2], col.ticks = r4apl$col[2], col.axis = r4apl$col[2], cex.axis = cex.axisp)
  par(new = T)
  plot(x1, y2
       , type = "l", ylim = c(0, rangexy(y2, 20)[2])
       , axes = F
       , col = r4apl$col[5]
       , xlab = "", ylab = "")
  axis(1, cex.axis = cex.axisp)
  axis(4, las = 2, col = r4apl$col[5], col.ticks = r4apl$col[5], col.axis = r4apl$col[5], cex.axis = cex.axisp)

  mtext(signal1, 2, linep.y, col = r4apl$col[ 2 ])
  mtext(signal2, 4, linep.x, col = r4apl$col[ 5 ])

  mtext(ifelse(unlist(gregexpr("FID", signal1)) + unlist(gregexpr("FID", signal2)) > -1
               , "FID & TCD"
               , "TCD")
        , 3, linep.main)

  legend("topright", c(signal1, signal2), col = r4apl$col[c(2,5)], lty = 1, cex = cex.axisp)
}
