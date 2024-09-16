rectplot <- function(y1
                     , y2
                     , angle = 45
                     , lty = 1
                     , lwd = 1
                     , density = 0
                     , col = NA
                     , width = .25
                     , border = "black"){
  
  seqp <- seq_along(y1)
  plot(1,1,type="n",xlab ="", ylab="",axes=F
       , xlim = c(1 - .25, length(y1) + .25)
       , ylim = c(0, rangexy(y1 + y2)[ 2 ]))
  
  rect(xleft = seqp - width, ybottom = 0, xright = seqp + width, ytop = y1
       , density = density
       , col = col
       , border = border
       , angle = angle
       , lty = lty
       , lwd = lwd)
  
  if(any(lty != 1)) rect(xleft = seqp - width, ybottom = 0, xright = seqp + width, ytop = y1
                         , border = border
                         , lty = 1
                         , lwd = lwd)
  
  segments(x0 = seqp, y0 = y1, x1 = seqp, y1 = y1+y2
           , col = border
           , lty = 1
           , lwd = lwd)
  rect(xleft = par("usr")[1], xright = par("usr")[2], ybottom = 0, ytop = par("usr")[4], xpd = T)
  axis(2, las = 2)
  
  return(seqp)
}