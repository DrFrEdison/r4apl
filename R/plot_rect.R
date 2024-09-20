#' Create a Rectangular Plot with Segments
#'
#' This function generates a bar plot with rectangular bars, overlaying segments for additional data visualization. It is useful for plotting two sets of related data.
#'
#' @param y1 Numeric vector, the heights of the bars.
#' @param y2 Numeric vector, the additional heights to be added as segments on top of the bars.
#' @param angle Numeric, the angle of shading lines. Default is 45.
#' @param lty Integer, line type for the borders of the rectangles. Default is 1 (solid).
#' @param lwd Numeric, line width for the borders of the rectangles and segments. Default is 1.
#' @param density Numeric, the density of shading lines, if shading is applied. Default is 0 (no shading).
#' @param col Color, the fill color of the rectangles. Default is NA (no fill).
#' @param width Numeric, the width of the bars. Default is 0.25.
#' @param border Color, the color of the rectangle borders and segments. Default is "black".
#'
#' @return A sequence corresponding to the x positions of the bars, for further customization if needed.
#'
#' @examples
#' \dontrun{
#' y1 <- c(3, 5, 2, 6)
#' y2 <- c(1, 2, 3, 1)
#' rectplot(y1, y2, col = "blue", border = "red", lwd = 2)
#' }
#'
#' @export
rectplot <- function(y1,
                     y2,
                     angle = 45,
                     lty = 1,
                     lwd = 1,
                     density = 0,
                     col = NA,
                     width = 0.25,
                     border = "black") {

  # Ensure y1 and y2 are of equal length
  if (length(y1) != length(y2)) {
    stop("y1 and y2 must be of equal length.")
  }

  # Create a sequence for bar positions
  seqp <- seq_along(y1)

  # Set up the plot area without plotting any data (type = "n")
  plot(1, 1, type = "n", xlab = "", ylab = "", axes = FALSE,
       xlim = c(1 - 0.25, length(y1) + 0.25),
       ylim = c(0, rangexy(y1 + y2)[2]))

  # Draw rectangles representing y1 values
  rect(xleft = seqp - width, ybottom = 0, xright = seqp + width, ytop = y1,
       density = density, col = col, border = border, angle = angle, lty = lty, lwd = lwd)

  # If line type is not 1, redraw the rectangles with lty = 1
  if (any(lty != 1)) {
    rect(xleft = seqp - width, ybottom = 0, xright = seqp + width, ytop = y1,
         border = border, lty = 1, lwd = lwd)
  }

  # Draw segments on top of the rectangles, representing y2 values
  segments(x0 = seqp, y0 = y1, x1 = seqp, y1 = y1 + y2,
           col = border, lty = 1, lwd = lwd)

  # Draw a background rectangle for the plot area
  rect(xleft = par("usr")[1], xright = par("usr")[2], ybottom = 0, ytop = par("usr")[4], xpd = TRUE)

  # Add y-axis
  axis(2, las = 2)

  # Return the x positions of the bars
  return(seqp)
}
