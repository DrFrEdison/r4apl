integrate_peak <- function(start, end, x, y) {
  trapz(x[start:end], y[start:end])
}
