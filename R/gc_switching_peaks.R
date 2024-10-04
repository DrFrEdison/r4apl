#' Detect Switching Peaks in GC-TCD Data
#'
#' This function identifies switching peaks in Gas Chromatography (GC) Thermoconductivity Detector (TCD) data.
#' It detects negative-to-positive crossings within a specified time window.
#'
#' @param data A data.table or data.frame containing the GC data.
#' @param data_x Character, the name of the column representing the x-axis (typically time). Default is "time".
#' @param data_y Character, the name of the column representing the y-axis (typically the signal, such as TCD2B). Default is "TCD2B".
#' @param window_size Numeric, the size of the window (in data points) used to filter out peaks based on their proximity. Default is 15.
#' @param cut_time Numeric, the time threshold beyond which peaks are considered. Default is 1.
#'
#' @return Numeric, the average time point where a switching peak occurs. Returns NA if no peak is found.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' gc_switching_peaks(data = my_gc_data, data_x = "time", data_y = "TCD2B")
#' }
#' 
#' @export
gc_switching_peaks <- function(data,
                               data_x = "time",
                               data_y = "TCD2B",
                               window_size = 15,
                               cut_time = 1) {
  
  # Extract time and signal columns from the data
  time_vector <- data[, get(data_x)]
  signal_vector <- data[, get(data_y)]
  
  # Step 1: Identify the index of time points greater than the cut_time
  cut_time_indices <- which(time_vector > cut_time)
  
  # Step 2: Detect negative-to-positive crossings
  # We look for changes in the sign of the signal and ensure the signal is non-zero
  crossings <- which(c(0, diff(sign(signal_vector[cut_time_indices]))) != 0 & abs(signal_vector[cut_time_indices]) > 0)
  
  # If no crossings are found, return NA
  if (length(crossings) == 0) return(NA)
  
  # Step 3: Filter crossings based on the specified window size
  switching_peaks <- crossings[which(diff(crossings) < window_size)]
  
  # Adjust for the fact that we're working on a subset of the data
  switching_peaks <- switching_peaks + min(cut_time_indices)
  
  # Return the rounded mean of the switching peaks
  return(round(mean(switching_peaks), 0))
}
