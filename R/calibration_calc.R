#' Calibration Calculation
#'
#' This function calculates the final concentration of a substance based on calibration models
#' from instruments like IC, HPLC, or GC-MS.
#'
#' @param y Numeric value, the observed area or height from the instrument.
#' @param instrument Character, the instrument used for analysis. Options are "IC", "HPLC", or "GC-MS".
#' @param substance Character, the name of the substance being analyzed.
#' @param type Character, the type of signal being measured ("area" or "height"). Default is "area".
#' @param Verdünnung Numeric, the dilution factor applied to the sample. Default is 1.
#' @param date Character, the date of the calibration in the format "YYYY-MM-DD". This is converted to a standardized format.
#' @param sub Optional, a numeric or logical vector specifying which subset of calibration data to use for recalculating the model.
#'
#' @return Numeric value, the calculated final concentration of the substance.
#' @export
#'
#' @examples
#' \dontrun{
#' final_conc <- calibration.calc(y = 1500, instrument = "IC", substance = "NaCl", date = "2024-01-01")
#' }
calibration.calc <- function(y,
                             instrument = c("IC", "HPLC", "GC-MS"),
                             substance,
                             type = c("area", "height"),
                             Verdünnung = 1,
                             date,
                             sub = NA) {

  # Ensure date is in proper format by using datetime() function
  if (nchar(date) == 10) date <- datetime(date)

  # Retrieve the calibration file based on the instrument and date
  cal_dir <- wd$cal[[grep(instrument, wd$cal)]]
  cal_rds <- dir(cal_dir, pattern = "\\.rds$")
  cal_rds <- grep(date, cal_rds, value = TRUE)[1]

  # Error handling: Ensure the calibration file is found
  if (is.null(cal_rds)) {
    stop("Calibration file not found for the specified date.")
  }

  # Load the calibration model from the RDS file
  cal_mod <- readRDS(file.path(cal_dir, cal_rds))

  # Error handling: Ensure the substance exists in the model
  if (!substance %in% names(cal_mod)) {
    stop("Substance not found in the calibration model.")
  }

  cal_mod <- cal_mod[[which(names(cal_mod) == substance)]]

  # If a subset is provided, recalculate the model
  if (any(!is.na(sub))) {
    cal_mod <- lm(cal_mod$model$area[sub] ~ cal_mod$model$conc[sub])
  }

  # Calculate the final concentration based on the calibration model
  final_concentration <- (y - as.numeric(cal_mod$coefficients[1])) / as.numeric(cal_mod$coefficients[2]) * Verdünnung

  return(final_concentration)
}
