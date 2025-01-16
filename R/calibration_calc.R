#' Calibration Calculation
#'
#' This function calculates the final concentration of a substance based on calibration models
#' from instruments like IC, HPLC, or GC-MS.
#'
#' @param area Numeric value, the observed area or height from the instrument.
#' @param calibration_model calibration model from rds file.
#' @param substance Character, the name of the substance being analyzed.
#' @param dilution Numeric, the dilution factor applied to the sample. Default is 1.
#' @param sub Optional, a matrix specifying which subset of calibration data to use for recalculating the model.
#'
#' @return Numeric value, the calculated final concentration of the substance.
#' @export
#'
#' @examples
#' \dontrun{
#' final_conc <- calibration.calc(area = 1500, instrument = "IC", substance = "NaCl", date = "2024-01-01")
#' }
calibration_calc <- function(area,
                             calibration_model = cal.file,
                             substance,
                             dilution = 1,
                             sub = NA) {

  # Error handling: Ensure the substance exists in the model
  if (!substance %in% names(calibration_model)) {
    stop("Substance not found in the calibration model.")
  }

  calibration_model <- calibration_model[[which(names(calibration_model) == substance)]]

  sub

  # If a subset is provided, recalculate the model
  if (any(!is.na(sub))) {

    if( any( is.na( sub[ , which(colnames(sub) %in% substance)])))

      sub <- min(sub[ , which(colnames(sub) %in% substance)], na.rm = T) : max(sub[ , which(colnames(sub) %in% substance)], na.rm = T)
    calibration_model <- lm(calibration_model$model$area[sub] ~ calibration_model$model$conc[sub])
  }

  # Calculate the final concentration based on the calibration model
  final_concentration <- (area - as.numeric(calibration_model$coefficients[1])) / as.numeric(calibration_model$coefficients[2]) * dilution

  return(final_concentration)
}
