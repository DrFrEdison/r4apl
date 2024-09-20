#' Convert Molar Concentration to Weight (Einwaage)
#'
#' This function converts a specified molar concentration (e.g., mmol/L or mol/L) to a weight (Einwaage) based on the molar mass of a substance, volume, and the desired return unit (e.g., grams or milligrams).
#'
#' @param SOLL_concentration Numeric vector, the target concentration in mol/L or mmol/L.
#' @param SOLL_unit Character, the unit of the target concentration. Default is "mmol/L".
#' @param return_unit Character, the desired unit for the resulting weight. Options are "g" or "mg". Default is "g".
#' @param Volume Numeric, the volume of the solution. Default is 2.
#' @param Volume_unit Character, the unit of the volume. Options are "L" or "mL". Default is "L".
#' @param substance Character, the name of the substance for which the weight is being calculated. Default is "Na2CO3".
#' @param molar_mass Numeric, the molar mass of the substance. Default is obtained from the r4apl package.
#'
#' @return A data frame containing the concentration, unit, and calculated weight (Einwaage).
#'
#' @examples
#' \dontrun{
#' convert_mol_to_Einwaage(SOLL_concentration = 10, SOLL_unit = "mmol/L", Volume = 1, substance = "NaCl")
#' }
#'
#' @export
convert_mol_to_Einwaage <- function(SOLL_concentration = c(1, 50),
                                    SOLL_unit = "mmol/L",
                                    return_unit = "g",
                                    Volume = 2,
                                    Volume_unit = "L",
                                    substance = "Na2CO3",
                                    molar_mass = r4apl$chem$molar_mass[[which(names(r4apl$chem$molar_mass) %in% substance)]]) {

  # Check if molar mass is provided or exists for the substance
  if (is.null(molar_mass) || !any(names(r4apl$chem$molar_mass) %in% substance)) {
    warning(paste0("Molar mass of ", substance, " is not in the molar mass list"))
    molar_mass <- molar_mass  # Keep the provided molar_mass as is (if given manually)
  }

  # Assign the correct molar mass if found in the list
  if (any(names(r4apl$chem$molar_mass) %in% substance)) {
    molar_mass <- r4apl$chem$molar_mass[[which(names(r4apl$chem$molar_mass) %in% substance)]]
  }

  # Input validation: Ensure valid units for conversion
  valid_return_units <- c("g", "mg")
  valid_SOLL_units <- c("mmol/L", "mol/L")
  valid_Volume_units <- c("L", "mL")

  if (!(return_unit %in% valid_return_units)) stop("Invalid return_unit. Must be 'g' or 'mg'.")
  if (!(SOLL_unit %in% valid_SOLL_units)) stop("Invalid SOLL_unit. Must be 'mmol/L' or 'mol/L'.")
  if (!(Volume_unit %in% valid_Volume_units)) stop("Invalid Volume_unit. Must be 'L' or 'mL'.")

  # Perform the calculation based on unit conditions
  if (return_unit == "g" && SOLL_unit == "mmol/L" && Volume_unit == "L") {
    SOLL_Einwaage <- molar_mass * SOLL_concentration / 1000 * Volume
  } else if (return_unit == "g" && SOLL_unit == "mmol/L" && Volume_unit == "mL") {
    SOLL_Einwaage <- molar_mass * SOLL_concentration / 1000 * Volume / 1000
  } else if (return_unit == "mg" && SOLL_unit == "mmol/L" && Volume_unit == "L") {
    SOLL_Einwaage <- molar_mass * SOLL_concentration / 1000 * Volume * 1000
  } else if (return_unit == "g" && SOLL_unit == "mol/L" && Volume_unit == "L") {
    SOLL_Einwaage <- molar_mass * SOLL_concentration * Volume
  } else {
    stop("Unsupported combination of units. Please check SOLL_unit, Volume_unit, and return_unit.")
  }

  # Return a data frame with the results
  return(data.frame(SOLL_concentration = SOLL_concentration,
                    concentration_unit = SOLL_unit,
                    SOLL_Einwaage = SOLL_Einwaage,
                    Einwaage_unit = return_unit))
}


#' Convert Einwaage to Molarity
#'
#' This function converts a given weight (Einwaage) of a substance to its molarity (mol/L or mmol/L) based on its molar mass, volume, and the desired return unit.
#'
#' @param IST_Einwaage Numeric, the actual weight of the substance used in grams or milligrams.
#' @param IST_unit Character, the unit of the weight (Einwaage). Options are "g" or "mg". Default is "g".
#' @param return_unit Character, the desired unit for the resulting molarity. Options are "mol/L" or "mmol/L". Default is "mol/L".
#' @param Volume Numeric, the volume of the solution. Default is 1.
#' @param Volume_unit Character, the unit of the volume. Options are "L" or "mL". Default is "L".
#' @param substance Character, the name of the substance for which the molarity is being calculated. Default is "Na2CO3".
#' @param molar_mass Numeric, the molar mass of the substance. Default is obtained from the r4apl package.
#'
#' @return A data frame containing the molarity (IST_Molaritaet), the units of molarity, and the initial Einwaage.
#'
#' @examples
#' \dontrun{
#' convert_Einwaage_to_mol(IST_Einwaage = 10, IST_unit = "g", Volume = 1, substance = "NaCl")
#' }
#'
#' @export
convert_Einwaage_to_mol <- function(IST_Einwaage = NA,
                                    IST_unit = "g",
                                    return_unit = "mol/L",
                                    Volume = 1,
                                    Volume_unit = "L",
                                    substance = "Na2CO3",
                                    molar_mass = r4apl$chem$molar_mass[[which(names(r4apl$chem$molar_mass) %in% substance)]]) {

  # Check if molar mass is provided or exists for the substance
  if (is.null(molar_mass) || !any(names(r4apl$chem$molar_mass) %in% substance)) {
    stop(paste0("Molar mass of ", substance, " is not found in the molar mass list. Please provide it manually."))
  }

  # Assign the correct molar mass if found in the list
  if (any(names(r4apl$chem$molar_mass) %in% substance)) {
    molar_mass <- r4apl$chem$molar_mass[[which(names(r4apl$chem$molar_mass) %in% substance)]]
  }

  # Input validation: Ensure valid units for conversion
  valid_IST_units <- c("g", "mg")
  valid_return_units <- c("mol/L", "mmol/L")
  valid_Volume_units <- c("L", "mL")

  if (!(IST_unit %in% valid_IST_units)) stop("Invalid IST_unit. Must be 'g' or 'mg'.")
  if (!(return_unit %in% valid_return_units)) stop("Invalid return_unit. Must be 'mol/L' or 'mmol/L'.")
  if (!(Volume_unit %in% valid_Volume_units)) stop("Invalid Volume_unit. Must be 'L' or 'mL'.")

  # Convert the input weight to grams if necessary
  if (IST_unit == "mg") {
    IST_Einwaage <- IST_Einwaage / 1000  # Convert mg to g
  }

  # Convert the volume to liters if necessary
  if (Volume_unit == "mL") {
    Volume <- Volume / 1000  # Convert mL to L
  }

  # Calculate molarity based on the Einwaage, molar mass, and volume
  IST_Molaritaet <- IST_Einwaage / molar_mass / Volume

  # Adjust the molarity if the return unit is mmol/L
  if (return_unit == "mmol/L") {
    IST_Molaritaet <- IST_Molaritaet * 1000
  }

  # Return the results as a data frame
  return(data.frame(IST_Molaritaet = IST_Molaritaet,
                    Molarity_unit = return_unit,
                    IST_Einwaage = IST_Einwaage,
                    Einwaage_unit = IST_unit))
}
