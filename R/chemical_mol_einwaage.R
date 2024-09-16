convert_mol_to_Einwaage <- function(SOLL_concentration = c(1, 50)
                                    , SOLL_unit = "mmol/L"
                                    , return_unit = "g"
                                    , Volume = 2
                                    , Volume_unit = "L"
                                    , substance = "Na2CO3"
                                    , molar_mass = r4apl$chem$molar_mass[[ which(names(r4apl$chem$molar_mass) %in% substance )]]){
  
  if(is.null(molar_mass))  if(!any(names(molar_mass) %in% substance) | is.null(molar_mass)){
    warning( paste0("Molar mass of ", substance, " is not in molar mass list"))
    molar_mass = molar_mass
  }
  
  if(any(names(molar_mass) %in% substance)){
    molar_mass = r4apl$chem$molar_mass[[ which(names(molar_mass) %in% substance) ]]
  }
  
  if(return_unit == "g" & SOLL_unit == "mmol/L" & Volume_unit == "L") SOLL_Einwaage = molar_mass * SOLL_concentration/1000 * Volume
  if(return_unit == "g" & SOLL_unit == "mmol/L" & Volume_unit == "mL") SOLL_Einwaage = molar_mass * SOLL_concentration/1000 * Volume/1000
  if(return_unit == "mg" & SOLL_unit == "mmol/L" & Volume_unit == "L") SOLL_Einwaage = molar_mass * SOLL_concentration/1000 * Volume * 1000
  if(return_unit == "g" & SOLL_unit == "mol/L" & Volume_unit == "L") SOLL_Einwaage = molar_mass * SOLL_concentration * Volume
  
  return(data.frame(SOLL_concentration = SOLL_concentration
                    , unit = SOLL_unit
                    , SOLL_Einwaage = SOLL_Einwaage
                    , unit = return_unit))
}

convert_Einwaage_to_mol <- function(IST_Einwaage = NA
                                    , IST_unit = "g"
                                    , return_unit = "mol/L"
                                    , Volume = 1
                                    , Volume_unit = "L"
                                    , substance = "Na2CO3"
                                    , molar_mass = r4apl$chem$molar_mass[[ which(names(r4apl$chem$molar_mass) %in% substance )]]){
  
  IST_Molaritaet = IST_Einwaage / molar_mass / Volume
  
  if(return_unit == "g" & SOLL_unit == "mmol/L" & Volume_unit == "L") SOLL_Einwaage = molar_mass * SOLL_concentration/1000 * Volume
  if(return_unit == "g" & SOLL_unit == "mmol/L" & Volume_unit == "mL") SOLL_Einwaage = molar_mass * SOLL_concentration/1000 * Volume/1000
  if(return_unit == "mg" & SOLL_unit == "mmol/L" & Volume_unit == "L") SOLL_Einwaage = molar_mass * SOLL_concentration/1000 * Volume * 1000
  if(return_unit == "g" & SOLL_unit == "mol/L" & Volume_unit == "L") SOLL_Einwaage = molar_mass * SOLL_concentration * Volume
  
  return(data.frame(IST_Molaritaet = IST_Molaritaet
                    , unit = return_unit
                    , IST_Einwaage = IST_Einwaage
                    , unit = IST_unit))
}



