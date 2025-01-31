# pV = nRT
ideales_gas_Stoffmenge <- function(p = 101325 # Pa
                                   , V = 0.050 # m³
                                   , n = NA
                                   , R = 8.314 # J / (mol * K)
                                   , Te = 20 # °C
){
  
  Te = Te + 273.15
  n = p * V / (R * Te)
  return(n)
}