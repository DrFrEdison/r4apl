autoclave_ppm <- function(conc # concentration of e.g. F- in the IC analysis
                          , V_WF = 0.03 # Volume of the washing bottle solution in L
                          , V_AK = 0.160 # Volume of the autoclave in m³
                          , V_Gas_Sampling = 0.002 # Gas sampling amount in m³
                          , Compound = "Hydrogen fluoride"
                          , Molar_Mass = NA
                          , Temperature # °C
                          , Pressure){
  
  Amount <- conc * V_WF
  Gas_Amount <- Amount / V_Gas_Sampling # mg/m³ in washed gas
  
  V_Molecular <- 8.3145 * (273.15 + Temperature) / (Pressure * 100)
  
  if(is.na(Molar_Mass)){
    
    find.compound <- rbind(unlist(gregexpr(Compound, chemicals.master$Compound))
                           , unlist(gregexpr(Compound, chemicals.master$Compound_Alternative))
                           , unlist(gregexpr(Compound, chemicals.master$Compound_Alternative_2))
                           , unlist(gregexpr(Compound, chemicals.master$Formula))
                           , unlist(gregexpr(Compound, chemicals.master$German.Name)))
    
    Molar_Mass <- chemicals.master$`Molar.Mass.g/mol`[ which(apply(find.compound, 2, function( x ) any( x > 0))) ]
  }
  
  ppm <- V_Molecular * Gas_Amount / Molar_Mass
  return(ppm)
}