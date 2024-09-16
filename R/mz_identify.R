mz_identify <- function(mz){
  
  if(mz == 1) return(expression("H"^"+"))
  if(mz == 2) return(expression("H"[2]^"+"))
  if(mz == 14) return(expression("N"^"+"))
  if(mz == 17) return(expression("C"[2]*"H"[3]))
  if(mz == 18) return(expression("H"^"+"))
  if(mz == 28) return(expression("N"[2]))
  if(mz == 32) return(expression("O"[2]))
  if(mz == 40) return("Argon")
  return("unknown substance")
}
  
  