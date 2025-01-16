#' Identify Substances Based on m/z Value
#'
#' This function returns an expression representing a chemical formula or a substance name based on a given mass-to-charge ratio (m/z).
#'
#' @param mz A numeric value representing the m/z (mass-to-charge ratio) to identify.
#'
#' @return A string or an expression representing the identified substance or chemical species based on the m/z value.
#' If the m/z is not recognized, it returns "unknown substance".
#'
#' @examples
#' mz_identify(1)   # Returns "H"^"+"
#' mz_identify(32)  # Returns "O"[2]
#' mz_identify(99)  # Returns "unknown substance"
#'
#' @export
mz_identify <- function(mz) {

  switch(as.character(mz),
         "1" = expression("H"^"+"),
         "2" = expression("H"[2]^"+"),
         "14" = expression("N"^"+"),
         "17" = expression("C"[2]*"H"[3]),
         "18" = expression("H"^"+"),
         "28" = expression("N"[2]),
         "32" = expression("O"[2]),
         "40" = "Argon",
         "unknown substance"
  )
}
