% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chemical_mol_weigh_in.R
\name{convert_Einwaage_to_mol}
\alias{convert_Einwaage_to_mol}
\title{Convert Einwaage to Molarity}
\usage{
convert_Einwaage_to_mol(
  IST_Einwaage = NA,
  IST_unit = "g",
  return_unit = "mol/L",
  Volume = 1,
  Volume_unit = "L",
  substance = "Na2CO3",
  molar_mass = r4apl$chem$molar_mass[[which(names(r4apl$chem$molar_mass) \%in\%
    substance)]]
)
}
\arguments{
\item{IST_Einwaage}{Numeric, the actual weight of the substance used in grams or milligrams.}

\item{IST_unit}{Character, the unit of the weight (Einwaage). Options are "g" or "mg". Default is "g".}

\item{return_unit}{Character, the desired unit for the resulting molarity. Options are "mol/L" or "mmol/L". Default is "mol/L".}

\item{Volume}{Numeric, the volume of the solution. Default is 1.}

\item{Volume_unit}{Character, the unit of the volume. Options are "L" or "mL". Default is "L".}

\item{substance}{Character, the name of the substance for which the molarity is being calculated. Default is "Na2CO3".}

\item{molar_mass}{Numeric, the molar mass of the substance. Default is obtained from the r4apl package.}
}
\value{
A data frame containing the molarity (IST_Molaritaet), the units of molarity, and the initial Einwaage.
}
\description{
This function converts a given weight (Einwaage) of a substance to its molarity (mol/L or mmol/L) based on its molar mass, volume, and the desired return unit.
}
\examples{
\dontrun{
convert_Einwaage_to_mol(IST_Einwaage = 10, IST_unit = "g", Volume = 1, substance = "NaCl")
}

}
