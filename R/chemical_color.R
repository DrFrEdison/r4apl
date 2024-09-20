#' Assign Colors to Chemicals Based on Chemical Properties
#'
#' This function assigns colors to a list of chemicals, using a provided reference Excel sheet that contains chemical formulas, names, and their corresponding colors.
#' If a chemical is not found in the reference list, a random color is assigned.
#'
#' @param chemicals A factor representing the list of chemical compounds.
#' @param xlsx A data frame or list (Excel sheet) containing chemical properties such as Formula, Compound, German Name, Compound_Alternative, and color. Default is `chemicals.master`.
#'
#' @return A list with two elements: "legend" (the colors for each chemical level) and "single" (the colors assigned to each entry in the `chemicals` vector).
#'
#' @examples
#' \dontrun{
#' # Assuming chemicals.master is a data frame with appropriate columns:
#' chemicals <- factor(c("H2O", "NaCl", "CO2"))
#' chemical_color(chemicals, xlsx = chemicals.master)
#' }
#'
#' @export
chemical_color <- function(chemicals, xlsx = chemicals.master) {

  # Input validation
  if (!is.factor(chemicals)) stop("Compound list is not a factor.")

  # Get the levels of chemicals
  chemicals.levels <- levels(chemicals)

  # Initialize matrices for legend and single colors
  col.legend <- matrix(NA, length(chemicals.levels), 1)
  col.single <- matrix(NA, length(chemicals), 1)

  # Assign colors based on chemical properties
  for (i in seq_along(chemicals.levels)) {

    level <- chemicals.levels[i]

    # Match colors based on different chemical properties
    if (length(which(xlsx$Formula %in% level)) == 1) {
      col.legend[i, 1] <- xlsx$color[which(xlsx$Formula %in% level)]
    } else if (length(which(xlsx$Compound %in% level)) == 1) {
      col.legend[i, 1] <- xlsx$color[which(xlsx$Compound %in% level)]
    } else if (length(which(xlsx$German.Name %in% level)) == 1) {
      col.legend[i, 1] <- xlsx$color[which(xlsx$German.Name %in% level)]
    } else if (length(which(xlsx$Compound_Alternative %in% level)) == 1) {
      col.legend[i, 1] <- xlsx$color[which(xlsx$Compound_Alternative %in% level)]
    }

    # Assign random color if no match is found
    if (is.na(col.legend[i, 1])) {
      col.legend[i, 1] <- sample(rainbow(100), 1)
      warning(paste("No color found for", level, "- assigning a random color."))
    }

    # Assign the color to all matching entries in the chemicals vector
    col.single[chemicals %in% level, 1] <- col.legend[i, 1]
  }

  # Return a list with the colors for the legend and individual chemicals
  returnlist <- list(legend = col.legend, single = col.single)

  return(returnlist)
}
