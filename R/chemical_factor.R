#' Create a Factor for Chemicals Based on Retention Time and a Reference Sheet
#'
#' This function creates a factor for a list of chemicals, optionally ordering them based on retention time. It matches the chemicals with a reference sheet (Excel) that contains various chemical properties.
#' If a chemical is not found in the reference, a warning is issued and the chemical is placed at the end.
#'
#' @param chemicals A character vector representing the list of chemical compounds.
#' @param retention.time Numeric, optional. Retention times for the chemicals. Default is NA.
#' @param xlsx A data frame or list (Excel sheet) containing chemical properties such as Formula, Compound, German Name, Compound_Alternative, and color. Default is `chemicals.master`.
#'
#' @return A factor where the levels are ordered based on the reference sheet or retention time.
#'
#' @examples
#' \dontrun{
#' chemicals <- c("H2O", "NaCl", "CO2")
#' retention.time <- c(2.5, 1.3, 3.1)
#' chemical_factor(chemicals, retention.time, xlsx = chemicals.master)
#' }
#'
#' @export
chemical_factor <- function(chemicals, retention.time = NA, xlsx = chemicals.master) {

  # Input validation
  if (!is.character(chemicals)) stop("The 'chemicals' parameter must be a character vector.")

  # If retention time is provided and valid, order chemicals by retention time
  if (length(retention.time) != 1 && !any(is.na(retention.time))) {
    retention.time.unique <- unique(retention.time)
    chemicals.order <- chemicals[order(retention.time)]
    chemicals.level.order <- unique(chemicals.order)
  }

  # Get the unique chemicals
  chemicals.unique <- unique(chemicals)

  # Initialize vector to store the level positions
  level.position <- numeric(length(chemicals.unique))

  # If retention time is NA, match chemicals with the reference data
  if (length(retention.time) == 1 || any(is.na(retention.time))) {
    for (i in seq_along(chemicals.unique)) {
      chem <- chemicals.unique[i]

      # Check for matches in the reference sheet based on different fields
      if (length(which(xlsx$Formula %in% chem)) == 1) {
        level.position[i] <- which(xlsx$Formula %in% chem)
      } else if (length(which(xlsx$Compound %in% chem)) == 1) {
        level.position[i] <- which(xlsx$Compound %in% chem)
      } else if (length(which(xlsx$German.Name %in% chem)) == 1) {
        level.position[i] <- which(xlsx$German.Name %in% chem)
      } else if (length(which(xlsx$Compound_Alternative %in% chem)) == 1) {
        level.position[i] <- which(xlsx$Compound_Alternative %in% chem)
      } else {
        # If no match is found, assign a position after the reference list
        level.position[i] <- i + nrow(xlsx)
        warning(paste0(chem, " not found in the reference file. Assigning to the end of the factor levels."))
      }
    }

    # Handle cases where chemicals are not found in the reference file
    if (any(level.position > nrow(xlsx))) {
      to.long <- which(level.position > nrow(xlsx))

      # Reorder unrecognized chemicals and place them after known chemicals
      for (k in seq_along(level.position[to.long])) {
        level.position[to.long][order(level.position[to.long])[k]] <- max(level.position[-to.long]) + k
      }
    }

    # Order chemicals based on their level positions
    chemicals.level.order <- chemicals.unique[order(level.position)]
  }

  # Create a factor with the ordered levels
  chemicals <- factor(chemicals, levels = chemicals.level.order)

  return(chemicals)
}
