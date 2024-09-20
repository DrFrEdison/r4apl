#' Generate PCA Component Label with Explained Variance
#'
#' This function generates a label for a principal component (PC) that includes the explained variance for a specified component.
#'
#' @param pca A PCA model object containing the results of the PCA analysis.
#' @param ncomp Integer, the component number to label. Default is 1.
#'
#' @return A character string with the component label and explained variance in percentage.
#'
#' @examples
#' \dontrun{
#' pca.lab(pca = powerco$pca$GC, ncomp = 1)
#' }
#'
#' @export
pca.lab <- function(pca = powerco$pca$GC, ncomp = 1) {
  return(paste0("PC", ncomp, ", Explained Variance = ", round(pca$calres$expvar[ncomp], 1), "%"))
}

#' Extract and Rescale PCA Arrows for Visualization
#'
#' This function extracts the loadings from a PCA model and rescales them for better visualization of PCA arrows. It calculates the correlation loadings for the first two principal components and rescales them by a specified factor.
#'
#' @param pca_model A PCA model object containing the loadings and explained variance.
#' @param pc_x Integer, the first principal component for the x-axis. Default is 1.
#' @param pc_y Integer, the second principal component for the y-axis. Default is 2.
#' @param scale_factor Numeric, the factor by which to rescale the loadings. Default is 5.
#'
#' @return A data frame containing the variable names and the rescaled loadings for the specified principal components.
#'
#' @examples
#' \dontrun{
#' extract_and_rescale_pca_arrows(pca_model = powerco$pca$GC, pc_x = 1, pc_y = 2, scale_factor = 5)
#' }
#'
#' @export
extract_and_rescale_pca_arrows <- function(pca_model, pc_x = 1, pc_y = 2, scale_factor = 5) {
  loadings <- pca_model$loadings

  # Calculate the explained variance (proportion of variance explained by each component)
  explained_variance <- pca_model$calres$expvar / 100

  # Ensure explained variance is correctly ordered (use only the first two components)
  explained_variance <- explained_variance[1:2]

  # Calculate the correlation loadings (scaling loadings by the square root of explained variance)
  correlation_loadings <- loadings[, 1:2] * sqrt(explained_variance)

  # Rescale the correlation loadings for better visualization
  rescaled_loadings <- correlation_loadings * scale_factor

  # Extract x and y values for the selected principal components
  x_values <- rescaled_loadings[, pc_x]
  y_values <- rescaled_loadings[, pc_y]

  # Create a data frame with the variable names and rescaled loadings
  result <- data.frame(Variable = rownames(rescaled_loadings), PC1 = x_values, PC2 = y_values)

  return(result)
}
