pca.lab <- function(pca = powerco$pca$GC, ncomp = 1) return(paste0("PC", ncomp, ", Explained Variance = ", round(pca$calres$expvar[ ncomp ], 1), "%"))

# Extract and rescale the names and xy values of the arrows
extract_and_rescale_pca_arrows <- function(pca_model, pc_x = 1, pc_y = 2, scale_factor = 5) {
  loadings <- pca_model$loadings
  
  # Calculate the explained variance (proportion of variance explained by each component)
  explained_variance <- pca_model$calres$expvar / 100
  
  # Ensure explained variance is correctly ordered
  explained_variance <- explained_variance[1:2]
  
  # Calculate the correlation loadings
  correlation_loadings <- loadings[, 1:2] * sqrt(explained_variance)
  
  # Rescale the correlation loadings for better visualization
  rescaled_loadings <- correlation_loadings * scale_factor
  
  # Extract x and y values for the first two principal components
  x_values <- rescaled_loadings[, pc_x]
  y_values <- rescaled_loadings[, pc_y]
  
  # Create a data frame with the results
  result <- data.frame(Variable = rownames(rescaled_loadings), PC1 = x_values, PC2 = y_values)
  return(result)
}
