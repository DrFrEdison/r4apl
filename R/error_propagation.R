# Define the function for error propagation when dividing two quantities
error_propagation_division <- function(mean_A, sd_A, mean_B, sd_B) {
  # Calculate the ratio of the means
  C <- mean_A / mean_B
  
  # Calculate the propagated uncertainty
  sigma_C <- C * sqrt((sd_A / mean_A)^2 + (sd_B / mean_B)^2)
  
  # Return the results as a list
  return(list(ratio = C, propagated_error = sigma_C))
}
