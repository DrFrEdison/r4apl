#' Error Propagation for Division of Two Quantities
#'
#' This function calculates the propagated error when dividing two quantities, taking into account their means and standard deviations.
#'
#' @param mean_A Numeric, the mean value of quantity A.
#' @param sd_A Numeric, the standard deviation (error) of quantity A.
#' @param mean_B Numeric, the mean value of quantity B.
#' @param sd_B Numeric, the standard deviation (error) of quantity B.
#'
#' @return A list containing the ratio of the means (C = mean_A / mean_B) and the propagated error (sigma_C).
#'
#' @examples
#' \dontrun{
#' result <- error_propagation_division(mean_A = 10, sd_A = 0.5, mean_B = 5, sd_B = 0.2)
#' print(result)
#' }
#'
#' @export
error_propagation_division <- function(mean_A, sd_A, mean_B, sd_B) {

  # Input validation
  if (any(mean_B == 0)) {
    stop("mean_B cannot be zero, division by zero is not allowed.")
  }

  if (any(mean_A == 0)) {
    warning("mean_A is zero, the ratio will be zero and propagated error may be large.")
  }

  # Calculate the ratio of the means
  C <- mean_A / mean_B

  # Calculate the propagated uncertainty
  propagated_error <- C * sqrt((sd_A / mean_A)^2 + (sd_B / mean_B)^2)

  # Return the results as a list
  return(list(ratio = C, propagated_error = propagated_error))
}
