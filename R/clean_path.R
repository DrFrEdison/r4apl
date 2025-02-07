clean_path <- function(path,
                       convert_slashes = TRUE,
                       force_encoding = TRUE,
                       normalize = TRUE) {

  # setwd(clean_path(r"(Z:\Verfahren_Geräte\GC\Batteriegasanalytik\03_Gasgemische\Sicherheitsdatenblätter)"))

  # Handle different input types
  if (!is.character(path)) {
    stop("Path must be a character string")
  }

  # Handle raw string input by removing r"()" if present
  if (grepl('^r"\\(.*\\)"$', path) || grepl("^r'\\(.*\\)'$", path)) {
    path <- gsub('^r"\\(|\\)"$', '', path)
    path <- gsub("^r'\\(|\\)'$", '', path)
  }

  # Convert backslashes to forward slashes if requested
  if (convert_slashes) {
    path <- gsub("\\\\", "/", path)
    # Handle any double forward slashes that might have been created
    path <- gsub("//", "/", path)
  }

  # Force UTF-8 encoding if requested
  if (force_encoding) {
    path <- enc2utf8(path)
  }

  # Normalize path if requested
  if (normalize) {
    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  }

  return(path)
}
