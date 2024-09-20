#' Find Folder in System Environment Path
#'
#' This function searches for a folder within a system environment path (e.g., "LOCALAPPDATA") based on specified patterns.
#' It can handle multiple matches and returns the first matching folder.
#'
#' @param x Character, the name of the system environment variable to search within (e.g., "LOCALAPPDATA"). Default is "LOCALAPPDATA".
#' @param pattern Character, the first pattern to match folder names against. Default is "Teckso".
#' @param pattern2 Character, an optional second pattern to narrow down results if multiple folders match the first pattern. Default is "GmbH".
#'
#' @return A string representing the path to the matched folder. If no folder is found, a warning is issued.
#'
#' @examples
#' \dontrun{
#' find.Sys.getenv.folder(x = "LOCALAPPDATA", pattern = "Teckso", pattern2 = "GmbH")
#' }
#'
#' @export
find.Sys.getenv.folder <- function(x = "LOCALAPPDATA", pattern = "Teckso", pattern2 = "GmbH") {

  # Get the system environment path
  dir.x <- Sys.getenv(x)

  # List directories in the environment path that match the first pattern
  x.dir <- dir(path = dir.x, pattern = pattern)

  # If no directories match, issue a warning
  if (length(x.dir) == 0) {
    warning("No folder found matching the first pattern.")
    return(NULL)
  }

  # Build the full path
  x.dir <- file.path(dir.x, x.dir)

  # If only one directory matches, return it
  if (length(x.dir) == 1) return(x.dir)

  # If multiple directories match, narrow down the results with the second pattern
  if (length(x.dir) > 1) {
    x.dir <- grep(pattern2, x.dir, value = TRUE)

    # If a directory matches the second pattern, return the first match
    if (length(x.dir) >= 1) {
      return(x.dir[1])
    } else {
      warning("No folder found matching the second pattern.")
      return(NULL)
    }
  }
}
