#' Read and parse multiple XPM files
#'
#' @param xpm_files Character vector of XPM file paths
#' @return A list of parsed results where each element corresponds to a file,
#'         named by the file's base name. Each element contains the same
#'         structure as returned by parse_xpm()
#' @details This function:
#' \itemize{
#'   \item Validates input type and file existence
#'   \item Handles missing files with warnings
#'   \item Automatically skips non-existent files
#'   \item Returns aggregated results in named list
#' }
#' @seealso \code{\link{parse_xpm}} for the underlying parser
#' @examples
#' results <- read_xpm(c("file1.xpm", "file2.xpm"))
read_xpm <- function(xpm_files) {
  # Validate input type
  if (!is.character(xpm_files)) {
    stop("xpm_files must be a character vector containing one or more XPM file paths")
  }

  # Check file existence
  missing_files <- xpm_files[!file.exists(xpm_files)]
  if (length(missing_files) > 0) {
    warning("The following files do not exist: ", paste(missing_files, collapse = ", "))
    xpm_files <- xpm_files[file.exists(xpm_files)]
  }

  if (length(xpm_files) == 0) {
    stop("No valid XPM files to read")
  }

  # Process files
  results <- list()
  for (file_path in xpm_files) {
    xpm_content <- paste(readLines(file_path), collapse = "\n")
    parsed_result <- parse_xpm(xpm_content)
    results[[basename(file_path)]] <- parsed_result
  }

  return(results)
}
