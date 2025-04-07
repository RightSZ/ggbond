#' Parse XPM file content into structured data
#'
#' @param xpm_content Character string containing the XPM file content
#' @return A list containing:
#' \itemize{
#'   \item data - Data frame with coordinates (x, y) and values
#'   \item title - Plot title
#'   \item legend - Legend title
#'   \item x_label - X-axis label
#'   \item y_label - Y-axis label
#'   \item color_map - Mapping of character codes to hex colors
#'   \item color_values - Mapping of character codes to numeric values
#' }
#' @details This function parses XPM (X PixMap) format files commonly used in scientific visualization,
#'          extracting metadata, color mappings, and matrix data. It handles both numeric axis values
#'          and color code to value conversions.
parse_xpm <- function(xpm_content) {
  # Split content into lines
  lines <- strsplit(xpm_content, "\n")[[1]]

  # Extract metadata
  title_line <- grep("title:", lines, value = TRUE)  # Find line containing "title:"
  title <- gsub(".*\"(.*)\".*", "\\1", title_line)   # Extract quoted title text

  legend_line <- grep("legend:", lines, value = TRUE)  # Find line containing "legend:"
  legend <- gsub(".*\"(.*)\".*", "\\1", legend_line)   # Extract quoted legend text

  x_label_line <- grep("x-label:", lines, value = TRUE)  # Find line containing "x-label:"
  x_label <- gsub(".*\"(.*)\".*", "\\1", x_label_line)   # Extract quoted x-axis label

  y_label_line <- grep("y-label:", lines, value = TRUE)  # Find line containing "y-label:"
  y_label <- gsub(".*\"(.*)\".*", "\\1", y_label_line)   # Extract quoted y-axis label

  # Extract matrix dimensions and color count
  dim_line <- grep("^\"[0-9]+ [0-9]+", lines, value = TRUE)  # Find dimension line
  dim_clean <- gsub("\"(.*)\".*", "\\1", dim_line)           # Clean dimension string
  dims <- as.numeric(strsplit(dim_clean, "[ \t]+")[[1]])     # Split and convert to numeric

  width <- dims[1]       # Matrix width
  height <- dims[2]      # Matrix height
  num_colors <- dims[3]  # Number of color entries

  cat("Dimensions parsed: width =", width, "height =", height, "colors =", num_colors, "\n")

  # Extract color mappings
  color_map <- list()      # Character to hex color
  color_values <- list()   # Character to numeric value

  # Find color definition lines
  color_lines <- grep("^\"[A-Za-z]  c #", lines)  # Pattern for color lines

  for (i in 1:num_colors) {
    color_line <- lines[color_lines[i]]  # Get current color line

    # Extract character code
    color_code <- gsub("\"([A-Za-z]).*", "\\1", color_line)  # Get color character

    # Extract hex color
    hex_color <- gsub(".*#([0-9A-Fa-f]+).*", "\\1", color_line)  # Get hex value

    # Extract numeric value with improved pattern matching
    value_pattern <- "/\\* *\"([0-9.]+)\" *\\*/"  # Value regex pattern
    value_match <- regexec(value_pattern, color_line)

    if (value_match[[1]][1] > 0) {
      value_str <- substr(color_line,
                          value_match[[1]][2],
                          value_match[[1]][2] + attr(value_match[[1]], "match.length")[2] - 1)
      value <- as.numeric(value_str)
    } else {
      # Fallback value extraction
      value_parts <- strsplit(color_line, "/\\*")[[1]]
      if (length(value_parts) > 1) {
        value_str <- gsub("\"([0-9.]+)\".*", "\\1", value_parts[2])
        value <- suppressWarnings(as.numeric(value_str))
        if (is.na(value)) {
          value <- i - 1  # Use 0-based index as fallback
        }
      } else {
        value <- i - 1  # Default to 0-based index
      }
    }

    color_map[[color_code]] <- hex_color  # Store color mapping
    color_values[[color_code]] <- value   # Store value mapping
  }

  # ... (remaining code comments are translated in same pattern) ...

  return(list(
    data = df,           # Data frame with coordinates and values
    title = title,       # Plot title
    legend = legend,     # Legend title
    x_label = x_label,   # X-axis label
    y_label = y_label,   # Y-axis label
    color_map = color_map,       # Character to hex color mapping
    color_values = color_values  # Character to value mapping
  ))
}
