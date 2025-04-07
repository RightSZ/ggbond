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
  title <- gsub(".*\"(.*)\".*", "\\1", title_line)   # Extract title from quotes

  legend_line <- grep("legend:", lines, value = TRUE)  # Find line containing "legend:"
  legend <- gsub(".*\"(.*)\".*", "\\1", legend_line)   # Extract legend from quotes

  x_label_line <- grep("x-label:", lines, value = TRUE)  # Find line containing "x-label:"
  x_label <- gsub(".*\"(.*)\".*", "\\1", x_label_line)   # Extract x-axis label

  y_label_line <- grep("y-label:", lines, value = TRUE)  # Find line containing "y-label:"
  y_label <- gsub(".*\"(.*)\".*", "\\1", y_label_line)   # Extract y-axis label

  # Extract matrix dimensions and color count
  dim_line <- grep("^\"[0-9]+ [0-9]+", lines, value = TRUE)  # Find dimension line
  dim_clean <- gsub("\"(.*)\".*", "\\1", dim_line)           # Clean quotes and extract dimension data
  dims <- as.numeric(strsplit(dim_clean, "[ \t]+")[[1]])     # Split and convert to numeric

  width <- dims[1]       # Matrix width
  height <- dims[2]      # Matrix height
  num_colors <- dims[3]  # Number of colors

  cat("Dimensions parsed: width =", width, "height =", height, "colors =", num_colors, "\n")

  # Create color mappings
  color_map <- list()      # Map of color codes to hex values
  color_values <- list()   # Map of color codes to numerical values

  # Find color definition lines
  color_lines <- grep("^\"[A-Za-z]  c #", lines)  # Pattern for color definitions

  for (i in 1:num_colors) {
    color_line <- lines[color_lines[i]]  # Get current color line

    # Extract color code (character used in matrix)
    color_code <- gsub("\"([A-Za-z]).*", "\\1", color_line)

    # Extract hex color value
    hex_color <- gsub(".*#([0-9A-Fa-f]+).*", "\\1", color_line)

    # Value extraction with improved pattern matching
    value_pattern <- "/\\* *\"([0-9.]+)\" *\\*/"
    value_match <- regexec(value_pattern, color_line)

    if (value_match[[1]][1] > 0) {
      value_str <- substr(color_line,
                          value_match[[1]][2],
                          value_match[[1]][2] + attr(value_match[[1]], "match.length")[2] - 1)
      value <- as.numeric(value_str)
    } else {
      # Fallback method if pattern doesn't match
      value_parts <- strsplit(color_line, "/\\*")[[1]]
      if (length(value_parts) > 1) {
        value_str <- gsub("\"([0-9.]+)\".*", "\\1", value_parts[2])
        value <- suppressWarnings(as.numeric(value_str))
        if (is.na(value)) {
          # Final fallback: use index as value
          value <- i - 1  # Using 0-based index
        }
      } else {
        value <- i - 1  # 0-based index
      }
    }

    color_map[[color_code]] <- hex_color  # Store color mapping
    color_values[[color_code]] <- value   # Store value mapping
  }

  # Extract axis values
  x_axis_line <- grep("x-axis:", lines, value = TRUE)
  if (length(x_axis_line) > 0) {
    x_values_str <- gsub(".*: *(.*)", "\\1", x_axis_line)
    x_values_str <- gsub("[ \t]+", " ", trimws(x_values_str))  # Clean whitespace
    x_values <- suppressWarnings(as.numeric(strsplit(x_values_str, " ")[[1]]))
    x_values <- x_values[!is.na(x_values)]
  } else {
    x_values <- 1:width  # Default sequence
  }

  y_axis_line <- grep("y-axis:", lines, value = TRUE)
  if (length(y_axis_line) > 0) {
    y_values_str <- gsub(".*: *(.*)", "\\1", y_axis_line)
    y_values_str <- gsub("[ \t]+", " ", trimws(y_values_str))  # Clean whitespace
    y_values <- suppressWarnings(as.numeric(strsplit(y_values_str, " ")[[1]]))
    y_values <- y_values[!is.na(y_values)]
  } else {
    y_values <- 1:height  # Default sequence
  }

  # Locate data matrix starting position
  data_lines <- grep("^\"[A-Za-z]+\"", lines)  # Potential data lines
  data_start <- data_lines[length(data_lines) - height + 1]

  # Extract data matrix
  data_matrix <- matrix(NA, nrow = height, ncol = width)

  for (i in 1:height) {
    row_line <- lines[data_start + i - 1]
    row_data <- gsub("\"(.*)\".*", "\\1", row_line)

    for (j in 1:width) {
      if (j <= nchar(row_data)) {
        char <- substr(row_data, j, j)
        if (char %in% names(color_values)) {
          data_matrix[i, j] <- color_values[[char]]
        } else {
          data_matrix[i, j] <- NA
        }
      } else {
        data_matrix[i, j] <- NA
      }
    }
  }

  # Create final data frame
  df <- expand.grid(x = 1:width, y = 1:height)
  df$value <- as.vector(t(data_matrix))

  # Map actual axis values if available
  if (length(x_values) >= width) {
    df$x_actual <- x_values[df$x]
  } else {
    df$x_actual <- df$x
  }

  if (length(y_values) >= height) {
    df$y_actual <- y_values[height - df$y + 1]  # Reverse y-axis for matrix alignment
  } else {
    df$y_actual <- df$y
  }

  # Return parsed data structure
  return(list(
    data = df,           # Parsed data frame
    title = title,       # Chart title
    legend = legend,     # Legend title
    x_label = x_label,   # X-axis label
    y_label = y_label,   # Y-axis label
    color_map = color_map,       # Color code to hex mapping
    color_values = color_values  # Color code to value mapping
  ))
}
