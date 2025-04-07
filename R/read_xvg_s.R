#' @title read a xvg (from GROMACS)
#'
#' @param file_path The path of the xvg file
#' @param col_names column names. (If not provided, the default column names will be used.)
#'
#' @returns a data.frame
#'
#' @examples
#' \donttest{
#' read_xvg_s(path)
#' }
#'
read_xvg_s <- function(file_path, col_names) {
  file_name <- sub("\\.[^.]*$", "", basename(file_path))
  lines <- readLines(file_path)

  title_line <- grep("^@\\s*title", lines, value = TRUE)
  title <- sub('^@\\s*title\\s*"([^"]+)".*', "\\1", title_line)

  xaxis_line <- grep("^@\\s*xaxis  label", lines, value = TRUE)
  xaxis <- sub('^@\\s*xaxis  label\\s*"([^"]+)".*', "\\1", xaxis_line)

  yaxis_line <- grep("^@\\s*yaxis  label", lines, value = TRUE)
  yaxis <- sub('^@\\s*yaxis  label\\s*"([^"]+)".*', "\\1", yaxis_line)

  subtitle_line <- grep("^@\\s*subtitle", lines, value = TRUE)
  subtitle <- sub('^@\\s*subtitle\\s*"([^"]+)".*', "\\1", subtitle_line)

  data_lines <- lines[!grepl("^[@#]", lines)]
  data <- read.table(text = data_lines)
  if(!missing(col_names)) {colnames(data) <- col_names
  } else {col_names <- paste0("V",1:ncol(data))}
  attr(data, "title_labels") <- title
  attr(data, "xaxis_labels") <- xaxis
  attr(data, "yaxis_labels") <- yaxis
  attr(data, "subtitle_labels") <- subtitle
  attr(data, "file_name") <- file_name
  return(data)
}
