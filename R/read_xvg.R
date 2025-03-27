#' @title read xvg (from GROMACS)
#'
#' @param file_path The path of the xvg file
#' @param col_names column names. (If not provided, the default column names will be used.)
#'
#' @returns a list
#' @export
#'
#' @examples
#' \donttest{
#' read_xvg(path)
#' }
#'
read_xvg<-function(file_path, col_names){
  dat <- lapply(file_path, read_xvg_s, col_names = col_names)
  names(dat) <- sapply(file_path, basename)
  return(dat)
}
