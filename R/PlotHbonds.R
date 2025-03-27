#' @title Plot Hbonds (from GROMACS) by ggplot2
#'
#' @param data an object of class xvg from read_xvg function.\code{\link[ggbond]{read_xvg}}
#'
#' @returns a ggplot2 object
#' @export
#'
#' @examples{
#' PlotHbonds(data)
#' }
#'
PlotHbonds<-function(data,legend_names=c("Hydrogen bonds","Pairs within 0.35 nm")){
  require(ggplot2)
  require(tidyr)
  xaxis_labels<-sapply(names(data), function(n) {
    return(attr(data[[n]], "xaxis_labels"))
  })
  yaxis_labels<-sapply(names(data), function(n) {
    return(attr(data[[n]], "yaxis_labels"))
  })
  title_labels<-sapply(names(data), function(n) {
    return(attr(data[[n]], "title_labels"))
  })
  d <- do.call(rbind,data)
  colnames(d)[2:ncol(d)]<-legend_names
  d <- d |>
    pivot_longer(
      cols = -V1,
      names_to = "Variable",
      values_to = "Value"
    )
  p<-ggplot(d, aes(x = V1, y = Value, color = Variable)) +
    geom_line() +
    labs(x = xaxis_labels, y = yaxis_labels, title = title_labels) +
    scale_x_continuous(expand = c(0, 0)) +
    theme_bw() +
    scale_color_viridis_d() +
    theme(plot.title = element_text(hjust = 0.5))
  return(p)
}


