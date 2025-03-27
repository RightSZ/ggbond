#' @title Plot RMSF (from GROMACS) by ggplot2
#'
#' @param data an object of class xvg from read_xvg function.\code{\link[ggbond]{read_xvg}}
#'
#' @returns a ggplot2 object
#' @export
#'
#' @examples{
#' PlotRMSF(data)
#' }
#'
PlotRMSF<-function(data){
  require(ggplot2)
  d <- lapply(names(data), function(n) {
    df <- data[[n]]
    df$Group <- n
    return(df)
  })
  xaxis_labels<-sapply(names(data), function(n) {
    return(attr(data[[n]], "xaxis_labels"))
  })
  if(length(unique(xaxis_labels))>1){
    cat("more than 1 xaxis label, choose first label")
    xaxis_labels <- xaxis_labels[1]
  }
  yaxis_labels<-sapply(names(data), function(n) {
    return(attr(data[[n]], "yaxis_labels"))
  })
  if(length(unique(yaxis_labels))>1){
    cat("more than 1 yaxis label, choose first label")
    yaxis_labels <- yaxis_labels[1]
  }
  title_labels<-sapply(names(data), function(n) {
    return(attr(data[[n]], "title_labels"))
  })
  if(length(unique(title_labels))>1){
    cat("more than 1 title label, choose first label")
    title_labels <- title_labels[1]
  }
  d <- do.call(rbind,d)
  p<-ggplot(d, aes(x = !!sym(names(d)[1]), y = !!sym(names(d)[2]), color = Group)) +
    geom_line() +
    labs(x = xaxis_labels, y = yaxis_labels, title = title_labels) +
    scale_x_continuous(expand = c(0, 0)) +
    theme_bw() +
    scale_color_viridis_d() +
    theme(plot.title = element_text(hjust = 0.5))
  return(p)
}
