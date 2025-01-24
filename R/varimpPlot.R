#' @title Plot importance by ggplot2
#' @description
#' Draw a plot from importance by ggplot2
#'
#' @param d an object of class randomForest from importance function.\code{\link[randomForest]{importance}}
#' @param cutoff a cutoff value to filter eligible features.
#'
#' @returns a ggplot2 object
#' @export
#'
#' @import ggplot2
#' @examples
#' \donttest{
#' varimpPlot(d, cutoff=3)
#' }
#'

varimpPlot<-function(d,cutoff){
  d<-as.data.frame(d)
  d$Rowname<-rownames(d)
  if(!missing(cutoff)){
    d$color_value <- ifelse(d$MeanDecreaseGini >= cutoff, d$MeanDecreaseGini, NA)
  }
  p<-ggplot(d, aes(
    x = MeanDecreaseGini,
    y = reorder(Rowname, MeanDecreaseGini)
  )) +
    geom_point(aes(color = color_value)) +
    geom_segment(aes(
      x = 0, xend = MeanDecreaseGini,color=color_value
    )) +
    labs(y = NULL) +
    theme_bw()+
    scale_color_viridis_c(na.value = "grey50")+
    theme(legend.position = "none")+
    scale_x_continuous(
      breaks = seq(0, ceiling(max(d$MeanDecreaseGini)), by = 2),
      limits = c(0, ceiling(max(d$MeanDecreaseGini)))
    )
  if(!missing(cutoff)){
    p<-p+geom_vline(xintercept = cutoff, color = "red",alpha=0.7, linetype = "dashed")
  }
  return(p)
}
