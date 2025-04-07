#' @title Plot aggr by ggplot2
#' @description
#' Draw three images from aggr by ggplot2
#'
#' @param d an object of class "aggr" from aggr function.\code{\link[VIM]{aggr}}
#' @param col_replace a logical indicating whether the colnames should be replaced by space (the default is TRUE).
#'
#' @returns three ggplot2 objects
#' @export
#'
#' @importFrom stringr str_replace_all
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#' @import patchwork
#' @import dplyr
#' @examples
#' \donttest{
#' aggrPlot(d, col_replace=TRUE)
#' }
#'
aggrPlot<-function(d, col_replace=TRUE){
  if (col_replace) {
    d[["missings"]][["Variable"]] <- str_replace_all(d[["missings"]][["Variable"]],
                                                     "_", " ")
  }
  h <- d[["tabcomb"]][order(d[["count"]], decreasing = T), ]
  if(!is.matrix(h)) h <- matrix(h,ncol = length(h))
  colnames(h) <- d[["missings"]][["Variable"]]
  h <- as.data.frame(h) %>%
    mutate(y = rownames(.)) %>%
    pivot_longer(cols = -y, names_to = "x", values_to = "value")
  h$x<-factor(h$x,levels=d[["missings"]][["Variable"]])
  h$value<-factor(h$value)
  p1 <- ggplot(h, aes(x = x, y = y, fill = value)) +
    geom_tile(colour = "white") +
    scale_fill_manual(values = c(`0` = "grey",
                                 `1` = "red")) +
    labs(title = "", x = "", y = "Combinations") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.5),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none")+
    scale_y_discrete(expand = c(0,0))
  p <- data.frame(Col = d[["missings"]][["Variable"]], Proportion = d[["missings"]][["Count"]]/nrow(d[["x"]]))
  p$Col <- factor(p$Col, levels = unique(p$Col))
  p2 <- ggplot(p, aes(x = Col, y = Proportion)) +
    geom_bar(stat = "identity", fill = "red") +
    labs(title = "", x = "", y = "Proportion of missings") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.5))
  l <- data.frame(percent = d[["percent"]][order(d[["count"]],
                                                 decreasing = T)],
                  count = d[["count"]][order(d[["count"]],
                                             decreasing = T)],
                  color = ifelse(stringr::str_detect(d[["combinations"]][order(d[["count"]],
                                                                               decreasing = T)], "1"), "1", "0"))
  l$rowname <- factor(rownames(l), levels = unique(rownames(l)))
  p3 <- ggplot(l, aes(y = rowname, x = count, fill = color)) +
    geom_bar(stat = "identity", colour = "black") +
    scale_y_discrete(labels = paste0(round(l$percent, digits = 2), "%"), expand = c(0, 0), position = "right") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_fill_manual(values = c(`0` = "grey", `1` = "red")) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.title = element_blank(), axis.line.y = element_blank(),
          axis.ticks.y = element_blank(), plot.background = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "none")

  return(p2 + p1 + p3 + plot_layout(ncol = 3, widths = c(1, 1, 0.1)))
}
