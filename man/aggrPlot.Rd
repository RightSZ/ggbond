% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggrPlot.R
\name{aggrPlot}
\alias{aggrPlot}
\title{Plot aggr by ggplot2}
\usage{
aggrPlot(d, col_replace = TRUE)
}
\arguments{
\item{d}{an object of class "aggr" from aggr function.\code{\link[VIM]{aggr}}}

\item{col_replace}{a logical indicating whether the colnames should be replaced by space (the default is TRUE).}
}
\value{
three ggplot2 objects
}
\description{
Draw three images from aggr by ggplot2
}
\examples{
\donttest{
aggrPlot(d, col_replace=TRUE)
}

}
