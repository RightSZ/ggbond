% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_xpm.R
\name{parse_xpm}
\alias{parse_xpm}
\title{Parse XPM file content into structured data}
\usage{
parse_xpm(xpm_content)
}
\arguments{
\item{xpm_content}{Character string containing the XPM file content}
}
\value{
A list containing:
\itemize{
\item data - Data frame with coordinates (x, y) and values
\item title - Plot title
\item legend - Legend title
\item x_label - X-axis label
\item y_label - Y-axis label
\item color_map - Mapping of character codes to hex colors
\item color_values - Mapping of character codes to numeric values
}
}
\description{
Parse XPM file content into structured data
}
\details{
This function parses XPM (X PixMap) format files commonly used in scientific visualization,
extracting metadata, color mappings, and matrix data. It handles both numeric axis values
and color code to value conversions.
}
