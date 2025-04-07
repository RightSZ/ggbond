#' Visualize XPM data using ggplot2
#'
#' @param dat List object returned by xpm_files function
#' @param interpolate Logical indicating whether to use interpolation for rendering (default: FALSE)
#' @return ggplot object ready for display or further customization
#' @export
#' @details
#' \itemize{
#'   \item Uses geom_raster() with interpolation for smooth gradients when interpolate=TRUE
#'   \item Uses geom_tile() for pixel-perfect rendering when interpolate=FALSE
#'   \item Automatically sets:
#'     \itemize{
#'       \item Axis labels from parsed metadata
#'       \item Color scale using viridis palette
#'       \title Fixed aspect ratio based on data dimensions
#'     }
#' }
#' @examples
#' # Basic plot without interpolation
#' PlotXpm(data)
#'
#' # With interpolation
#' PlotXpm(data, interpolate=TRUE)
PlotXpm <- function(dat, interpolate = F) {
  if (interpolate) {
    p <- ggplot(dat$data, aes(x = x_actual, y = y_actual, fill = value)) +
      geom_raster(interpolate = T)
  } else {
    p <- ggplot(dat$data, aes(x = x_actual, y = y_actual, fill = value)) +
      geom_tile()
  }

  p <- p +
    labs(
      title = dat$title,
      x = dat$x_label,
      y = dat$y_label,
      fill = dat$legend
    ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_viridis_c() +
    coord_fixed(
      ratio = (max(dat$data$x_actual, na.rm = TRUE) - min(dat$data$x_actual, na.rm = TRUE)) /
        (max(dat$data$y_actual, na.rm = TRUE) - min(dat$data$y_actual, na.rm = TRUE))
    )

  return(p)
}
