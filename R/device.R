#' Open a fixed-size graphics device
#'
#' Opens a platform-appropriate graphics device with a fixed size for live
#' layout preview.
#'
#' @param width Device width in inches.
#' @param height Device height in inches.
#' @param title Device window title where supported.
#'
#' @return The numeric id of the current graphics device.
#' @keywords internal
#' @noRd
open_fixed_device <- function(
    width = 7,
    height = 5,
    title = "ggbond preview"
) {
  sys <- Sys.info()[["sysname"]]

  if (sys == "Darwin") {
    grDevices::quartz(
      title = title,
      width = width,
      height = height
    )
  } else if (sys == "Windows") {
    grDevices::windows(
      title = title,
      width = width,
      height = height,
      restoreConsole = FALSE
    )
  } else {
    grDevices::x11(
      title = title,
      width = width,
      height = height
    )
  }

  grDevices::dev.cur()
}
