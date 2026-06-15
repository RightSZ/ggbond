#' Run the ggbond Shiny app
#'
#' @param plot_list A named list of plot objects. If `NULL`, demo plots are
#'   used. Supported objects include ggplot2 plots, base graphics functions or
#'   recorded plots, pheatmap objects, ComplexHeatmap objects, grobs, gtables,
#'   and local image panels uploaded in the app.
#' @param canvas_width_px Canvas width in pixels.
#' @param canvas_height_px Canvas height in pixels.
#' @param device_width_in Graphics device width in inches.
#' @param device_height_in Graphics device height in inches.
#' @details Canvas and device sizes are linked at 100 pixels per inch. If only
#'   one size pair is supplied, the other pair is derived automatically.
#' @param launch.browser Passed to shiny::runApp().
#'
#' @return A `ggbond` layout object containing panel positions, canvas metadata,
#'   graphics device metadata, uploaded image metadata, and the app exit reason.
#'
#' @examples
#' if (interactive()) {
#'   plots <- list(
#'     scatter = ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'       ggplot2::geom_point()
#'   )
#'   layout <- run_ggbond(plots)
#'   layout
#' }
#'
#' @export
run_ggbond <- function(
    plot_list = NULL,
    canvas_width_px = 700,
    canvas_height_px = 500,
    device_width_in = 7,
    device_height_in = 5,
    launch.browser = TRUE
) {
  canvas_width_supplied <- !missing(canvas_width_px)
  canvas_height_supplied <- !missing(canvas_height_px)
  device_width_supplied <- !missing(device_width_in)
  device_height_supplied <- !missing(device_height_in)
  px_per_in <- 100

  sizes <- normalize_ggbond_sizes(
    canvas_width_px = canvas_width_px,
    canvas_height_px = canvas_height_px,
    device_width_in = device_width_in,
    device_height_in = device_height_in,
    canvas_width_supplied = canvas_width_supplied,
    canvas_height_supplied = canvas_height_supplied,
    device_width_supplied = device_width_supplied,
    device_height_supplied = device_height_supplied,
    px_per_in = px_per_in
  )
  canvas_width_px <- sizes$canvas_width_px
  canvas_height_px <- sizes$canvas_height_px
  device_width_in <- sizes$device_width_in
  device_height_in <- sizes$device_height_in

  if (is.null(plot_list)) {
    plot_list <- ggbond_demo_plots()
  }

  if (!is.list(plot_list)) {
    stop("`plot_list` must be a named list of plot objects.", call. = FALSE)
  }

  if (is.null(names(plot_list)) || any(names(plot_list) == "")) {
    stop("`plot_list` must be a named list.", call. = FALSE)
  }

  # Locate the packaged static assets.
  www_dir <- system.file("app/www", package = "ggbond")

  # In development mode, fall back to the project-local inst directory.
  if (identical(www_dir, "")) {
    www_dir <- file.path(getwd(), "inst", "app", "www")
  }

  if (!dir.exists(www_dir)) {
    stop(
      "Cannot find ggbond www directory: ",
      www_dir,
      call. = FALSE
    )
  }

  # Register static assets for the Shiny session. The prefix is unique so
  # browsers and long-lived R sessions cannot reuse an older ggbond.js mapping.
  asset_prefix <- paste0(
    "ggbond-assets-",
    format(Sys.time(), "%Y%m%d%H%M%OS3"),
    "-",
    sample.int(1e6, 1)
  )
  asset_prefix <- gsub("[^A-Za-z0-9_-]", "", asset_prefix)

  shiny::addResourcePath(
    prefix = asset_prefix,
    directoryPath = www_dir
  )

  ui <- ggbond_ui(
    canvas_width_px = canvas_width_px,
    canvas_height_px = canvas_height_px,
    device_width_in = device_width_in,
    device_height_in = device_height_in,
    asset_prefix = asset_prefix
  )

  server <- ggbond_server(
    plot_list = plot_list,
    canvas_width_px = canvas_width_px,
    canvas_height_px = canvas_height_px,
    device_width_in = device_width_in,
    device_height_in = device_height_in
  )

  app <- shiny::shinyApp(ui = ui, server = server)

  shiny::runApp(app, launch.browser = launch.browser)
}

#' Normalize linked canvas and graphics device sizes
#'
#' @param canvas_width_px Canvas width in pixels.
#' @param canvas_height_px Canvas height in pixels.
#' @param device_width_in Graphics device width in inches.
#' @param device_height_in Graphics device height in inches.
#' @param canvas_width_supplied Whether the canvas width was supplied by the
#'   caller.
#' @param canvas_height_supplied Whether the canvas height was supplied by the
#'   caller.
#' @param device_width_supplied Whether the device width was supplied by the
#'   caller.
#' @param device_height_supplied Whether the device height was supplied by the
#'   caller.
#' @param px_per_in Pixel-to-inch conversion ratio.
#'
#' @return A list with synchronized canvas and device sizes.
#' @keywords internal
#' @noRd
normalize_ggbond_sizes <- function(
    canvas_width_px,
    canvas_height_px,
    device_width_in,
    device_height_in,
    canvas_width_supplied,
    canvas_height_supplied,
    device_width_supplied,
    device_height_supplied,
    px_per_in = 100
) {
  if (!canvas_width_supplied && device_width_supplied) {
    canvas_width_px <- round(device_width_in * px_per_in)
  }
  if (!canvas_height_supplied && device_height_supplied) {
    canvas_height_px <- round(device_height_in * px_per_in)
  }
  if (!device_width_supplied && canvas_width_supplied) {
    device_width_in <- canvas_width_px / px_per_in
  }
  if (!device_height_supplied && canvas_height_supplied) {
    device_height_in <- canvas_height_px / px_per_in
  }

  list(
    canvas_width_px = canvas_width_px,
    canvas_height_px = canvas_height_px,
    device_width_in = device_width_in,
    device_height_in = device_height_in
  )
}
