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
#' @param launch.browser Passed to shiny::runApp().
#'
#' @return Runs a Shiny application. This function is called for its side
#'   effect and returns when the Shiny app stops.
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

  # Register static assets for the Shiny session.
  shiny::addResourcePath(
    prefix = "ggbond-assets",
    directoryPath = www_dir
  )

  ui <- ggbond_ui(
    canvas_width_px = canvas_width_px,
    canvas_height_px = canvas_height_px,
    device_width_in = device_width_in,
    device_height_in = device_height_in
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
