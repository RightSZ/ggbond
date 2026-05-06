#' Create the ggbond Shiny user interface
#'
#' Builds the browser-based layout editor used by ggbond. Most users should
#' call `run_ggbond()` instead of using this function directly.
#'
#' @param canvas_width_px Canvas width in pixels.
#' @param canvas_height_px Canvas height in pixels.
#' @param device_width_in Graphics device width in inches.
#' @param device_height_in Graphics device height in inches.
#' @param asset_prefix Shiny resource path prefix for static app assets.
#'
#' @return A Shiny UI object.
#' @export
ggbond_ui <- function(canvas_width_px = 700,
                      canvas_height_px = 500,
                      device_width_in = 7,
                      device_height_in = 5,
                      asset_prefix = "ggbond-assets") {
  asset_version <- as.integer(Sys.time())

  shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = paste0(asset_prefix, "/ggbond.css?v=", asset_version)
      ),
      shiny::tags$script(src = paste0(asset_prefix, "/ggbond.js?v=", asset_version))
    ),

    shiny::titlePanel("ggbond: Fixed-size ggplot Layout Editor"),

    shiny::fluidRow(
      shiny::column(
        width = 2,

        shiny::h4("Controls"),

        shiny::actionButton("open_device", "Open fixed R plot window"),
        shiny::br(), shiny::br(),

        shiny::div(
          class = "panel-action-row",
          shiny::actionButton("add_panel", "Add panel", icon = shiny::icon("plus"), class = "btn-primary"),
          shiny::actionButton("delete_panel", "Delete selected", icon = shiny::icon("trash"), class = "btn-danger")
        ),
        shiny::br(), shiny::br(),

        shiny::fileInput(
          "image_files",
          "Add local image",
          multiple = TRUE,
          accept = c(
            ".png",
            ".jpg",
            ".jpeg",
            ".tif",
            ".tiff",
            "image/png",
            "image/jpeg",
            "image/tiff"
          )
        ),
        shiny::br(),

        shiny::downloadButton("download_pdf", "Export PDF"),
        shiny::br(), shiny::br(),
        shiny::downloadButton("download_png", "Export PNG"),
        shiny::br(), shiny::br(),

        shiny::actionButton(
          "exit_app",
          "Exit Shiny",
          icon = shiny::icon("power-off"),
          class = "btn-danger exit-app-button"
        )
      ),

      shiny::column(
        width = 7,

        shiny::tags$b(sprintf(
          "Shiny layout canvas: %d x %d px, mapped to %.1f x %.1f inch R device",
          canvas_width_px,
          canvas_height_px,
          device_width_in,
          device_height_in
        )),

        shiny::div(
          id = "canvas",
          style = sprintf(
            "width:%spx; height:%spx;",
            canvas_width_px,
            canvas_height_px
          ),
          shiny::div(id = "guide_v", class = "snap-guide v"),
          shiny::div(id = "guide_h", class = "snap-guide h")
        ),

        shiny::h4("Current layout"),
        shiny::verbatimTextOutput("layout_text")
      ),

      shiny::column(
        width = 3,

        shiny::div(
          id = "inspector_shell",
          class = "inspector-shell",
          shiny::div(
            class = "inspector-card",
            shiny::div(
              class = "inspector-title",
              shiny::span("Panel Inspector"),
              shiny::tags$button(
                id = "toggle_inspector",
                type = "button",
                class = "inspector-toggle",
                title = "Hide panel inspector",
                "Hide"
              )
            ),
            shiny::div(
              class = "inspector-body",
              shiny::uiOutput("panel_inspector")
            )
          )
        )
      )
    )
  )
}
