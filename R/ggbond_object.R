#' Create a ggbond layout object
#'
#' @param layout A data frame containing panel layout information.
#' @param canvas A list with canvas metadata.
#' @param device A list with graphics device metadata.
#' @param image_assets A data frame of uploaded image assets.
#' @param exit_reason Character scalar describing how the Shiny app stopped.
#'
#' @return An object of class `ggbond`.
#' @keywords internal
#' @noRd
new_ggbond <- function(
    layout = data.frame(),
    canvas = list(width_px = 700, height_px = 500),
    device = list(width_in = 7, height_in = 5),
    image_assets = data.frame(),
    exit_reason = "unknown"
) {
  structure(
    list(
      layout = layout,
      canvas = canvas,
      device = device,
      image_assets = image_assets,
      exit_reason = exit_reason,
      created_at = Sys.time()
    ),
    class = "ggbond"
  )
}

#' Default a NULL value
#'
#' @param x An object.
#' @param y Fallback value.
#'
#' @return `x` unless it is `NULL`, otherwise `y`.
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Normalize a JSON-decoded table
#'
#' @param x A decoded JSON object.
#'
#' @return A data frame.
#' @keywords internal
#' @noRd
normalize_ggbond_table <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(data.frame())
  }
  if (is.data.frame(x)) {
    return(x)
  }
  if (is.matrix(x)) {
    return(as.data.frame(x, stringsAsFactors = FALSE))
  }
  if (is.list(x)) {
    lengths <- vapply(x, length, integer(1))
    if (length(lengths) == 0 || all(lengths == 0)) {
      return(data.frame())
    }
    max_length <- max(lengths)
    normalized <- lapply(x, function(column) {
      if (length(column) == max_length) {
        return(column)
      }
      if (length(column) == 1) {
        return(rep(column, max_length))
      }
      column
    })
    return(as.data.frame(normalized, stringsAsFactors = FALSE))
  }

  data.frame()
}

#' Print a ggbond layout object
#'
#' @param x A `ggbond` object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#'
#' @examples
#' layout <- structure(
#'   list(
#'     layout = data.frame(id = "panel_1", label = "A"),
#'     canvas = list(width_px = 700, height_px = 500),
#'     device = list(width_in = 7, height_in = 5),
#'     image_assets = data.frame(),
#'     exit_reason = "example",
#'     created_at = Sys.time()
#'   ),
#'   class = "ggbond"
#' )
#' print(layout)
#'
#' @export
print.ggbond <- function(x, ...) {
  panel_count <- if (is.data.frame(x$layout)) {
    nrow(x$layout)
  } else {
    0
  }

  cat("<ggbond layout>\n")
  cat("Panels: ", panel_count, "\n", sep = "")
  cat(
    "Canvas: ",
    x$canvas$width_px,
    " x ",
    x$canvas$height_px,
    " px\n",
    sep = ""
  )
  cat(
    "Device: ",
    x$device$width_in,
    " x ",
    x$device$height_in,
    " in\n",
    sep = ""
  )
  cat("Exit reason: ", x$exit_reason, "\n", sep = "")

  invisible(x)
}

#' Save a ggbond layout object to JSON
#'
#' @param x A `ggbond` object returned by [run_ggbond()].
#' @param file Output JSON file path.
#' @param pretty Whether to write pretty-formatted JSON.
#'
#' @return Invisibly returns `file`.
#'
#' @examples
#' layout <- structure(
#'   list(
#'     layout = data.frame(
#'       id = "panel_1",
#'       label = "A",
#'       x = 0,
#'       y = 0,
#'       width = 700,
#'       height = 500,
#'       plot = "scatter",
#'       source = "plot:scatter",
#'       lock_aspect = FALSE,
#'       show_border = FALSE,
#'       z = 1
#'     ),
#'     canvas = list(width_px = 700, height_px = 500),
#'     device = list(width_in = 7, height_in = 5),
#'     image_assets = data.frame(),
#'     exit_reason = "example",
#'     created_at = Sys.time()
#'   ),
#'   class = "ggbond"
#' )
#' file <- tempfile(fileext = ".json")
#' save_ggbond_json(layout, file)
#' restored <- read_ggbond_json(file)
#' restored
#'
#' @export
save_ggbond_json <- function(x, file, pretty = TRUE) {
  if (!inherits(x, "ggbond")) {
    stop("`x` must be a ggbond object returned by `run_ggbond()`.", call. = FALSE)
  }

  payload <- unclass(x)
  payload$schema <- "ggbond"
  payload$schema_version <- 1L
  payload$created_at <- as.character(payload$created_at)

  jsonlite::write_json(
    payload,
    path = file,
    dataframe = "rows",
    auto_unbox = TRUE,
    pretty = pretty,
    na = "null",
    null = "null"
  )

  invisible(file)
}

#' Read a ggbond layout object from JSON
#'
#' @param file Input JSON file path produced by [save_ggbond_json()].
#'
#' @return A `ggbond` object.
#'
#' @examples
#' layout <- structure(
#'   list(
#'     layout = data.frame(
#'       id = "panel_1",
#'       label = "A",
#'       x = 0,
#'       y = 0,
#'       width = 700,
#'       height = 500,
#'       plot = "scatter",
#'       source = "plot:scatter",
#'       lock_aspect = FALSE,
#'       show_border = FALSE,
#'       z = 1
#'     ),
#'     canvas = list(width_px = 700, height_px = 500),
#'     device = list(width_in = 7, height_in = 5),
#'     image_assets = data.frame(),
#'     exit_reason = "example",
#'     created_at = Sys.time()
#'   ),
#'   class = "ggbond"
#' )
#' file <- tempfile(fileext = ".json")
#' save_ggbond_json(layout, file)
#' read_ggbond_json(file)
#'
#' @export
read_ggbond_json <- function(file) {
  payload <- jsonlite::read_json(
    file,
    simplifyVector = TRUE,
    simplifyDataFrame = TRUE
  )

  if (!identical(payload$schema, "ggbond")) {
    stop("`file` does not contain a ggbond JSON object.", call. = FALSE)
  }

  created_at <- payload$created_at
  if (!is.null(created_at) && length(created_at) > 0) {
    created_at <- as.POSIXct(created_at, tz = "UTC")
  } else {
    created_at <- Sys.time()
  }

  out <- new_ggbond(
    layout = normalize_ggbond_table(payload$layout),
    canvas = payload$canvas %||% list(width_px = 700, height_px = 500),
    device = payload$device %||% list(width_in = 7, height_in = 5),
    image_assets = normalize_ggbond_table(payload$image_assets),
    exit_reason = payload$exit_reason %||% "unknown"
  )
  out$created_at <- created_at

  out
}

#' Render a ggbond layout object
#'
#' Re-renders a layout returned by [run_ggbond()] using a supplied plot list.
#'
#' @param x A `ggbond` object returned by [run_ggbond()].
#' @param plot_list A named list of plot objects matching the plot sources used
#'   in the layout.
#' @param file Optional output path. When `NULL`, rendering is drawn to the
#'   current graphics device.
#' @param device Output device when `file` is supplied. Supported values are
#'   `"pdf"` and `"png"`. When `NULL`, the device is inferred from `file`.
#' @param width Device width in inches. Defaults to the width stored in `x`.
#' @param height Device height in inches. Defaults to the height stored in `x`.
#' @param res PNG resolution in dots per inch.
#'
#' @return Invisibly returns `x`.
#'
#' @examples
#' plots <- list(
#'   scatter = ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'     ggplot2::geom_point()
#' )
#' layout <- structure(
#'   list(
#'     layout = data.frame(
#'       id = "panel_1",
#'       label = "A",
#'       x = 0,
#'       y = 0,
#'       width = 700,
#'       height = 500,
#'       plot = "scatter",
#'       source = "plot:scatter",
#'       lock_aspect = FALSE,
#'       show_border = FALSE,
#'       z = 1
#'     ),
#'     canvas = list(width_px = 700, height_px = 500),
#'     device = list(width_in = 7, height_in = 5),
#'     image_assets = data.frame(),
#'     exit_reason = "example",
#'     created_at = Sys.time()
#'   ),
#'   class = "ggbond"
#' )
#' file <- tempfile(fileext = ".png")
#' render_ggbond(layout, plots, file = file, res = 72)
#' file.exists(file)
#'
#' @export
render_ggbond <- function(
    x,
    plot_list,
    file = NULL,
    device = NULL,
    width = x$device$width_in,
    height = x$device$height_in,
    res = 300
) {
  if (!inherits(x, "ggbond")) {
    stop("`x` must be a ggbond object returned by `run_ggbond()`.", call. = FALSE)
  }
  if (!is.list(plot_list) || is.null(names(plot_list)) || any(names(plot_list) == "")) {
    stop("`plot_list` must be a named list of plot objects.", call. = FALSE)
  }

  if (is.null(file)) {
    draw_layout_to_device(
      layout = x$layout,
      device_id = grDevices::dev.cur(),
      plot_list = plot_list,
      image_list = x$image_assets,
      canvas_width_px = x$canvas$width_px,
      canvas_height_px = x$canvas$height_px
    )
    return(invisible(x))
  }

  if (is.null(device)) {
    ext <- tolower(tools::file_ext(file))
    device <- if (ext %in% c("pdf", "png")) ext else "pdf"
  }
  device <- match.arg(device, c("pdf", "png"))

  if (device == "pdf") {
    grDevices::pdf(file, width = width, height = height)
  } else {
    grDevices::png(file, width = width, height = height, units = "in", res = res)
  }
  on.exit(grDevices::dev.off(), add = TRUE)

  draw_layout_to_device(
    layout = x$layout,
    device_id = grDevices::dev.cur(),
    plot_list = plot_list,
    image_list = x$image_assets,
    canvas_width_px = x$canvas$width_px,
    canvas_height_px = x$canvas$height_px
  )

  invisible(x)
}
