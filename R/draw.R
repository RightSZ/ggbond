#' Draw a saved panel layout to a graphics device
#'
#' Renders the current ggbond panel layout into an already-open graphics device.
#' The renderer supports ggplot2 plots, base graphics functions or recorded
#' plots, pheatmap objects, ComplexHeatmap objects, grobs, gtables, and uploaded
#' image assets.
#'
#' @param layout A data frame describing panel positions, sizes, sources, and
#'   optional display flags.
#' @param device_id Numeric graphics device id.
#' @param plot_list A named list of plot objects.
#' @param image_list Optional data frame of uploaded image assets with `id`,
#'   `label`, `path`, and `aspect` columns.
#' @param canvas_width_px Canvas width in pixels.
#' @param canvas_height_px Canvas height in pixels.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
draw_layout_to_device <- function(
    layout,
    device_id,
    plot_list,
    image_list = NULL,
    canvas_width_px = 700,
    canvas_height_px = 500
) {
  if (is.null(device_id)) return(NULL)
  if (!device_id %in% grDevices::dev.list()) return(NULL)

  previous_device <- as.integer(grDevices::dev.cur())
  restore_previous_device <- previous_device != as.integer(device_id) &&
    previous_device %in% grDevices::dev.list()

  invisible(grDevices::dev.set(device_id))
  on.exit({
    if (restore_previous_device && previous_device %in% grDevices::dev.list()) {
      invisible(grDevices::dev.set(previous_device))
    }
  }, add = TRUE)

  grid::grid.newpage()

  grid::grid.rect(
    x = grid::unit(0.5, "npc"),
    y = grid::unit(0.5, "npc"),
    width = grid::unit(1, "npc"),
    height = grid::unit(1, "npc"),
    gp = grid::gpar(fill = "white", col = NA)
  )

  if (is.null(layout) || nrow(layout) == 0) {
    grid::grid.text(
      "Drag panels in the Shiny canvas",
      x = 0.5,
      y = 0.5,
      gp = grid::gpar(fontsize = 16, col = "grey40")
    )
    return(invisible(NULL))
  }

  if (!"z" %in% names(layout)) {
    layout$z <- seq_len(nrow(layout))
  }

  layout <- layout[order(layout$z), , drop = FALSE]

  for (i in seq_len(nrow(layout))) {
    panel <- layout[i, ]

    x_npc <- (panel$x + panel$width / 2) / canvas_width_px
    y_npc <- 1 - (panel$y + panel$height / 2) / canvas_height_px
    w_npc <- panel$width / canvas_width_px
    h_npc <- panel$height / canvas_height_px

    panel_source <- if ("source" %in% names(panel)) {
      as.character(panel$source)
    } else {
      paste0("plot:", as.character(panel$plot))
    }

    raster_plot <- NULL
    selected_plot <- NULL

    if (!startsWith(panel_source, "image:")) {
      plot_name <- sub("^plot:", "", panel_source)
      selected_plot <- plot_list[[plot_name]]

      if (!is.null(selected_plot) && needs_raster_panel_plot(selected_plot)) {
        raster_plot <- rasterize_panel_plot(
          selected_plot,
          width_px = max(400, round(panel$width * 4)),
          height_px = max(320, round(panel$height * 4))
        )
      }
    }

    vp <- grid::viewport(
      x = grid::unit(x_npc, "npc"),
      y = grid::unit(y_npc, "npc"),
      width = grid::unit(w_npc, "npc"),
      height = grid::unit(h_npc, "npc"),
      clip = "on"
    )

    grid::pushViewport(vp)
    tryCatch(
      {
        if (startsWith(panel_source, "image:")) {
          image_id <- sub("^image:", "", panel_source)
          image_path <- NULL

          if (!is.null(image_list) && nrow(image_list) > 0) {
            match_idx <- which(image_list$id == image_id)
            if (length(match_idx) > 0) {
              image_path <- image_list$path[[match_idx[1]]]
            }
          }

          if (!is.null(image_path) && file.exists(image_path)) {
            grid::grid.raster(
              read_panel_image(image_path),
              width = grid::unit(1, "npc"),
              height = grid::unit(1, "npc"),
              interpolate = TRUE
            )
          } else {
            grid::grid.text(
              "Image not found",
              x = 0.5,
              y = 0.5,
              gp = grid::gpar(fontsize = 10, col = "grey45")
            )
          }
        } else {
          if (!is.null(selected_plot)) {
            if (!is.null(raster_plot)) {
              grid::grid.raster(
                raster_plot,
                width = grid::unit(1, "npc"),
                height = grid::unit(1, "npc"),
                interpolate = TRUE
              )
            } else {
              draw_panel_plot(
                selected_plot,
                width_px = max(400, round(panel$width * 4)),
                height_px = max(320, round(panel$height * 4))
              )
            }
          } else {
            grid::grid.text(
              "Plot not found",
              x = 0.5,
              y = 0.5,
              gp = grid::gpar(fontsize = 10, col = "grey45")
            )
          }
        }
      },
      error = function(e) {
        grid::grid.text(
          paste("Render failed:", conditionMessage(e)),
          x = 0.5,
          y = 0.5,
          gp = grid::gpar(fontsize = 8, col = "firebrick")
        )
      },
      finally = {
        grid::popViewport()
      }
    )

    show_border <- if ("show_border" %in% names(panel)) {
      isTRUE(panel$show_border)
    } else {
      FALSE
    }

    if (show_border) {
      grid::grid.rect(
        x = grid::unit(x_npc, "npc"),
        y = grid::unit(y_npc, "npc"),
        width = grid::unit(w_npc, "npc"),
        height = grid::unit(h_npc, "npc"),
        gp = grid::gpar(fill = NA, col = "black", lwd = 1)
      )
    }

    grid::grid.text(
      panel$label,
      x = grid::unit(panel$x / canvas_width_px, "npc") + grid::unit(4, "pt"),
      y = grid::unit(1 - panel$y / canvas_height_px, "npc") - grid::unit(4, "pt"),
      just = c("left", "top"),
      gp = grid::gpar(fontface = "bold", fontsize = 13)
    )
  }

  invisible(NULL)
}

#' Check whether a plot object needs raster rendering
#'
#' @param plot_object A plot-like object.
#'
#' @return A logical scalar.
#' @keywords internal
#' @noRd
needs_raster_panel_plot <- function(plot_object) {
  if (
    inherits(plot_object, "ggplot") ||
      inherits(plot_object, "pheatmap") ||
      inherits(plot_object, "Heatmap") ||
      inherits(plot_object, "HeatmapList") ||
      inherits(plot_object, "grob") ||
      inherits(plot_object, "gTree") ||
      inherits(plot_object, "gtable")
  ) {
    return(FALSE)
  }

  if (
    (is.function(plot_object) || inherits(plot_object, "recordedplot")) &&
      requireNamespace("gridGraphics", quietly = TRUE)
  ) {
    return(FALSE)
  }

  TRUE
}

#' Draw one plot object inside the current viewport
#'
#' @param plot_object A plot-like object.
#' @param width_px Fallback raster width in pixels.
#' @param height_px Fallback raster height in pixels.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
draw_panel_plot <- function(plot_object, width_px = 700, height_px = 500) {
  if (inherits(plot_object, "ggplot")) {
    grid::grid.draw(ggplot2::ggplotGrob(plot_object))
    return(invisible(NULL))
  }

  if (inherits(plot_object, "pheatmap")) {
    grid::grid.draw(plot_object$gtable)
    return(invisible(NULL))
  }

  if (
    inherits(plot_object, "Heatmap") ||
      inherits(plot_object, "HeatmapList")
  ) {
    if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) {
      stop("Package 'ComplexHeatmap' is required.", call. = FALSE)
    }

    ComplexHeatmap::draw(
      plot_object,
      newpage = FALSE,
      heatmap_legend_side = "right",
      annotation_legend_side = "right"
    )
    return(invisible(NULL))
  }

  if (inherits(plot_object, "grob") || inherits(plot_object, "gTree")) {
    grid::grid.draw(plot_object)
    return(invisible(NULL))
  }

  if (inherits(plot_object, "gtable")) {
    grid::grid.draw(plot_object)
    return(invisible(NULL))
  }

  if (
    (is.function(plot_object) || inherits(plot_object, "recordedplot")) &&
      requireNamespace("gridGraphics", quietly = TRUE)
  ) {
    tryCatch(
      draw_base_plot_as_grid(plot_object),
      error = function(e) {
        grid::grid.raster(
          rasterize_panel_plot(plot_object, width_px, height_px),
          width = grid::unit(1, "npc"),
          height = grid::unit(1, "npc"),
          interpolate = TRUE
        )
      }
    )
    return(invisible(NULL))
  }

  grid::grid.text(
    "Unsupported plot object",
    x = 0.5,
    y = 0.5,
    gp = grid::gpar(fontsize = 10, col = "grey45")
  )

  invisible(NULL)
}

#' Convert a base graphics plot to grid output
#'
#' @param plot_object A zero-argument plotting function or recorded plot.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
draw_base_plot_as_grid <- function(plot_object) {
  if (!requireNamespace("gridGraphics", quietly = TRUE)) {
    stop("Package 'gridGraphics' is required.", call. = FALSE)
  }

  gridGraphics::grid.echo(plot_object, newpage = FALSE)

  invisible(NULL)
}

#' Rasterize a base graphics plot
#'
#' @param plot_object A zero-argument plotting function, recorded plot, or
#'   object accepted by [graphics::plot()].
#' @param width_px Output width in pixels.
#' @param height_px Output height in pixels.
#'
#' @return A raster array as returned by [png::readPNG()].
#' @keywords internal
#' @noRd
rasterize_panel_plot <- function(plot_object, width_px, height_px) {
  if (!requireNamespace("png", quietly = TRUE)) {
    stop("Package 'png' is required to render base plots.", call. = FALSE)
  }

  file <- tempfile(fileext = ".png")
  grDevices::png(
    filename = file,
    width = width_px,
    height = height_px,
    units = "px",
    res = 192,
    bg = "white"
  )
  on.exit({
    grDevices::dev.off()
    unlink(file)
  }, add = TRUE)

  graphics::par(mar = c(4, 4, 2, 1))

  if (is.function(plot_object)) {
    plot_object()
  } else if (inherits(plot_object, "recordedplot")) {
    grDevices::replayPlot(plot_object)
  } else {
    plot(plot_object)
  }

  grDevices::dev.off()
  on.exit(unlink(file), add = FALSE)

  png::readPNG(file)
}

#' Read an image file for panel rendering
#'
#' @param path Path to a PNG, JPEG, or TIFF image.
#'
#' @return A raster array suitable for [grid::grid.raster()].
#' @keywords internal
#' @noRd
read_panel_image <- function(path) {
  ext <- tolower(tools::file_ext(path))

  if (ext == "png") {
    if (!requireNamespace("png", quietly = TRUE)) {
      stop("Package 'png' is required to render PNG images.", call. = FALSE)
    }

    return(png::readPNG(path))
  }

  if (ext %in% c("jpg", "jpeg")) {
    if (!requireNamespace("jpeg", quietly = TRUE)) {
      stop("Package 'jpeg' is required to render JPEG images.", call. = FALSE)
    }

    return(jpeg::readJPEG(path))
  }

  if (ext %in% c("tif", "tiff")) {
    if (!requireNamespace("tiff", quietly = TRUE)) {
      stop("Package 'tiff' is required to render TIFF images.", call. = FALSE)
    }

    return(tiff::readTIFF(path, all = FALSE))
  }

  stop("Unsupported image type: ", ext, call. = FALSE)
}
