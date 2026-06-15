#' Create the ggbond Shiny server function
#'
#' Builds the server-side logic for the ggbond layout editor. Most users should
#' call `run_ggbond()` instead of using this function directly.
#'
#' @param plot_list A named list of plot objects.
#' @param canvas_width_px Canvas width in pixels.
#' @param canvas_height_px Canvas height in pixels.
#' @param device_width_in Graphics device width in inches.
#' @param device_height_in Graphics device height in inches.
#'
#' @return A Shiny server function.
#' @importFrom grDevices dev.off pdf png
#' @importFrom shiny actionButton checkboxInput downloadHandler isolate
#'   numericInput observeEvent p reactive reactiveVal renderText renderUI
#'   selectInput showNotification tagList tags textInput
#' @keywords internal
#' @noRd
ggbond_server <- function(
    plot_list,
    canvas_width_px = 700,
    canvas_height_px = 500,
    device_width_in = 7,
    device_height_in = 5
) {
  force(plot_list)
  force(canvas_width_px)
  force(canvas_height_px)
  force(device_width_in)
  force(device_height_in)

  function(input, output, session) {
    layout_state <- reactiveVal("[]")
    selected_panel_id <- reactiveVal(NULL)
    selected_panel_ids <- reactiveVal(character())
    image_assets <- reactiveVal(data.frame(
      id = character(),
      label = character(),
      path = character(),
      aspect = numeric(),
      stringsAsFactors = FALSE
    ))
    device_id <- reactiveVal(NULL)

    source_choices <- reactive({
      plot_values <- paste0("plot:", names(plot_list))
      names(plot_values) <- paste0("Plot: ", names(plot_list))

      images <- image_assets()
      image_values <- paste0("image:", images$id)
      names(image_values) <- paste0("Image: ", images$label)

      c(plot_values, image_values)
    })

    send_source_choices <- function() {
      plots <- names(plot_list)
      images <- isolate(image_assets())

      choices <- c(
        lapply(plots, function(plot_name) {
          list(
            value = paste0("plot:", plot_name),
            label = paste0("Plot: ", plot_name),
            type = "plot",
            aspect = NULL
          )
        }),
        lapply(seq_len(nrow(images)), function(i) {
          list(
            value = paste0("image:", images$id[[i]]),
            label = paste0("Image: ", images$label[[i]]),
            type = "image",
            aspect = images$aspect[[i]]
          )
        })
      )

      session$sendCustomMessage(
        "update_source_choices",
        choices
      )

      invisible(choices)
    }

    parse_layout <- function() {
      txt <- layout_state()

      if (is.null(txt) || txt == "" || txt == "[]") {
        return(data.frame())
      }

      out <- jsonlite::fromJSON(txt)

      if (is.null(out) || length(out) == 0) {
        return(data.frame())
      }

      out
    }

    redraw_device <- function(layout) {
      id <- device_id()

      if (is.null(id) || !id %in% grDevices::dev.list()) {
        return(invisible(NULL))
      }

      tryCatch(
        draw_layout_to_device(
          layout = layout,
          device_id = id,
          plot_list = plot_list,
          image_list = image_assets(),
          canvas_width_px = canvas_width_px,
          canvas_height_px = canvas_height_px
        ),
        error = function(e) {
          showNotification(
            paste("Render failed:", conditionMessage(e)),
            type = "error"
          )
        }
      )

      invisible(NULL)
    }

    close_preview_device <- function() {
      id <- isolate(device_id())

      if (!is.null(id) && id %in% grDevices::dev.list()) {
        grDevices::dev.off(id)
      }

      device_id(NULL)
      invisible(NULL)
    }

    build_return_value <- function(exit_reason = "unknown") {
      txt <- isolate(layout_state())
      layout <- if (is.null(txt) || txt == "" || txt == "[]") {
        data.frame()
      } else {
        out <- jsonlite::fromJSON(txt)
        if (is.null(out) || length(out) == 0) {
          data.frame()
        } else {
          out
        }
      }

      new_ggbond(
        layout = layout,
        canvas = list(
          width_px = canvas_width_px,
          height_px = canvas_height_px
        ),
        device = list(
          width_in = device_width_in,
          height_in = device_height_in
        ),
        image_assets = isolate(image_assets()),
        exit_reason = exit_reason
      )
    }

    get_selected_panel <- reactive({
      layout <- parse_layout()
      sid <- selected_panel_id()

      if (is.null(sid) || nrow(layout) == 0) {
        return(NULL)
      }

      idx <- which(layout$id == sid)

      if (length(idx) == 0) {
        return(NULL)
      }

      layout[idx[1], , drop = FALSE]
    })

    get_selected_panels <- reactive({
      layout <- parse_layout()
      sids <- selected_panel_ids()

      if (length(sids) == 0 || nrow(layout) == 0) {
        return(data.frame())
      }

      layout[layout$id %in% sids, , drop = FALSE]
    })

    observeEvent(input$open_device, {
      id <- open_fixed_device(
        width = device_width_in,
        height = device_height_in
      )
      device_id(id)

      redraw_device(parse_layout())
    })

    session$onFlushed(function() {
      send_source_choices()
    }, once = TRUE)

    observeEvent(input$ggbond_client_ready, {
      send_source_choices()
    }, ignoreInit = TRUE)

    observeEvent(input$add_panel, {
      # Default to the first plot source when one is available.
      default_source <- if (length(plot_list) > 0) {
        paste0("plot:", names(plot_list)[1])
      } else {
        ""
      }
      session$sendCustomMessage(
        "add_panel",
        list(
          source = default_source,
          choices = send_source_choices()
        )
      )
    })

    observeEvent(input$delete_panel, {
      session$sendCustomMessage(
        "delete_selected_panels",
        list(nonce = input$delete_panel)
      )
    })

    observeEvent(input$exit_app, {
      return_value <- build_return_value("button")
      close_preview_device()
      shiny::stopApp(returnValue = return_value)
    })

    observeEvent(input$image_files, {
      files <- input$image_files
      if (is.null(files) || nrow(files) == 0) {
        return(NULL)
      }

      existing <- image_assets()
      new_assets <- lapply(seq_len(nrow(files)), function(i) {
        ext <- tolower(tools::file_ext(files$name[[i]]))

        if (!ext %in% c("png", "jpg", "jpeg", "tif", "tiff")) {
          return(NULL)
        }

        id <- paste0(
          "img_",
          format(Sys.time(), "%Y%m%d%H%M%OS3"),
          "_",
          i,
          "_",
          sample.int(1e6, 1)
        )
        target <- file.path(tempdir(), paste0(id, ".", ext))
        file.copy(files$datapath[[i]], target, overwrite = TRUE)
        image_dim <- dim(read_panel_image(target))
        aspect <- image_dim[[2]] / image_dim[[1]]

        data.frame(
          id = id,
          label = files$name[[i]],
          path = target,
          aspect = aspect,
          stringsAsFactors = FALSE
        )
      })
      new_assets <- do.call(rbind, Filter(Negate(is.null), new_assets))

      if (!is.null(new_assets) && nrow(new_assets) > 0) {
        image_assets(rbind(existing, new_assets))
        send_source_choices()
      }
    })

    observeEvent(input$selected_panel_id, {
      selected_panel_id(input$selected_panel_id)
    })

    observeEvent(input$selected_panel_ids, {
      ids <- jsonlite::fromJSON(input$selected_panel_ids)
      if (is.null(ids) || length(ids) == 0) {
        ids <- character()
      }
      ids <- as.character(ids)
      selected_panel_ids(ids)
    })

    observeEvent(input$layout_state, {
      layout_state(input$layout_state)

      layout <- parse_layout()

      redraw_device(layout)
    })

    output$layout_text <- renderText({
      layout_state()
    })

    output$panel_inspector <- renderUI({
      panel <- get_selected_panel()
      panels <- get_selected_panels()

      if (nrow(panels) > 1) {
        return(
          tagList(
            tags$p(
              tags$b("Selected panels: "),
              paste(panels$id, collapse = ", ")
            ),
            tags$div(
              class = "align-toolbar",
              actionButton("align_left", "Align left"),
              actionButton("align_right", "Align right"),
              actionButton("align_center_h", "Align horizontal center"),
              actionButton("align_top", "Align top"),
              actionButton("align_center_v", "Align vertical center"),
              actionButton("align_bottom", "Align bottom")
            ),
            tags$hr(),
            tags$div(
              class = "size-toolbar",
              actionButton("match_width", "Equal width"),
              actionButton("match_height", "Equal height"),
              actionButton("match_size", "Equal size")
            ),
            tags$hr(),
            tags$div(
              class = "layer-toolbar",
              actionButton("layer_top", "Bring to front"),
              actionButton("layer_bottom", "Send to back")
            )
          )
        )
      }

      if (is.null(panel)) {
        return(
          tagList(
            p("No panel selected."),
            p("Click a panel on the canvas to edit its properties.")
          )
        )
      }

      panel_source <- if ("source" %in% names(panel)) {
        panel$source
      } else {
        paste0("plot:", panel$plot)
      }
      is_image_panel <- startsWith(as.character(panel_source), "image:")
      lock_aspect <- if ("lock_aspect" %in% names(panel)) {
        isTRUE(panel$lock_aspect)
      } else {
        FALSE
      }
      show_border <- if ("show_border" %in% names(panel)) {
        isTRUE(panel$show_border)
      } else {
        FALSE
      }

      tagList(
        tags$p(
          tags$b("Selected: "),
          panel$id
        ),

        textInput(
          "inspector_label",
          "Panel label",
          value = panel$label
        ),

        selectInput(
          "inspector_source",
          "Panel source",
          choices = source_choices(),
          selected = panel_source
        ),

        tags$hr(),

        numericInput(
          "inspector_x",
          "x",
          value = round(panel$x, 1),
          min = 0,
          max = canvas_width_px,
          step = 1
        ),

        numericInput(
          "inspector_y",
          "y",
          value = round(panel$y, 1),
          min = 0,
          max = canvas_height_px,
          step = 1
        ),

        numericInput(
          "inspector_width",
          "width",
          value = round(panel$width, 1),
          min = 100,
          max = canvas_width_px,
          step = 1
        ),

        numericInput(
          "inspector_height",
          "height",
          value = round(panel$height, 1),
          min = 80,
          max = canvas_height_px,
          step = 1
        ),

        if (is_image_panel) {
          checkboxInput(
            "inspector_lock_aspect",
            "Lock aspect ratio",
            value = lock_aspect
          )
        },

        checkboxInput(
          "inspector_show_border",
          "Show border",
          value = show_border
        ),

        tags$hr(),

        tags$div(
          class = "layer-toolbar",
          actionButton("layer_top", "Bring to front"),
          actionButton("layer_bottom", "Send to back")
        ),

        actionButton(
          "apply_panel_changes",
          "Apply changes",
          class = "btn-warning"
        )
      )
    })
    shiny::outputOptions(output, "panel_inspector", suspendWhenHidden = FALSE)

    observeEvent(input$align_left, {
      session$sendCustomMessage("align_selected_panels", list(mode = "left"))
    })

    observeEvent(input$align_right, {
      session$sendCustomMessage("align_selected_panels", list(mode = "right"))
    })

    observeEvent(input$align_center_h, {
      session$sendCustomMessage("align_selected_panels", list(mode = "center_h"))
    })

    observeEvent(input$align_top, {
      session$sendCustomMessage("align_selected_panels", list(mode = "top"))
    })

    observeEvent(input$align_center_v, {
      session$sendCustomMessage("align_selected_panels", list(mode = "center_v"))
    })

    observeEvent(input$align_bottom, {
      session$sendCustomMessage("align_selected_panels", list(mode = "bottom"))
    })

    observeEvent(input$match_width, {
      session$sendCustomMessage("resize_selected_panels", list(mode = "width"))
    })

    observeEvent(input$match_height, {
      session$sendCustomMessage("resize_selected_panels", list(mode = "height"))
    })

    observeEvent(input$match_size, {
      session$sendCustomMessage("resize_selected_panels", list(mode = "size"))
    })

    observeEvent(input$layer_top, {
      session$sendCustomMessage("set_selected_layer", list(direction = "top"))
    })

    observeEvent(input$layer_bottom, {
      session$sendCustomMessage("set_selected_layer", list(direction = "bottom"))
    })

    observeEvent(input$apply_panel_changes, {
      panel <- get_selected_panel()

      if (is.null(panel)) {
        return(NULL)
      }

      sid <- selected_panel_id()

      session$sendCustomMessage(
        "update_panel_from_inspector",
        list(
          id = sid,
          label = input$inspector_label,
          source = input$inspector_source,
          lock_aspect = isTRUE(input$inspector_lock_aspect),
          show_border = isTRUE(input$inspector_show_border),
          x = input$inspector_x,
          y = input$inspector_y,
          width = input$inspector_width,
          height = input$inspector_height
        )
      )
    })

    output$download_pdf <- downloadHandler(
      filename = function() {
        paste0("ggbond_layout_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        layout <- parse_layout()

        pdf(
          file,
          width = device_width_in,
          height = device_height_in
        )

        draw_layout_to_device(
          layout = layout,
          device_id = grDevices::dev.cur(),
          plot_list = plot_list,
          image_list = image_assets(),
          canvas_width_px = canvas_width_px,
          canvas_height_px = canvas_height_px
        )

        dev.off()
      }
    )

    output$download_png <- downloadHandler(
      filename = function() {
        paste0("ggbond_layout_", Sys.Date(), ".png")
      },
      content = function(file) {
        layout <- parse_layout()

        png(
          file,
          width = device_width_in,
          height = device_height_in,
          units = "in",
          res = 300
        )

        draw_layout_to_device(
          layout = layout,
          device_id = grDevices::dev.cur(),
          plot_list = plot_list,
          image_list = image_assets(),
          canvas_width_px = canvas_width_px,
          canvas_height_px = canvas_height_px
        )

        dev.off()
      }
    )

    session$onSessionEnded(function() {
      return_value <- build_return_value("session_ended")
      close_preview_device()
      shiny::stopApp(returnValue = return_value)
    })
  }
}
