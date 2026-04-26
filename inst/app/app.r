library(shiny)
library(ggbond)

plot_list <- ggbond_demo_plots()

canvas_width_px <- 700
canvas_height_px <- 500
device_width_in <- 7
device_height_in <- 5

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

shinyApp(ui, server)
