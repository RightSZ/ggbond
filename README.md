# ggbond

ggbond is a Shiny-based layout editor for arranging R plots on a
fixed-size canvas. It is designed for building multi-panel figures
interactively while preserving a predictable mapping between the browser
canvas and the final graphics device.

## Features

-   Add, move, resize, align, layer, and label panels on a fixed-size
    canvas.
-   Select multiple panels with Ctrl or Command.
-   Move selected panels with arrow keys; use Shift plus arrow keys for
    larger steps.
-   Undo movement, resizing, alignment, and layering changes with Ctrl
    or Command plus Z.
-   Render ggplot2 plots, base graphics functions or recorded plots,
    pheatmap objects, ComplexHeatmap objects, grid grobs, gtables, and
    uploaded raster images.
-   Lock the aspect ratio for image panels.
-   Optionally draw panel borders in the rendered output.
-   Export the current layout to PDF or PNG.
-   Stop the Shiny app automatically when the browser session closes.

## Installation

From the package directory:

``` r
install.packages("devtools")
devtools::install("ggbond")
```

## Quick Start

``` r
library(ggbond)

run_ggbond()
```

The demo app includes ggplot2 plots, a base R plot, and optional
pheatmap and ComplexHeatmap examples when those packages are installed.

## Using Custom Plots

Pass a named list of plot objects to `run_ggbond()`.

``` r
library(ggplot2)
library(ggbond)

plots <- list(
  scatter = ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    theme_classic(),
  base = function() {
    plot(mtcars$wt, mtcars$mpg, pch = 19, xlab = "wt", ylab = "mpg")
  }
)

run_ggbond(plots)
```

For pheatmap:

``` r
plots <- list(
  heatmap = pheatmap::pheatmap(as.matrix(mtcars[1:10, 1:6]), silent = TRUE)
)

run_ggbond(plots)
```

For ComplexHeatmap:

``` r
plots <- list(
  complex = ComplexHeatmap::Heatmap(as.matrix(mtcars[1:10, 1:6]))
)

run_ggbond(plots)
```

## Browser Workflow

1.  Click **Add panel** to add a panel to the canvas.
2.  Use the panel dropdown to choose a plot or uploaded image source.
3.  Drag panels to move them and use the bottom-right handle to resize.
4.  Hold Ctrl or Command while clicking panels to multi-select.
5.  Use the inspector to align panels, change layer order, toggle
    borders, or lock image aspect ratios.
6.  Click **Open fixed R plot window** when you want a live external
    preview.
7.  Export the layout with **Export PDF** or **Export PNG**.

Closing the browser session stops the Shiny app so the R console can be
used again.

## Notes

Base graphics are converted to grid output with gridGraphics when
possible so PDF exports remain vector-based. If conversion fails for a
custom base graphics function, ggbond falls back to high-resolution
raster rendering for that panel.
