#' Create demo plots for ggbond
#'
#' This function creates a small set of example plot objects used by the
#' ggbond demo Shiny app, including ggplot2 and optional non-ggplot examples.
#'
#' @return A named list of plot objects.
#'
#' @examples
#' plots <- ggbond_demo_plots()
#' names(plots)
#' length(plots)
#'
#' @export
ggbond_demo_plots <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.", call. = FALSE)
  }

  set.seed(123)

  demo_data <- data.frame(
    sample = paste0("S", seq_len(60)),
    group = rep(c("Control", "Treatment1", "Treatment2"), each = 20),
    x = stats::rnorm(60),
    y = c(
      stats::rnorm(20, mean = 5, sd = 1),
      stats::rnorm(20, mean = 6, sd = 1),
      stats::rnorm(20, mean = 7, sd = 1)
    ),
    value = c(
      stats::rnorm(20, mean = 10, sd = 2),
      stats::rnorm(20, mean = 14, sd = 2),
      stats::rnorm(20, mean = 18, sd = 2)
    )
  )

  bar_data <- stats::aggregate(value ~ group, data = demo_data, FUN = mean)
  bar_sd <- stats::aggregate(value ~ group, data = demo_data, FUN = stats::sd)
  bar_data$sd <- bar_sd$value

  heatmap_data <- expand.grid(
    Gene = paste0("Gene", seq_len(8)),
    Sample = paste0("Sample", seq_len(10))
  )

  heatmap_data$Expression <- stats::rnorm(
    n = nrow(heatmap_data),
    mean = rep(seq(-1.5, 1.5, length.out = 8), each = 10),
    sd = 0.7
  )

  p_scatter <- ggplot2::ggplot(
    demo_data,
    ggplot2::aes(x = x, y = y, color = group)
  ) +
    ggplot2::geom_point(size = 2, alpha = 0.85) +
    ggplot2::geom_smooth(
      method = "lm",
      se = FALSE,
      linewidth = 0.6
    ) +
    ggplot2::theme_classic(base_size = 10) +
    ggplot2::labs(
      title = "A. Scatter plot",
      x = "Variable X",
      y = "Variable Y",
      color = "Group"
    )

  p_box <- ggplot2::ggplot(
    demo_data,
    ggplot2::aes(x = group, y = value, fill = group)
  ) +
    ggplot2::geom_boxplot(
      width = 0.55,
      outlier.shape = NA,
      alpha = 0.75
    ) +
    ggplot2::geom_jitter(
      width = 0.12,
      size = 1.2,
      alpha = 0.55
    ) +
    ggplot2::theme_classic(base_size = 10) +
    ggplot2::labs(
      title = "B. Boxplot",
      x = NULL,
      y = "Value"
    ) +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
    )

  p_bar <- ggplot2::ggplot(
    bar_data,
    ggplot2::aes(x = group, y = value, fill = group)
  ) +
    ggplot2::geom_col(
      width = 0.6,
      alpha = 0.8
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = value - sd, ymax = value + sd),
      width = 0.15,
      linewidth = 0.4
    ) +
    ggplot2::theme_classic(base_size = 10) +
    ggplot2::labs(
      title = "C. Bar plot",
      x = NULL,
      y = "Mean value"
    ) +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
    )

  p_heatmap <- ggplot2::ggplot(
    heatmap_data,
    ggplot2::aes(x = Sample, y = Gene, fill = Expression)
  ) +
    ggplot2::geom_tile(
      color = "white",
      linewidth = 0.25
    ) +
    ggplot2::scale_fill_gradient2(
      low = "#2166AC",
      mid = "white",
      high = "#B2182B",
      midpoint = 0
    ) +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::labs(
      title = "D. Heatmap",
      x = NULL,
      y = NULL,
      fill = "Expression"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2::element_blank()
    )

  base_scatter <- local({
    data <- demo_data

    function() {
      graphics::par(mar = c(3, 3, 2, 1), mgp = c(1.8, 0.6, 0))

      graphics::plot(
        data$x,
        data$y,
        col = as.integer(factor(data$group)) + 1,
        pch = 19,
        xlab = "Variable X",
        ylab = "Variable Y",
        main = "E. Base R scatter"
      )
      graphics::legend(
        "topleft",
        legend = levels(factor(data$group)),
        col = seq_along(levels(factor(data$group))) + 1,
        pch = 19,
        bty = "n",
        cex = 0.8
      )
    }
  })
  class(base_scatter) <- c("ggbond_base_plot", class(base_scatter))

  demo_plots <- list(
    A = p_scatter,
    B = p_box,
    C = p_bar,
    D = p_heatmap,
    E = base_scatter
  )

  matrix_data <- matrix(
    stats::rnorm(80, mean = rep(seq(-1.5, 1.5, length.out = 8), each = 10)),
    nrow = 8,
    ncol = 10,
    dimnames = list(
      paste0("Gene", seq_len(8)),
      paste0("Sample", seq_len(10))
    )
  )

  if (requireNamespace("pheatmap", quietly = TRUE)) {
    demo_plots$F <- pheatmap::pheatmap(
      matrix_data,
      silent = TRUE,
      main = "F. pheatmap"
    )
  }

  if (requireNamespace("ComplexHeatmap", quietly = TRUE)) {
    demo_plots$G <- ComplexHeatmap::Heatmap(
      matrix_data,
      name = "z",
      column_title = "G. ComplexHeatmap",
      cluster_rows = TRUE,
      cluster_columns = TRUE
    )
  }

  demo_plots
}
