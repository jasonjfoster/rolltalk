"%+replace%" <- ggplot2::"%+replace%"

theme_jjf <- function(base_size = 11, base_family = "") {

  half_line <- base_size / 2

  ggplot2::theme_gray(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(line = ggplot2::element_line(colour = grDevices::rgb(217, 217, 217, max = 255),
                                                linewidth = base_size / 22, linetype = 1, lineend = "butt"),
                   axis.text = ggplot2::element_text(size = ggplot2::rel(0.8), colour = "black"),
                   axis.ticks = ggplot2::element_blank(),
                   legend.margin = ggplot2::margin(0),
                   legend.key = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(hjust = 0),
                   legend.title = ggplot2::element_blank(),
                   legend.position = "bottom",
                   panel.background = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_line(),
                   strip.background = ggplot2::element_blank(),
                   plot.background = ggplot2::element_blank(),
                   plot.margin = ggplot2::unit(c(0.01, 0, 0, 0), "inches")) # UPDATED

}

palette_jjf <- function(n_cols, n_rows = 1) {
  # if n_cols > length(colors) then repeat with transparency
  # otherwise repeat n_rows with transparency

  colors_jjf <- c(grDevices::rgb(236, 105,  65, maxColorValue = 255),
                  grDevices::rgb(253, 197, 129, maxColorValue = 255),
                  grDevices::rgb( 20,  76,  89, maxColorValue = 255),
                  grDevices::rgb( 22, 144, 133, maxColorValue = 255))

  rep_cols <- floor(n_cols / length(colors_jjf))
  rep_rows <- n_rows - 1

  rep_alpha <- rep_cols + rep_rows + 1
  a <- 1
  b <- 1 / rep_alpha

  if (rep_cols > 0) {

    result <- colors_jjf

    for (j in 1:rep_cols) {

      a <- a - b
      result <- c(result, scales::alpha(colors_jjf, alpha = a))

    }

    result <- result[1:n_cols]

  } else {
    result <- colors_jjf[1:n_cols]
  }

  if (rep_rows > 0) {

    result_ls <- list(result)

    for (i in 1:rep_rows) {

      a <- a - b
      result_ls <- append(result_ls, list(scales::alpha(result_ls[[i]], alpha = a)))

    }

    result <- do.call(c, result_ls)

  }

  return(result)

}

scale_color_jjf <- function(...) {

  ggplot2::discrete_scale("color", palette = palette_jjf, ...)

}

scale_fill_jjf <- function(...) {

  ggplot2::discrete_scale("fill", palette = palette_jjf, ...)

}

capitalize <- function(string) {
  string <- as.character(string)
  gsub("^(\\w)", "\\U\\1", string, perl = TRUE)
}

plot_jjf <- function(dt, x, y, z, decomp, title, xlab, ylab, multiple, palette, stack) {

  if (!is.null(palette)) {
    dt[ , (z) := factor(get(z), levels = names(palette))]
  } else {
    dt[ , (z) := factor(capitalize(get(z)), levels = unique(capitalize(get(z))))]
  }

  result <- ggplot2::ggplot() +
    theme_jjf() +
    ggplot2::labs(title = title, x = xlab, y = ylab)

  if (stack) {

    result <- result +
      ggplot2::geom_area(data = dt[get(z) != decomp],
                         ggplot2::aes(x = get(x), y = get(y) * multiple, fill = get(z))) +
      ggplot2::geom_line(data = dt[get(z) == decomp],
                         ggplot2::aes(x = get(x), y = get(y) * multiple, color = get(z)))

    if (!is.null(palette)) {

      result <- result +
        ggplot2::scale_fill_manual(values = palette)
      # ggplot2::scale_fill_manual(values = palette, guide = ggplot2::guide_legend(order = 2)) +
      # ggplot2::scale_color_manual(values = palette, guide = ggplot2::guide_legend(order = 1))

    } else {

      result <- result +
        scale_fill_jjf(guide = ggplot2::guide_legend(order = 2)) +
        ggplot2::scale_color_manual(values = "black", guide = ggplot2::guide_legend(order = 1))

    }

  } else {

    result <- result +
      ggplot2::geom_line(data = dt,
                         ggplot2::aes(x = get(x), y = get(y) * multiple, color = get(z)))

    if (!is.null(palette)) {

      result <- result +
        ggplot2::scale_color_manual(values = palette)

    } else {

      result <- result +
        scale_color_jjf()

    }

  }

  return(result)

}

plot_ts <- function(dt, x = "index", y = "value", z = "variable",
                    title = NULL, xlab = NULL, ylab = NULL,
                    multiple = 1, palette = NULL, stack = FALSE) {

  result <- plot_jjf(dt, x, y, z, NULL, title, xlab, ylab, multiple, palette, stack)

  return(result)

}
