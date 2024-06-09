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
                   plot.margin = ggplot2::margin(0))

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
