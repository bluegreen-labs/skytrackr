
#' Plot seasonal profiles
#'
#' Uses plotly to provide a seasonal profile plot
#'
#' @param data skytrackr compatible data frame
#' @param logger the logger to plot
#' @param range the light range to plot
#'
#' @return a plotly dynamic graph of light levels for a given logger
#' @export

stk_profile <- function(
    data,
    logger,
    range = c(0, 100000),
    plotly = FALSE
  ) {

  # check for multiple logger
  # report first only or requested
  data <- data |>
    filter(
      logger == logger
    )

  # subset data range for light measurements
  #

  p <- data |>
    group_by(measurement) |>
    do(p = {

      if(.$measurement[1] == "light.lux.") {
        .data$value[
          which(.data$value < range[1] | .data$value > range[2])] <- NA
        .data$value <- log(.data$value)
      }

      if(.$measurement[1] == "T..C.") {
        .data$value[
          which(.data$value > 50)] <- NA
      }

      ggplot2::ggplot(.) +
        ggplot2::geom_raster(
          ggplot2::aes(
            .data$date,
            .data$hour,
            fill = .data$value
          )
        ) +
        ggplot2::labs(
          x = "Date",
          y = "Hour"
        ) +
        ggplot2::scale_fill_viridis_c(
          na.value = NA
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          legend.position = "none"
        )
    })

  if (plotly){

    plots <- lapply(p$p, function(x) {

      plotly::ggplotly(x)
    })

    plotly::subplot(
      plots,
      nrows = length(plots),
      shareX = TRUE,
      margin = c(0.8, 0.5, 0, 0)
      )

  } else {

    # combine plots in ggplot patchwork
    p <- patchwork::wrap_plots(
      p$p,
      nrow = length(unique(data$measurement))
    )

    return(p)
  }
}

