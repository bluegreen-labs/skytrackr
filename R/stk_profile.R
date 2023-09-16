
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
    range = c(0.32, 400),
    plotly = FALSE
  ) {

  # check for multiple logger
  # report first only or requested
  data <- data |>
    filter(
      logger == logger
    )

  # subset data range for light measurements
  data <- data |>
    mutate(
     measurement =
    )
  data$lux[which(data$lux < range[1] | data$lux > range[2])] <- NA

  p <- ggplot2::ggplot(data) +
    ggplot2::geom_tile(
      ggplot2::aes(
        .data$date,
        .data$hour,
        fill = log(.data$lux)
      )
    ) +
    ggplot2::labs(
      x = "Date",
      y = "Hour"
    ) +
    ggplot2::scale_fill_viridis_c(
      na.value = NA
    ) +
    ggplot2::theme_bw()

  if (plotly){
    fig <- plotly::ggplotly(p)
    plotly::config(fig, displaylogo = FALSE)
    print(fig)
  }

  return(p)
}

