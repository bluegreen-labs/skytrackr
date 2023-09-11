
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

stk_profile <- function(data, logger, range = c(0.32, 400)) {

  # check for multiple logger
  # report first only or requested
  data <- data[data$logger == logger,]

  # subset data range
  data$lux[which(data$lux < range[1] | data$lux > range[2])] <- NA

  p <- ggplot(data) +
    geom_tile(
      aes(
        date,
        hour,
        fill = log(lux)
      )
    ) +
    labs(
      x = "Date",
      y = "Hour"
    ) +
    scale_fill_viridis_c() +
    theme_bw()

  fig <- plotly::ggplotly(p)
  plotly::config(fig, displaylogo = FALSE)
  print(fig)
}

