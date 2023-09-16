
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

  # config
  plotly::config(fig, displaylogo = FALSE)

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
    do(gg = {

      if(.$measurement[1] == "light.lux.") {
        .data$value[
          which(.data$value < range[1] | .data$value > range[2])] <- NA
        .data$value <- log(.data$value)
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
        ggplot2::theme_bw()
    })

  if (plotly){

    fig <- plotly::ggplotly(p)

    print(fig)
  } else {

    # combine plots in ggplot patchwork
    p <- patchwork::wrap_plots(
      p$gg,
      nrow = length(unique(data$measurement))
    )

    return(p)
  }
}

