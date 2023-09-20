
#' Plot seasonal profiles
#'
#' Uses plotly to provide a seasonal profile plot
#'
#' @param data skytrackr compatible data frame
#' @param logger the logger to plot
#' @param range the light range to plot
#' @param center center data on day or night
#' @param plotly TRUE or FALSE (convert to dynamic plotly plot or not)
#'
#' @importFrom rlang .data
#'
#' @return a plotly dynamic graph of light levels for a given logger
#' @export

stk_profile <- function(
    data,
    logger,
    range = c(0, 100000),
    center = "day",
    plotly = FALSE
  ) {

  # silence spurious note
  . <- NULL

  # check for multiple logger
  # report first only or requested
  data <- data |>
    dplyr::filter(
      .data$logger == logger
    )

  # center on midnight
  if (center != "day"){
    data <- data |>
      dplyr::mutate(
        date_time = .data$date_time - 12*60*60,
        hour = as.numeric(format(.data$date_time,"%H")) +
          as.numeric(format(.data$date_time,"%M"))/60 +
          as.numeric(format(.data$date_time,"%S"))/3600,
        hour = .data$hour - 12
        )
  }

  # subset data range for light measurements
  #

  p <- data |>
    dplyr::group_by(.data$measurement) |>
    dplyr::do(p = {

      if(.$measurement[1] == "lux") {
        .data$value[
          which(.data$value < range[1] | .data$value > range[2])] <- NA
        .data$value <- log(.data$value)
      }

      if(.$measurement[1] == "temperature") {
        .data$value[
          which(.data$value > 50)] <- NA
      }

      ggplot2::ggplot(.data) +
        ggplot2::geom_raster(
          ggplot2::aes(
            .data$date,
            .data$hour,
            fill = .data$value
          )
        ) +
        ggplot2::labs(
          x = "Date",
          y = paste0(.data$measurement[1],"\n(Hour)")
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
      titleX = TRUE,
      titleY = TRUE,
      shareX = TRUE
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

