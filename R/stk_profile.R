#' Plot seasonal profiles
#'
#' Provides static or dynamic (plotly) seasonal profile plot
#'
#' @param data A skytrackr compatible data frame.
#' @param logger The logger to plot
#' @param plotly Logical, convert to dynamic plotly plot or not (default = FALSE)
#' @importFrom rlang .data
#'
#' @return A static or dynamic graph of light levels for a given logger.
#' @export

stk_profile <- function(
    data,
    logger,
    plotly = FALSE
  ) {

  # check for multiple logger
  # report first only or requested

  if(missing(logger)){
    logger <- unique(data$logger)[1]
  }

  data <- data |>
    dplyr::filter(
      .data$logger == logger
    )

  # round hour values
  data <- data |>
    dplyr::mutate(
      hour = round(.data$hour, 4)
    )

  p <- data |>
    dplyr::group_by(.data$measurement) |>
    dplyr::do(p = {

      if(.data$measurement[1] == "lux") {
        .data$value <- log(.data$value)
      }

      if(.data$measurement[1] == "temperature") {
        .data$value[
          which(.data$value > 50)] <- NA
      }

      ggplot2::ggplot(.data) +
        ggplot2::geom_tile(
          ggplot2::aes(
            .data$date,
            .data$hour,
            fill = .data$value
          ),
          height = 0.25
        ) +
        ggplot2::labs(
          title = .data$logger[1],
          x = "",
          y = paste0(.data$measurement[1],"\n(Hour)")
        ) +
        ggplot2::scale_fill_viridis_c(
          na.value = NA
        ) +
        ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
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

    # return single plot
    if (length(p$p) == 1){
      return(p$p[[1]])
    }

    # combine plots in ggplot patchwork
    p <- patchwork::wrap_plots(
      p$p,
      nrow = length(unique(data$measurement))
    )

    return(p)
  }
}

