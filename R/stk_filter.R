
#' Filter twilight values by range
#'
#' Filter out twilight values by range, and
#' returns the data frame with a twilight (TRUE/FALSE)
#' column or the data frame with only twilight values
#' selected (filtered out).
#'
#' Generally used for internal process, but can be
#' useful for visualizations of profiles as well.
#'
#' @param data a skytrackr compatible data frame
#' @param range a range c(min, max) of valid values in lux
#' @param plot plot daily profiles with the range filter applied
#' @param filter if TRUE only twilight values are returned if
#'  FALSE the data frame is returned with an annotation column
#'  called 'twilight' for further processing.
#'
#' @returns a skytrackr compatible data frame, either filtered
#'  to only include twilight values selected by the range parameter
#'  or with an additional 'twilight' column to annotate these values.
#' @export

stk_filter <- function(
    data,
    range,
    plot = FALSE,
    filter = FALSE
){

  # unravel the light data
  data <- data |>
    dplyr::filter(
      .data$measurement == "lux"
    )

  # filter data
  data <- data |>
    dplyr::group_by(.data$logger, .data$date) |>
    dplyr::do({

      first_low <- which(.data$value > range[1])[1]
      last_low <- tail(which(.data$value > range[1]), 1)
      first_high <- which(.data$value > range[2])[1] - 1
      last_high <- tail(which(.data$value > range[2]), 1) + 1
      idx <- 1:nrow(.data)

      first_low <- ifelse(
        is.na(first_low) || rlang::is_empty(first_low) , 1, first_low)
      last_low <- ifelse(
        is.na(last_low) || rlang::is_empty(last_low), nrow(.data), last_low)
      first_high <- ifelse(
        is.na(first_high) || rlang::is_empty(first_high), nrow(.data), first_high)
      last_high <- ifelse(
        is.na(last_high) || rlang::is_empty(last_high),
        1, last_high)

      df <- .data
      df$idx <- idx
      df$first_low <- first_low
      df$first_high <- first_high
      df$last_low <- last_low
      df$last_high <- last_high
      df
    })

  data <- data |>
    dplyr::group_by(.data$logger, .data$date) |>
    dplyr::mutate(
      twilight = ifelse(
      (.data$idx >= .data$first_low & .data$idx <= .data$first_high) | (.data$idx >= .data$last_high & .data$idx <= .data$last_low),
      TRUE, FALSE
    )
  )

  # cleanup
  data <- data |>
    dplyr::ungroup() |>
    dplyr::select(
      !dplyr::starts_with(c("first", "last","idx")),
    )

  # plot
  if(plot){
    p <- data |>
      ggplot2::ggplot() +
      ggplot2::geom_point(
        ggplot2::aes(
          .data$hour,
          log(.data$value),
          colour = .data$twilight
        )
      ) +
      ggplot2::labs(
        x = "hour",
        y = "log(lux)",
        title = "Diurnal light profile"
      ) +
      ggplot2::scale_color_manual(
        values = c("black","red")
      ) +
      ggplot2::theme_bw()
    plot(p)
  }

  # only retain twilight values
  if (filter){
    data <- data |>
      dplyr::filter(.data$twilight)
  }

  return(data)
}


