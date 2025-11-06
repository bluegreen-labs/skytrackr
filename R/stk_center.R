#' Centers diurnal curves on midday
#'
#' Values are centered on midday on a day by day basis. Note that this does
#' not shift the time series properly as hours are wrapped around. This
#' function should not be used stand-alone.
#'
#' @param df a skytrackr compatible data frame
#' @param floor threshold value to center the data with (default = 1.5)
#' @param replace replace original decimal hour values (default = FALSE)
#'
#' @returns a centered skytrackr compatible data frame
#' @export
#'
#' @examples
#'
#' cc876 |> stk_center()

stk_center <- function(df, floor = 1.5, replace = FALSE){

  # center the midday data using an hour correction
  df_center <- df |>
    dplyr::group_by(.data$logger, .data$date) |>
    dplyr::do({

      hour_data <- .data |>
        dplyr::filter(
          .data$value > floor
        ) |>
        dplyr::mutate(
          angle = .data$hour/24 * 360
        )

      # mean angle of valid hours
      a <-  suppressWarnings(
        as.numeric(
          circular::mean.circular(
            circular::as.circular(
            hour_data$angle,
            control.circular = list(
              zero = 0,
              units ="degrees",
              type = "angles"
              )
            )
          )
        )
      )

      # calculate offset in hours
      # and shift values to center on midday
      df <- .data |>
        dplyr::mutate(
          offset = 24 * (180 - a) / 360,
          hour_cor = .data$hour + .data$offset,
          hour_cor = ifelse(.data$hour_cor > 24, .data$hour_cor - 24, .data$hour_cor)
        )

      if (replace){
        df <- df |>
          dplyr::mutate(
            hour = .data$hour_cor
          ) |>
          dplyr::select(
            -"hour_cor"
          ) |>
          dplyr::arrange(.data$hour)
      }

      df
    })

  return(df_center)
}

