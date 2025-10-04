#' Twilight screening routine
#'
#' Removes poor quality data based on twilight heuristics. Allows for quick
#' screening of data containing "false" twilight values.
#'
#' @param df A skytrackr data frame.
#' @param threshold A twilight threshold (default = 1.5).
#' @param dips The allowed number of interruptions during a daylight profile
#'  below the twilight threshold before flagging as a poor quality "suspect"
#'  day.
#' @param step A threshold of the allowed step change in illuminance values
#'  between the twilight value and the preceding one. Large jumps and the lack
#'  of a smooth transition suggest a false twilight (bird leaving a dark nest
#'  site long after or long before dawn or dusk).
#' @param filter Logical if to return data pre-filtered, removing all poor quality days
#'  or false twilight ones (default = TRUE)
#'
#' @returns A skytrackr data frame with poor twilight quality days removed and
#'  dusk and dawn timings marked (data is returned as a long format, not a wide
#'  format).
#' @export
#' @examples
#'
#' # set demo values artificially low as a demonstration
#' library(dplyr)
#' df <- cc876 |>
#'   mutate(
#'   value = ifelse(
#'     date_time > "2021-08-15 05:00:00" & date_time < "2021-08-15 12:00:00",
#'     0.1,
#'     value)
#'   )
#'
#' # screen values and remove them (filter = TRUE)
#' df <- df |> stk_screen_twl(filter = TRUE)

stk_screen_twl <- function(
    df,
    threshold = 1.5,
    dips = 3,
    step = 100,
    filter = TRUE
){

  # no summarize feedback
  options(dplyr.summarise.inform = FALSE)

  # for all dates calculate twilight values based upon
  # a threshold, commonly 1.5
  twl <- df |>
    dplyr::filter(
      .data$measurement == "lux"
    ) |>
    dplyr::group_by(.data$logger, .data$date) |>
    dplyr::summarize(
      dawn = min(.data$hour[.data$value >= threshold], na.rm = TRUE),
      dawn = ifelse(is.infinite(.data$dawn), min(.data$hour), .data$dawn),
      dusk = max(.data$hour[.data$value >= threshold], na.rm = TRUE),
      dusk = ifelse(is.infinite(.data$dusk), max(.data$hour), .data$dusk),
      roost = length(
        which(.data$value[.data$hour > .data$dawn & .data$hour < .data$dusk] < threshold)
        ) > dips
    )

  df_out <- dplyr::left_join(
    df,
    twl,
    by = c("logger","date")
  )

  # detect false twilight by looking at how much the dawn and dusk values
  # differ from the proceeding ones towards or from daylight. Twilight is
  # logarithmic in developent so missed steps increase light levels significantly
  # exiting dark nests late shows up as an abrupt step in light levels
  false_twl <- df_out |>
    dplyr::group_by(.data$logger, .data$date) |>
    dplyr::summarise(
      dawn_start = which(.data$hour == .data$dawn)[1],
      dusk_start = which(.data$hour == .data$dusk)[1],
      dawn_diff = abs(diff(.data$value[.data$dawn_start:(.data$dawn_start - 1)]))[1],
      dusk_diff = abs(diff(.data$value[.data$dusk_start:(.data$dusk_start + 1)]))[1],
      false_twl = ifelse(.data$dawn_diff > step | .data$dusk_diff > step, TRUE, FALSE)
    ) |>
    dplyr::select(
      -dplyr::starts_with(c("dawn", "dusk"))
    )

  df_out <- dplyr::left_join(
    df_out,
    false_twl,
    by = c("logger","date")
  )

  if(filter){
    df_out <- df_out |>
      dplyr::filter(
        !.data$roost,
        !.data$false_twl
      )
  }

  return(df_out |> dplyr::ungroup())
}
