
#' Estimate scale the scale range
#'
#' Provides a rough estimate on the light loss and corresponding
#' range of scale values to consider during optimization. The full
#' range is reported, but a percentile parameter can be provided
#' to remove outliers. It is recommended to first use `st_screen_twl()`
#' to remove "odd" days.
#'
#' @param df a skytrackr dataframe
#' @param percentile percentile of the spread of data to use in scale value
#'  evaluation
#' @param floor threshold to remove low (nighttime) values
#' @param verbose Give detailed feedback (TRUE or FALSE, default = TRUE)
#'
#' @importFrom utils packageVersion
#'
#' @returns an estimated scale value to be used in optimization
#' @export
#' @examples
#'
#' # Estimate the upper scale value for fitting routine
#' upper_scale_value <- cc876 |> stk_calibrate()

stk_calibrate <- function(
    df,
    percentile = 100,
    floor = 1.5,
    verbose = TRUE
){

  # verbose feedback
  if(verbose) {

    cli::cli_div(
      theme = list(
        rule = list(
          color = "darkgrey",
          "line-type" = "double",
          "margin-bottom" = 1,
          "margin-top" = 1
        ),
        span.strong = list(color = "black"))
    )
    cli::cli_rule(
      left = "{.strong Estimating suggested scale value}",
      right = "{.pkg skytrackr v{packageVersion('skytrackr')}}",

    )
    cli::cli_end()
  }

  # pre-process the data, only retaining
  # a subset of "daytime" data
  df <- df |>
    stk_filter(
      range = c(floor, 150000),
      smooth = FALSE,
      filter = TRUE
    )

  # calculate daily maxima
  df_max <- df |>
    dplyr::filter(.data$measurement == "lux") |>
    dplyr::group_by(.data$logger,.data$date) |>
    dplyr::summarize(
      max_illuminance = stats::quantile(.data$value, 0.9, na.rm = TRUE)
    )

  i <- 1
  k <- 0

  while(i > 0){
    k <- k + 1

    # generate a reference
    reference <- skylight::skylight(
      date = as.POSIXct("2022-09-23 12:00:00"),
      latitude = 0,
      longitude = 0,
      sky_condition = k
    )$sun_illuminance

    # calculate the observed attenuation
    obs_att <- 100 - (df_max$max_illuminance/reference) * 100
    i <- stats::quantile(obs_att, percentile/100)
  }

  # set ranges
  upper <- k
  lower <- ifelse(k <= 20, 0.00001, 1)

  # feedback
  if(verbose) {
    cli::cli_bullets(c(
      ">" = "The suggested upper scale parameter is {.strong {upper}}!",
      ">" = "The suggested lower scale parameter is {.strong {lower}}!",
      "i" = "Note, these estimates are approximations!"
      )
    )
  }

  # return estimated values invisible
  invisible(c(lower, upper))
}
