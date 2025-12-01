
#' Estimate scale the scale range
#'
#' Provides a rough estimate on the light loss and corresponding
#' range of scale values to consider during optimization.
#' A percentile parameter can be provided to remove outliers. It is
#' recommended to first use `st_screen_twl()` to remove poor quality days.
#'
#' @param df a skytrackr dataframe
#' @param percentile percentile of the diurnal data (abvoe the floor value)
#'  used in calculating daily statistics (default = 100)
#' @param floor threshold to remove low (nighttime) values
#' @param distribution provide the full distribution across the track of scale
#'  factors (default = FALSE, providing only a range as a global constraint)
#' @param verbose Give detailed feedback (TRUE or FALSE, default = TRUE)
#'
#' @importFrom utils packageVersion
#'
#' @returns An estimated range of scale values to be used in optimization.
#'  Values are not log transformed.
#' @export
#' @examples
#'
#' # Estimate the upper scale value for fitting routine
#' upper_scale_value <- cc876 |> stk_calibrate()

stk_calibrate <- function(
    df,
    percentile = 100,
    floor = 1.5,
    distribution = FALSE,
    verbose = TRUE
){

  # verbose feedback
  if(verbose) {

    cli::cli_div(
      theme = list(
        rule = list(
          color = "darkgrey",
          "line-type" = "single",
          "margin-bottom" = 1,
          "margin-top" = 1
        ),
        span.strong = list(color = "black"))
    )
    cli::cli_rule(
      left = "{.strong Estimating suggested scale values}",
      #right = "{.pkg skytrackr v{packageVersion('skytrackr')}}",

    )
    cli::cli_end()
  }

  # pre-process the data, only retaining
  # a subset of "daytime" data
  df <- df |>
    stk_filter(
      range = c(floor, Inf),
      smooth = FALSE,
      filter = TRUE,
      verbose = FALSE
    )

  # calculate daily maxima
  df_max <- df |>
    dplyr::filter(.data$measurement == "lux") |>
    dplyr::group_by(.data$logger,.data$date) |>
    dplyr::summarize(
      max_illuminance = stats::quantile(log(.data$value), percentile/100, na.rm = TRUE),
      .groups = "drop"
    )

  df_scales <- df_max |>
    dplyr::group_by(.data$logger,.data$date) |>
    dplyr::do({

      i <- 1
      k <- 0

      # sets reference to a sky condition k to look at the distribution
      # of scaling factors
      while(any(i > 0)){
        k <- k + 1

        # generate a reference (should pre-calculate this!)
        reference <- skylight::skylight(
          date = as.POSIXct("2022-09-23 12:00:00", tz = "GMT")  + seq(0, 24*3600, 300),
          latitude = 0,
          longitude = 0,
          sky_condition = k
        )$sun_illuminance

        # reference
        reference <- reference[reference > floor] # log convert!
        reference <- stats::quantile(log(reference), percentile/100, na.rm = TRUE)

        # calculate the observed attenuation
        i <- 1 - (.data$max_illuminance/reference)
      }

      data.frame(scale = k)
    }) |>
    dplyr::ungroup()

  # return estimated values invisible
  if(distribution){

    # feedback
    if(verbose) {
      cli::cli_bullets(c(
        ">" = "Calculating the full distribution of scale parameters!"
        )
      )
    }

    return(df_scales)
  } else {

    # set ranges
    lower <- 0.9
    upper <- as.numeric(
      stats::quantile(
        df_scales$scale, 1
      )
    )

    if(upper < 10){
      if(verbose) {
        cli::cli_bullets(c(
          "!" = "Upper bound is lower than the light model's normal maximum range (10).",
          "i" = "Suggesting 10 instead of the calculated {upper}."
        )
        )
      }

      # set upper to 10 (default max of skylight)
      # if < 10
      upper <- 10

    }

    # feedback
    if(verbose) {
      cli::cli_bullets(c(
        ">" = "The suggested scale range is {.strong c({lower},{upper})}!",
        "i" = "Note, these estimates are approximations! Always inspect your
      data (e.g. by using stk_filter()) and consider using the stk_screen_twl()
      to remove days with poor twilight and other characteristics. When using the
      'individual' light model you might have to relax the upper scaling value."
      )
      )
    }

    return(c(lower, upper))
  }
}
