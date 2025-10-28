
#' Estimate upper scale value
#'
#' Estimates the upper scale value based upon
#' midday values of the data itself. The method
#' requires midday values and will not work on
#' loggers with a capped value (only recording twilight
#' values)
#'
#' @param df a skytrackr dataframe
#' @param percentile percentile of the spread of data to use in scale value
#'  evaluation
#' @param floor threshold to remove low (nighttime) values
#' @param sky_condition reference sky conditions, by default set
#'  to three (3), an average sky with ~30% cloud cover. One (1)
#'  is a clear sky, while a value of ten (10) is an extremely dark
#'  sky under stormy / rainy conditions (uncommon).
#' @param verbose Give feedback including a progress bar (TRUE or FALSE,
#'  default = TRUE)
#' @param plot Plot statistical information on the suggested scale estimate
#'
#' @returns an estimated scale value to be used in optimization
#' @export

stk_calibrate <- function(
    df,
    percentile = 95,
    floor = 1.5,
    sky_condition = 3,
    plot = TRUE,
    verbose = TRUE
){

  # create progress bar
  if(verbose) {

    cli::cli_div(
      theme = list(
        rule = list(
          color = "darkgrey",
          "line-type" = "double",
          "margin-bottom" = 1
        ),
        span.strong = list(color = "black"))
    )
    cli::cli_rule(
      left = "{.strong Estimating suggested scale value}",
      right = "{.pkg skytrackr v{packageVersion('skytrackr')}}",

    )
    cli::cli_end()
  }

  # pre-process the data
  df <- df |>
    stk_filter(range = c(floor, 150000), filter = TRUE)

  # calculate daily maxima
  df_max <- df |>
    dplyr::filter(.data$measurement == "lux") |>
    dplyr::group_by(.data$logger,.data$date) |>
    dplyr::summarize(
      max_illuminance = max(value, na.rm = TRUE)
    )

  # generate a reference
  reference <- skylight::skylight(
    date = as.POSIXct("2022-09-23 12:00:00"),
    latitude = 0,
    longitude = 0,
    sky_condition = 1
  )$sun_illuminance

  # calculate the observed attenuation
  obs_att <- (df_max$max_illuminance/reference) * 100

  # calculate the attenuation Beer-Lambert for LAI
  lai_range <- seq(0, 10, by = 0.1)
  lai_att <- exp(-0.5 * lai_range) * 100

  # set the range of scale factors
  scale_factor <- seq(1, 2000, by = 1)

  # calculate "sky condition" based attentuation values
  sky_att <- lapply(scale_factor, function(i){
    ideal <- skylight::skylight(
      date = as.POSIXct("2022-01-01 12:00:00"),
      latitude = 0,
      longitude = 0,
      sky_condition = sky_condition
    )$sun_illuminance

    c <- skylight::skylight(
      date = as.POSIXct("2022-01-01 12:00:00"),
      latitude = 0,
      longitude = 0,
      sky_condition = i
    )$sun_illuminance / ideal

    c <- c*100
    c <- ifelse(c>100, 100, c)
  }) |> unlist()

  # map scale factors to LAI values
  lai_scale_factor <- lapply(lai_att, function(a){
    scale_factor[which.min(abs(sky_att - a))]
  }) |> unlist()

  obs_scale_factor <- lapply(obs_att, function(a){
    scale_factor[which.min(abs(sky_att - a))]
  }) |> unlist()

  # calculate quantile
  qtl <- round(quantile(obs_scale_factor, (percentile/100)), 4)
  qtl <- ifelse(qtl < 10, 10, qtl)

  if (plot){
    par(mfrow=c(2,1))
    hist(
      obs_scale_factor,
      breaks = 20,
      main = "",
      xlab = "Observed scale"
    )

    plot(
      lai_range,
      lai_scale_factor,
      type = "l",
      xlab = "LAI",
      ylab = "scale factor"
    )
    abline(h = qtl)
  }

  # cleanup of progress bar
  if(verbose) {

    cli::cli_alert(c(
      "Given ambient sky conditions of {.strong {sky_condition}}!",
      "x" = "
           [with 1 being clear sky, 3 being an average sky, and 10 being a very grey rainy sky]
         "
    ))
    cli::cli_alert_info(
      "the suggested upper scale parameter is: {.strong {qtl}}!")
  }

  invisible(return(as.numeric(qtl)))
}
