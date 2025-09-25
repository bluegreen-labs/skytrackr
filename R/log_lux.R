#' Simulate illuminance value
#'
#' Calculates log(lux) values for a give location, date, time
#' and sky conditions.
#'
#' @param par Three parameters specifying the illuminance model.
#' @param data A data frame with the required drivers for the illuminance model.
#' @param ... optional other parameters to forward
#'
#' @return Sky illuminance as log(lux)
#' @export

log_lux <- function(
    par,
    data,
    ...
) {

  # split out model parameters
  lat <- par[1]
  lon <- par[2]
  sky <- exp(par[3])

  # run model
  illuminance <- skylight::skylight(
    longitude = lon,
    latitude = lat,
    date = data$date_time,
    sky_condition = sky
  )$total_illuminance

  # return log lux
  return(log(illuminance))

}
