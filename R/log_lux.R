#' Simulate illuminance as log(lux) values
#'
#' Calculates log(lux) values for a give location, date, time
#' and sky conditions.
#'
#' @param par three parameters specifying the cycling model
#' @param data data consisting of vegetation greenness (G), mean
#'  daytime temperature (T) and daylenght (D) as a data frame
#' @param ... optional other parameters to forward
#'
#' @return sky illuminance as log(lux)
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
