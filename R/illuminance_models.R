#' Simulate diurnal illuminance value
#'
#' Calculates log(lux) values for a give location, date, time
#' and sky conditions.
#'
#' @param par Three parameters specifying the illuminance model.
#' @param data A data frame with the required drivers for the illuminance model.
#' @param loc previous location
#' @param ... optional other parameters to forward
#'
#' @return Sky illuminance as log(lux).
#' @export

diurnal <- function(
    par,
    data,
    loc,
    ...
) {

  # split out model parameters
  # these are the proposed values
  lat <- par[1]
  lon <- par[2]
  sky <- par[3]

  # run model (no geodesic path)
  illuminance <- skylight::skylight(
    longitude = lon,
    latitude = lat,
    date = data$date_time,
    sky_condition = sky
  )$total_illuminance

  # return log lux
  return(log(illuminance))
}

#' Simulate geodesic corrected illuminance value
#'
#' Calculates log(lux) values for a give location, date, time
#' and sky conditions along a geodesic.
#'
#' @param par Three parameters specifying the illuminance model.
#' @param data A data frame with the required drivers for the illuminance model.
#' @param loc previous location
#' @param ... optional other parameters to forward
#'
#' @return Sky illuminance as log(lux).
#' @export

geodesic <- function(
    par,
    data,
    loc,
    ...
) {

  # split out model parameters
  # these are the proposed values
  lat <- par[1]
  lon <- par[2]
  sky <- par[3]

  # calculate intermediate steps
  # along geodesic path between
  # the start location and the
  # current proposition (from parameters)
  out <- geodesic_steps(
    data,
    start = loc,
    end = c(lon, lat),
    sky = sky
  )

  # run model
  illuminance <- skylight::skylight(
    out
  )$total_illuminance

  # return log lux
  return(log(illuminance))
}

