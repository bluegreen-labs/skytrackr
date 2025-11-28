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

#' Updates values individually not globally (diurnally)
#'
#' Calculates log(lux) values for a give location, date, time
#' and sky conditions along the track. Updates to the bearing
#' direction of flight are only made for selected values.
#'
#' After a diurnal cycle the course is updated for the next,
#' diurnal cycle. Processing uses a GMT time reference and
#' large east-west movements might cut through the date line,
#' hence updates along the flight track would not perfectly
#' coincide with the end of a twilight phase.
#'
#' @param par Three parameters specifying the illuminance model.
#' @param data A data frame with the required drivers for the illuminance model.
#' @param loc previous location
#' @param ... optional other parameters to forward
#'
#' @return Sky illuminance as log(lux).
#' @export

individual <- function(
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
  out <- .flight_steps(
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


# Blind function to calculate flight steps for the individual model
.flight_steps <- function(.data, start, end, sky){

  # calculate the distance to the end-point
  dist <- geosphere::distGeo(
    start,
    end,
    f = 0
  )

  # calculate the average speed required for this
  # given the total time and distance
  speed <- dist/sum(.data$time_step)

  # output data frame
  out <- data.frame()

  # for all values calculate the
  # intermediate steps
  for(i in 1:nrow(.data)){

    # f = 0 for great circle route
    bearing <- geosphere::bearingRhumb(
      start,
      end
    )

    # create data frame with destination lat/lon values
    tmp <- as.data.frame(
      geosphere::destPointRhumb(
        start,
        b = bearing,
        d = .data$time_step[i] * speed
      )
    )

    # update start position
    start <- tmp

    # add date/bearing field
    tmp$date <- .data$date_time[i]
    tmp$bearing <- bearing

    # bind output
    out <- dplyr::bind_rows(out, tmp)
  }

  out <- out |>
    dplyr::rename(
      'latitude' = 'lat',
      'longitude' = 'lon'
    )

  # set sky
  out$sky_condition <- sky

  return(out)
}


