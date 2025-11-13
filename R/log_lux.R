#' Simulate illuminance value
#'
#' Calculates log(lux) values for a give location, date, time
#' and sky conditions.
#'
#' @param par Three parameters specifying the illuminance model.
#' @param data A data frame with the required drivers for the illuminance model.
#' @param ... optional other parameters to forward
#'
#' @return Sky illuminance as log(lux).
#' @export

log_lux <- function(
    par,
    data,
    loc,
    ...
) {

  # split out model parameters
  lat <- par[1]
  lon <- par[2]
  sky <- exp(par[3])
  speed <- par[4]

  # calculate distances along reference rhumb line
  # given a starting position (loc), a suggested
  # end position and a speed
  start <- loc

  out <- data.frame(
    lat = lat,
    lon = lon,
    date = data$date_time[1]
  )

  for(i in 2:nrow(data)){
    bearing <- geosphere::bearing(start, c(lon, lat))
    tmp <- as.data.frame(
      geosphere::destPoint(
        start,
        b = bearing,
        d = data$time_step[i] * speed
        )
    )

    # update start position
    start <- tmp

    # add date field
    tmp$date <- data$date_time[i]

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

  # run model
  illuminance <- skylight::skylight(
    out
  )$total_illuminance

  # return log lux
  return(log(illuminance))
}
