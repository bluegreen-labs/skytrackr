geodesic_steps <- function(.data, start, end, sky = 1){

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
    bearing <- geosphere::bearing(
      start,
      end,
      f = 0
    )

    # create data frame with destination lat/lon values
    tmp <- as.data.frame(
      geosphere::destPoint(
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
