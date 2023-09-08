
#' Sky (illuminance) tracker
#'
#' Skytrack compares geolocator based light measurements in lux with
#' those modelled by the sky illuminance model of xyz.
#'
#' Model fits are applied by default to values up to sunrise or after
#' sunset only as most critical to the model fit (capturing daylength,
#' i.e. latitude and the location of the diurnal pattern -
#' longitudinal displacement).
#'
#' @param data a data frame containing date time and lux values
#' @param iterations number of optimization iterations
#' @param bbox bounding box of the location search domain given as
#'  c(xmin, ymin, xmax, ymax)
#' @param floor lower bound of lux values to consider (values below this
#'  value are ignored)
#' @param ceiling upper bound of lux values to consider (values above this
#'  value are ignored)
#' @param plot plot map of incrementally changing determined locations as
#'  a progress method
#' @param offset by default the diurnal cycle around noon is considered (from
#'  sunrise to sunset), alternatively estimates can be made around  midnight
#'  considering the profile from sunset to sunrise.
#' @param verbose given feedback including a progress bar
#'
#' @return data frame with location estimate, their uncertainties, and
#' ancillary model parameters useful in quality control
#' @export

skytrackr <- function(
    data,
    iterations = 10000,
    floor = 0.32,
    ceiling = 400,
    bbox = c(-180, -90, 180, 90),
    offset = "day",
    plot = FALSE,
    verbose = TRUE
) {

  # preprocess data
  data <- data |>
    dplyr::filter(
      lux > floor,
      lux < ceiling
    ) |>
    mutate(
      lux = log(lux)
    )

  # unique dates
  dates <- unique(data$date)

  # empty data frame
  locations <- data.frame()

  # plot progress
  if(plot){
    maps::map(
      xlim = bbox[c(1,3)],
      ylim = bbox[c(2,4)]
    )
  }

  # create progress bar
  if(verbose) {
  message(
    sprintf(
      "- Estimating locations from light (lux) profiles for logger: %s!",
        data$logger[1])
  )
  pb <- progress::progress_bar$new(
    format = "  processing [:bar] :percent eta: :eta",
    total = length(dates),
    clear = FALSE,
    width= 60
    )
  pb$tick(0)
  }

  # loop over all available dates
  for (i in seq_len(length(dates))) {

    # create daily subset
    subs <- data |>
      dplyr::filter(
        date == dates[i]
      )

    # fit model parameters for a given
    # day to estimate the location
    out <- stk_fit(
      data = subs,
      iterations = iterations,
      bbox = bbox
    )

    # set date
    out$date <- dates[i]

    # append output to data frame
    locations <- rbind(locations, out)

    # increment on progress bar
    if(verbose) {
      pb$tick()
    }

    if(plot){
      points(
        locations[,1:2],
        pch = 19,
        col = 'red'
      )
    }
  }

  # return the data frame with
  # location
  return(locations)
}
