
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
#' @param range range of values to consider during processing, should be
#'  provided in lux c(min, max) or the equivalent if non-calibrated
#' @param plot plot map of incrementally changing determined locations as
#'  a progress method
#' @param offset by default the diurnal cycle around noon is considered (from
#'  sunrise to sunset), alternatively estimates can be made around  midnight
#'  considering the profile from sunset to sunrise.
#' @param verbose given feedback including a progress bar
#' @param control control settings for the Bayesian optimization, generally
#'  should not be altered
#'
#' @return data frame with location estimate, their uncertainties, and
#' ancillary model parameters useful in quality control
#' @export

skytrackr <- function(
    data,
    iterations = 20000,
    range = c(0.32, 400),
    bbox = c(-180, -90, 180, 90),
    scale = c(1, 10),
    offset = "day",
    control = list(
      sampler = 'DEzs',
      settings = list(
        burnin = iterations * 0.2,
        iterations = iterations * 0.8,
        message = FALSE
      )
    ),
    plot = FALSE,
    verbose = TRUE
) {

  # preprocess data
  # data <- data |>
  #   dplyr::filter(
  #     lux > range[1],
  #     lux < range[2]
  #   ) |>
  #   mutate(
  #     lux = log(lux)
  #   )
  data <- data[which(data$lux > range[1] & data$lux < range[2]),]
  data$lux <- log(data$lux)

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
    # subs <- data |>
    #   dplyr::filter(
    #     date == dates[i]
    #   )
    subs <- data[which(data$date == dates[i]),]

    # fit model parameters for a given
    # day to estimate the location
    out <- stk_fit(
      data = subs,
      iterations = iterations,
      bbox = bbox,
      scale = scale,
      control = control
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
