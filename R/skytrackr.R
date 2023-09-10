
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
#' @param start_location start location of logging, required when using a
#'  tolerance based estimation. When no start_location is used the default
#'  bounding box (bbox) settings are used.
#' @param tolerance tolerance on the search window for optimization, given in
#'  degrees (to pad left - right, top - bottom). Uses the previous steps
#'  location to constrain the parameter (location) search space.
#'  This requires the start_location parameter to be set, as you need a
#'  start position for a trusted initial location.
#' @param iterations number of optimization iterations
#' @param particles number of particles when using the default particle filter
#'  optimization
#' @param bbox bounding box of the location search domain given as
#'  c(xmin, ymin, xmax, ymax)
#' @param range range of values to consider during processing, should be
#'  provided in lux c(min, max) or the equivalent if non-calibrated
#' @param scale scale / sky condition factor, by default covering the
#'  skylight() range of 1-10 but can be extended for more flexibility
#'  in case of non lux measurements
#' @param control control settings for the Bayesian optimization, generally
#'  should not be altered (defaults to a sequential monte carlo method)
#' @param plot plot map of incrementally changing determined locations as
#'  a progress method
#' @param verbose given feedback including a progress bar

#'
#' @return data frame with location estimate, their uncertainties, and
#' ancillary model parameters useful in quality control
#' @export

skytrackr <- function(
    data,
    start_location,
    tolerance = 15,
    iterations = 20,
    particles = 200,
    range = c(0.32, 400),
    bbox = c(-180, -90, 180, 90),
    scale = c(0, 20),
    control = list(
      sampler = 'SMC',
      settings = list(
        initialParticles = 100,
        iterations= 10,
        message = FALSE
      )
    ),
    plot = FALSE,
    verbose = TRUE
) {

  # subset data
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
    if (i != 1) {
        bbox <- c(locations$longitude[i-1] - tolerance,
                  locations$latitude[i-1] - tolerance,
                  locations$longitude[i-1] + tolerance,
                  locations$latitude[i-1] + tolerance
        )
    } else {
      if(!missing(start_location)) {
        bbox <-c(start_location[2] - tolerance,
                 start_location[1] - tolerance,
                 start_location[2] + tolerance,
                 start_location[1] + tolerance
        )
      } else {
message(
"  - No start location provided,
     using default bounding box throughout search
     or the first acquired fix!
")
      }
    }

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
      graphics::lines(
        locations[,1:2],
        col = 'grey'
      )
      graphics::points(
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
