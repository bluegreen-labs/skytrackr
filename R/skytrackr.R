
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
#' @param land_mask use a land mask to constrain positions to land, buffered
#'  by a value set by buffer
#' @param buffer buffer to pad the land mask with, in degrees, default set
#'  to 1 or ~110km
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
    particles = 100,
    range = c(0.32, 150),
    bbox = c(-180, -90, 180, 90),
    scale = c(1, 10),
    control = list(
      sampler = 'SMC',
      settings = list(
        initialParticles = particles,
        iterations = iterations,
        message = FALSE
      )
    ),
    land_mask = FALSE,
    buffer = 1,
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

  if(plot){
    maps::map(
      xlim = bbox[c(1,3)],
      ylim = bbox[c(2,4)]
    )
  }

  # create mask if required
  if(land_mask){
    mask <- stk_mask(buffer = buffer)
  }

  # loop over all available dates
  for (i in seq_len(length(dates))) {
    if (i != 1) {
        if(!missing(start_location)){

          # create data point
          roi <-  sf::st_as_sf(
            data.frame(
              lon = locations$longitude[i-1],
              lat = locations$latitude[i-1]
              ),
            coords = c("lon","lat"),
            crs = "epsg:4326"
          ) |>
            sf::st_buffer(tolerance)

          if(land_mask){
            roi <- sf::st_intersection(
              roi,
              mask
            )
          }

          bbox <- roi |>
            sf::st_bbox()
        }
    } else {
      if(!missing(start_location)) {

        # create data point
        roi <- sf::st_as_sf(
          data.frame(
            lon = start_location[2],
            lat = start_location[1]
          ),
          coords = c("lon","lat"),
          crs = "epsg:4326"
        ) |>
          sf::st_buffer(tolerance)

        if(land_mask){
          roi <- sf::st_intersection(
            roi,
            mask
          )
        }

        bbox <- roi |>
          sf::st_bbox()

      } else {
message(
"  - No start location provided,
     using default bounding box throughout search
     or the first acquired fix!
")
      }
    }

    # create a subset
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
      if(land_mask){
        plot(
          mask,
          border = "grey",
          add = TRUE
        )
      }

      plot(
        roi,
        border = "grey",
        add = TRUE
      )

      graphics::lines(
        locations[,5:4],
        col = 'grey'
      )
      graphics::points(
        locations[,5:4],
        pch = 19,
        col = 'red'
      )
    }
  }

  # return the data frame with
  # location
  return(locations)
}
