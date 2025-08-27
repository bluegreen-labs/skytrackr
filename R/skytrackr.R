
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
#' @param range range of values to consider during processing, should be
#'  provided in lux c(min, max) or the equivalent if non-calibrated
#' @param scale scale / sky condition factor, by default covering the
#'  skylight() range of 1-10 but can be extended for more flexibility
#'  in case of non lux measurements
#' @param control control settings for the Bayesian optimization, generally
#'  should not be altered (defaults to a sequential monte carlo method)
#' @param mask mask with priors to constrain positions
#' @param plot plot map of incrementally changing determined locations as
#'  a progress method
#' @param verbose given feedback including a progress bar
#'
#' @importFrom rlang .data
#' @import patchwork
#'
#' @return data frame with location estimate, their uncertainties, and
#' ancillary model parameters useful in quality control
#' @export

skytrackr <- function(
    data,
    start_location,
    tolerance = 1,
    range = c(0.32, 150),
    scale = c(1, 10),
    control = list(
      sampler = 'SMC',
      settings = list(
        initialParticles = 100,
        iterations = 20,
        message = FALSE
      )
    ),
    mask,
    plot = TRUE,
    verbose = TRUE
) {

  if(missing(mask)){
    stop("please provide a base mask or grid of valid sample locations!")
  }

  if(missing(start_location)) {
    stop("
        - No (approximate) start location provided, please provide a start location!
        ")
  }

  # unravel the data
  data <- data |>
    dplyr::filter(
      .data$measurement == "lux"
    ) |>
    tidyr::pivot_wider(
      names_from = "measurement",
      values_from = "value"
    )

  # subset data
  data <- data |>
    dplyr::filter(
      (.data$lux > range[1] & .data$lux < range[2])
    ) |>
    dplyr::mutate(
      lux = log(.data$lux)
    )

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

  # loop over all available dates
  for (i in seq_len(length(dates))) {
    if (i != 1) {
          # create data point
          loc <-  sf::st_as_sf(
              data.frame(
                lon = locations$longitude[i-1],
                lat = locations$latitude[i-1]
              ),
              coords = c("lon","lat")
            ) |> st_set_crs(4326)
    } else {
        # create data point
        loc <- sf::st_as_sf(
            data.frame(
              lon = start_location[2],
              lat = start_location[1]
            ),
            coords = c("lon","lat")
          ) |> st_set_crs(4326)
    }

    # set tolerance units to km
    units(tolerance) <- "km"

    # buffer the location in equal area
    # projection, back convert to lat lon
    pol <- loc |>
      st_transform(crs = "+proj=laea") |>
      sf::st_buffer(tolerance) |>
      st_transform(crs = "epsg:4326")

    roi <- mask(mask, pol) |>
      crop(st_bbox(pol))

    # create a subset
    subs <- data[which(data$date == dates[i]),]

    # fit model parameters for a given
    # day to estimate the location
    out <- stk_fit(
        data = subs,
        roi = roi,
        loc = sf::st_coordinates(loc),
        scale = scale,
        control = control
      )

    # set date
    out$date <- dates[i]

    # equinox flag
    doy <- as.numeric(format(out$date, "%j"))
    out$equinox <- ifelse(
      (doy > 266 - 10 & doy < 266 + 10) |
        (doy > 80 - 10 & doy < 80 + 10),
      TRUE, FALSE)

    # append output to data frame
    locations <- rbind(locations, out)

    # increment on progress bar
    if(verbose) {
      pb$tick()
    }

    if(plot & nrow(locations) > 1){

      p <- stk_map(
        locations,
        buffer = buffer,
        bbox = st_bbox(mask),
        start_location = start_location,
        roi = pol # forward roi polygon / not raster
      )

      plot(p)
    }
  }

  # return the data frame with
  # location
  return(locations)
}
