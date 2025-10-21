#' Sky (illuminance) location estimation routine
#'
#' Skytrack compares geolocator based light measurements in lux with
#' those modelled by the sky illuminance model of Janiczek and DeYoung (1987).
#'
#' Model fits are applied by default to values up to sunrise or after
#' sunset only as most critical to the model fit (capturing daylength,
#' i.e. latitude and the location of the diurnal pattern -
#' longitudinal displacement).
#'
#' @param data A skytrackr data frame.
#' @param start_location A start location of logging as a vector of
#'  latitude and longitude
#' @param tolerance Tolerance distance on the search window for optimization,
#'  given in km (left/right, top/bottom). Sets a hard limit on the search window
#'  regardless of the step selection function used.
#' @param range Range of values to consider during processing, should be
#'  provided in lux c(min, max) or the equivalent if non-calibrated.
#' @param scale Scale / sky condition factor, by default covering the
#'  skylight() range of 1-10 (from clear sky to extensive cloud coverage)
#'  but can be extended for more flexibility to account for coverage by plumage,
#'  note that in case of non-physical accurate lux measurements values can have
#'  a range starting at 0.0001 (a multiplier instead of a divider). Values need
#'  to be provided on a log scale (default = log(c(0.00001, 50)))
#' @param control Control settings for the Bayesian optimization, generally
#'  should not be altered (defaults to a Monte Carlo method). For detailed
#'  information I refer to the BayesianTools package documentation.
#' @param mask Mask to constrain positions to land
#' @param window_size use a moving window across x days during data processing,
#'  this effectively smooths responses (default = 1, day-by-day processing)
#' @param step_selection A step selection function on the distance of a proposed
#'  move, step selection is specified on distance (in km) basis.
#' @param plot Plot a map during location estimation (updated every seven days)
#' @param verbose Give feedback including a progress bar (TRUE or FALSE,
#'  default = TRUE)
#'
#' @importFrom rlang .data
#' @import patchwork
#'
#' @return A data frame with location estimate, their uncertainties, and
#'  ancillary model parameters useful in quality control.
#' @export
#' @examples
#' \donttest{
#'
#' # define land mask with a bounding box
#' # and an off-shore buffer (in km), in addition
#' # you can specify the resolution of the resulting raster
#' mask <- stk_mask(
#'   bbox  =  c(-20, -40, 60, 60), #xmin, ymin, xmax, ymax
#'   buffer = 150, # in km
#'   resolution = 0.5 # map grid in degrees
#'   )
#'
#'   # define a step selection distribution/function
#'   ssf <- function(x, shape = 0.9, scale = 100, tolerance = 1500){
#'   norm <- sum(stats::dgamma(1:tolerance, shape = shape, scale = scale))
#'   prob <- stats::dgamma(x, shape = shape, scale = scale) / norm
#'   }
#'
#' # estimate locations
#' locations <- cc876 |> skytrackr(
#'   plot = TRUE,
#'   mask = mask,
#'   step_selection = ssf,
#'   start_location = c(50, 4),
#'       control = list(
#'         sampler = 'DEzs',
#'         settings = list(
#'         iterations = 10, # change iterations
#'          message = FALSE
#'         )
#'       )
#'   )
#' }

skytrackr <- function(
    data,
    start_location,
    tolerance = 1500,
    range = c(0.09, 148),
    scale = log(c(0.00001, 50)),
    control = list(
      sampler = 'DEzs',
      settings = list(
        burnin = 1000,
        iterations = 3000,
        message = FALSE
      )
    ),
    window_size = 1,
    mask,
    step_selection,
    plot = TRUE,
    verbose = TRUE
) {

  if(missing(mask)){
    cli::cli_abort(c(
      "No grid mask is provided.",
      "x" = "Please provide a base mask or grid of valid sample locations!"
      )
    )
  }

  if(missing(start_location)) {
    cli::cli_abort(c(
          "No (approximate) start location provided.",
          "x" = "Please provide a start location!"
        )
      )
  }

  if(window_size %% 2 == 0) {
    cli::cli_abort(c(
      "The chosen window size is even.",
      "x" = "Please provide an uneven window size"
    )
    )
  }

  # unravel the light data
  data <- data |>
    skytrackr::stk_filter(range = range, filter = TRUE) |>
    tidyr::pivot_wider(
      names_from = "measurement",
      values_from = "value"
    )

  # convert to log lux
  data <- data |>
    dplyr::mutate(
      lux = log(.data$lux)
    )

  # unique dates
  dates <- unique(data$date)

  # empty data frame
  locations <- data.frame()

  # create progress bar
  if(verbose) {

    cli::cli_div(
      theme = list(
        rule = list(
          color = "darkgrey",
          "line-type" = "double",
          "margin-bottom" = 1
          ),
        span.strong = list(color = "black"))
        )
    cli::cli_rule(
      left = "{.strong Estimating locations}",
      right = "{.pkg skytrackr v{packageVersion('skytrackr')}}",

      )
    cli::cli_end()
    cli::cli_alert_info(
        "Processing logger: {.strong {data$logger[1]}}!")

    if(plot){
    cli::cli_alert_info(
        "(preview plot will update every 7 days)"
      )
    }

    cli::cli_progress_bar(
      " - Estimating positions",
      total = length(dates)
    )
  }

  # plot updates every 5 days (if possible)
  if(length(dates) >= 7){
    plot_update <- seq(2, length(dates), by = 7)
  } else {
    plot_update  <- length(dates)
  }

  # loop over all available dates
  for (i in seq_along(dates)) {
    if (i != 1) {
          # create data point
          loc <-  sf::st_as_sf(
              data.frame(
                lon = locations$longitude[i-1],
                lat = locations$latitude[i-1]
              ),
              coords = c("lon","lat")
            ) |> sf::st_set_crs(4326)
    } else {
        # create data point
        loc <- sf::st_as_sf(
            data.frame(
              lon = start_location[2],
              lat = start_location[1]
            ),
            coords = c("lon","lat")
          ) |> sf::st_set_crs(4326)
    }

    # set tolerance units to km
    units(tolerance) <- "km"

    # buffer the location in equal area
    # projection, back convert to lat lon
    pol <- loc |>
      sf::st_transform(crs = "+proj=laea") |>
      sf::st_buffer(tolerance) |>
      sf::st_transform(crs = "epsg:4326")

    roi <- terra::mask(mask, pol) |>
      terra::crop(sf::st_bbox(pol))

    # calculate first and last window positions
    # trap begin and end exceptions
    w <- window_size%/%2
    w_f <- ifelse(i-w <= 0, 1, i-w)
    w_l <- ifelse(i+w >= length(dates), length(dates), i+w)

    # create a subset of the data to fit
    # the skylight model to
    subs <- data[which(data$date %in% dates[w_f:w_l]),]

    # fit model parameters for a given
    # day to estimate the location
    out <- stk_fit(
        data = subs,
        roi = roi,
        loc = sf::st_coordinates(loc),
        scale = scale,
        control = control,
        step_selection = step_selection
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
      cli::cli_progress_update()
    }

    if(plot & i %in% plot_update){
      p <- try(stk_map(
        locations,
        bbox = sf::st_bbox(mask),
        start_location = start_location,
        roi = pol # forward roi polygon / not raster
      ))

      if(!inherits(p, "try-error")){
        plot(p)
      }
    }
  }

  # cleanup of progress bar
  if(verbose) {
    cli::cli_progress_done()
    cli::cli_alert_info(
      "Data processing done ..."
    )
  }

  # save setup
  locations$tolerance <- tolerance
  locations$range <- list(range)
  locations$control <- list(control)
  locations$scale <- list(scale)
  locations$window_size <- window_size

  # return the data frame with
  # location
  return(locations)
}
