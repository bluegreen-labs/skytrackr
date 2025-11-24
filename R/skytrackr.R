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
#'  provided in lux c(min, max), or a single value. If providing
#'  a single value a twilight threshold based method will be used rather
#'  than a template matching approach. The underlying optimization will remain
#'  the same.
#' @param scale Scale / sky condition factor, by default covering the
#'  skylight() range of 1-10 (from clear sky to extensive cloud coverage)
#'  but can be extended for more flexibility to account for coverage by plumage,
#'  note that in case of non-physical accurate lux measurements values can have
#'  a range starting at 0.0001 (a multiplier instead of a divider)
#'  (default = c(1,50)).
#' @param control Control settings for the Bayesian optimization, generally
#'  should not be altered (defaults to a Monte Carlo method). For detailed
#'  information I refer to the BayesianTools package documentation.
#' @param mask Mask to constrain positions to land
#' @param step_selection A step-selection function on the distance of a proposed
#'  move, step selection is specified on distance (in km) basis. If missing,
#'  (default = NULL) no distance based constraints are used, aside from the
#'  tolerance value (a hard limit on the distance of travel).
#' @param model model to use, either "diurnal" calculating the diurnal profile
#'  fit using a single set of locations, or "individual" where the positions
#'  are updated along the flight track but only the last position is reported
#'  back (default = diurnal). For individual flight tracks the assumption is
#'  that the flight bearing isn't constantly updated, but only adjusted after
#'  key (twilight) events. Between updates individuals move along a track of
#'  constant bearing (loxodrome).
#' @param MAP use the maximum aposteriori method to determine the best estimate
#'  parameter set (default = TRUE). If FALSE, the median of a MCMC sample is used.
#' @param smooth smooth the data before processing (default = TRUE)
#' @param clip value over which lux values are clipped, to be set to the
#'  saturation value of your system when using the full diurnal profile (not only
#'  twilight) (default = NULL)
#' @param plot Plot a map during location estimation (updated every seven days)
#' @param verbose Give feedback including a progress bar (TRUE or FALSE,
#'  default = TRUE)

#' @param debug debugging info and plots
#'
#' @importFrom rlang .data
#' @importFrom utils packageVersion
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
    scale = c(1, 50),
    control = list(
      sampler = 'DEzs',
      settings = list(
        burnin = 1000,
        iterations = 3000,
        message = FALSE
      )
    ),
    mask,
    step_selection = NULL,
    model = "diurnal",
    smooth = TRUE,
    MAP = TRUE,
    clip = NULL,
    plot = TRUE,
    verbose = TRUE,
    debug = FALSE
) {

  if(debug){
    plot = FALSE
  }

  if(verbose){
    cli::cli_div(
      theme = list(
        rule = list(
          color = "darkgrey",
          "line-type" = "double",
          "margin-bottom" = 1,
          "margin-top" = 1
        ),
        span.strong = list(color = "black"))
    )
    cli::cli_rule(
      left = "{.strong Estimating locations}",
      right = "{.pkg skytrackr v{packageVersion('skytrackr')}}",

    )
    cli::cli_end()
    cli::cli_alert_info(
      "Processing logger: {.strong {data$logger[1]}}!"
    )
  }

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

  if(length(range) != 2) {
    cli::cli_bullets(c(
      "x" = "The range parameter has only one value!"
      )
    )
  }

  if(is.null(scale)){
    if(verbose){
      cli::cli_bullets(c(
        "i" = "No scale range is provided, will be estimated from data!"
        )
      )
    }

    # calculating informed priors for the scale
    # parameter
    scales <- data |>
      stk_calibrate(
        distribution = TRUE,
        verbose = verbose
      )

    # merge with data file, sets daily priors
    data <- dplyr::left_join(data, scales, by = c("logger", "date"))
  }

  if(length(scale) == 1) {
    cli::cli_bullets(c(
      "i" = "Single scale value is provided, using it as a global prior!"
      )
    )

    # set global scale prior
    data$scale <- scale
  }

  # filter the light data, for the diurnal model
  # all filtered data is removed, for the alternative
  # geodesic model all values are retained, but labelled
  # for later use
  data <- data |>
      skytrackr::stk_filter(
        range = range,
        filter = TRUE,
        verbose = verbose
      )

   data <- data |>
    tidyr::pivot_wider(
      names_from = "measurement",
      values_from = "value"
    )

  # convert to log lux
  data <- data |>
    dplyr::mutate(
      lux = log(.data$lux)
    )

  # calculate time_steps between data points
  # used to calculate distances covered
  data <- data |>
    dplyr::mutate(
    time_step =
      c(0,as.numeric(diff(.data$date_time, units = "secs")))
  )

  # unique dates
  dates <- unique(data$date)

  # empty data frame
  locations <- data.frame()

  # create progress bar
  if(verbose) {

    if(plot){
      cli::cli_alert_info(
        "(preview plot will update every 2 days)"
      )
    }

    cli::cli_progress_bar(
      " - Estimating positions",
      total = length(dates)
    )
  }

  # plot updates every 2 days (if possible)
  if(length(dates) >= 2){
    plot_update <- seq(2, length(dates), by = 2)
  } else {
    plot_update  <- length(dates)
  }

  # loop over all available dates
  for (i in seq_along(dates)) {
    if (i != 1) {

          # by default use maximum aposteriori values
          # to estimate the best parameter value (default)
          # otherwise use the median of a sample
          if (MAP) {
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
            loc <-  sf::st_as_sf(
              data.frame(
                lon = locations$longitude_qt_50[i-1],
                lat = locations$latitude_qt_50[i-1]
              ),
              coords = c("lon","lat")
            ) |> sf::st_set_crs(4326)
          }

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

    # create a subset of the data to fit
    # the skylight model to
    subs <- data[which(data$date %in% dates[i]),]

    # set proper scaling factor
    if(is.null(scale) | length(scale) == 1){
      scale_subs <- subs$scale[1]
    } else {
      scale_subs <- scale
    }

    # fit model parameters for a given
    # day to estimate the location
    out <- stk_fit(
        data = subs,
        roi = roi,
        loc = sf::st_coordinates(loc),
        scale = scale_subs,
        control = control,
        step_selection = step_selection,
        clip = clip,
        model = model
      )

    # plot debugging graph of fit curve for every day / period
    if (debug){

      par <- c(
        out$latitude,
        out$longitude,
        out$sky_conditions
      )

      if(model == "diurnal"){
        ll <- diurnal(
          par,
          data = subs,
          loc = sf::st_coordinates(loc)
        )

        subs$sun_illuminance <- ll
      } else {
        ll <- individual(
          par,
          data = subs,
          loc = sf::st_coordinates(loc)
        )

        subs$sun_illuminance <- ll
      }

      if(!is.null(clip)){
        subs <- subs |>
          dplyr::mutate(
            sun_illuminance = ifelse(
              .data$sun_illuminance > log(clip),
              log(clip),
              .data$sun_illuminance)
          )
      }

      graphics::par(mfrow=c(1,1))
      plot(
        subs$date_time,
        subs$lux,
        ylim = c(-5, 12),
        main = paste(
          "lat:", round(out$latitude,3),
          "lon:", round(out$longitude,3),
          "sky:", round(out$sky_conditions,3)
          ),
        ylab = "log(lux)",
        xlab = "time"
        )
      graphics::points(
        subs$date_time,
        subs$sun_illuminance,
        col = "red"
      )
    }

    # set date
    out$date <- dates[i]
    out$logger <- data$logger[1]

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
  locations$scale <- list(scale)
  locations$range <- list(range)
  locations$control <- list(control)
  locations$clip <- clip
  locations$model <- model
  locations$step_selection <- list(step_selection)
  locations$version <- as.character(packageVersion('skytrackr'))

  # return the data frame with
  # location
  return(locations)
}
