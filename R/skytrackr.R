
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
#' @param tolerance tolerance distance on the search window for optimization,
#'  given in km (left/right, top/bottom). Sets a hard limit on the search window
#'  regardless of the step selection function used.
#' @param range range of values to consider during processing, should be
#'  provided in lux c(min, max) or the equivalent if non-calibrated. In case of
#'  non calibrated values you will need to adapt the scale values accordingly.
#' @param scale scale / sky condition factor, by default covering the
#'  skylight() range of 1-10 (from clear sky to extensive cloud coverage)
#'  but can be extended for more flexibility to account for coverage by plumage,
#'  note that in case of non-physical accurate lux measurements values can have
#'  a range starting at 0.0001 (a multiplier instead of a divider).
#' @param control control settings for the Bayesian optimization, generally
#'  should not be altered (defaults to a sequential monte carlo method)
#' @param mask mask with priors to constrain positions
#' @param step_selection a step selection function on the distance of a proposed
#'  move, step selection is specified as average flight speed to achieve this
#'  distance (in km/h).
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
    tolerance = 1500,
    range = c(0.32, 140),
    scale = c(1, 10),
    control = list(
      sampler = 'DEzs',
      settings = list(
        burnin = 1000,
        iterations = 2000,
        message = FALSE
      )
    ),
    mask,
    step_selection,
    plot = TRUE,
    verbose = TRUE
) {

  if(missing(mask)){
    cl::cli_alert_danger("
        - please provide a base mask or grid of valid sample locations!
          ")
    stop()
  }

  if(missing(start_location)) {
    cl::cli_alert_danger("
        - No (approximate) start location provided, please provide a start location!
        ")
    stop()
  }

  # unravel the light data
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
      "estimating positions",
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

    # create a subset
    subs <- data[which(data$date == dates[i]),]

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

      p <- stk_map(
        locations,
        bbox = sf::st_bbox(mask),
        start_location = start_location,
        roi = pol # forward roi polygon / not raster
      )

      plot(p)
    }
  }

  # cleanup of progress bar
  if(verbose) {
    cli::cli_progress_done()
  }

  # return the data frame with
  # location
  return(locations)
}
