#' Match track
#'
#' Match track
#'
#' @param par three parameters specifying the cycling model
#' @param data data consisting of vegetation greenness (G), mean
#'  daytime temperature (T) and daylenght (D) as a data frame
#'
#' @return Vegetation greenness values
#' @export

tk_track<- function(par, data, buffer, land_mask) {

  # split out model parameters
  lat <- par[1]
  lon <- par[2]

  # combine buffer with land mask

  # check if not exceeding reasonable buffer
  circ <- sf::st_as_sf(
    x = data.frame(
      lat = 40,
      lon = 10
    ),
    coords = c("lat","lon"),
    crs = "epsg:4326"
  )

  if(st_intersects(circ, buffer)){

    illuminance <- skylight(
      longitude = lon,
      latitude = lat,
      date = data$date_time
    )$total_illuminance

    log(illuminance)
  }
}

# parameters for final function:
#
# - data ()
# - floor (values ignored during the night)
# - iterations (MCMC iterations run)
# - land mask TRUE/FALSE
# - start position (lat / lon)

fit_parameters <- function(
  data,
  start_position, # start location
  previous_position, # previous position
  iterations, # mcmc iterations
  speed = 15, # in m/s
  step = 1, # nr steps since last good fix
  land_mask = FALSE
  ) {

  # calculate buffer distance
  buffer <- speed * 60 * 60 * 24 * step

  # scope
  # buffer around prevoius location
  circ <- sf::st_as_sf(
    x = data.frame(
      lat = 40,
      lon = 10
    ),
    coords = c("lat","lon")
   ) |>
    sf::st_buffer(
      dist = buffer
      )

  # Bayesian optimization routine
  control = list(
    sampler = 'DEzs',
    settings = list(
      burnin = iterations * 0.2,
      iterations = iterations * 0.8
    )
  )

  # setup of the BT setup
  setup <- BayesianTools::createBayesianSetup(
    likelihood = function(random_par){
      do.call("likelihood",
              list(par = random_par,
                   data = data,
                   model = "tk_track"
              ))},
    # include an additional parameter
    # range for data uncertainty
    lower = c(lower, 0),
    upper = c(upper, 2)
  )

  # calculate the optimization
  # run and return results
  out <- BayesianTools::runMCMC(
    bayesianSetup = setup,
    sampler = control$sampler,
    settings = control$settings
  )

  # GGelman-Brooks-Rubin (GBR) potential
  # scale factors to check covergence
  # convergence between 1.05 and 1.1
  grb <- gelmanDiagnostics(out)$mpsrf

  # sample the posterior distribution
  # with thinning factor is 10
  samples_par <- BayesianTools::getSample(
    out,
    thin = 10
  )

  # to deal with the date line use circular
  # quantiles to get median and CI for the
  # longitude
  longitude <- suppressWarnings(
    circular::quantile.circular(
      circular::as.circular(
        samples_par[,2],
        type = "angles",
        units ="degrees"
      ),
      c(0.05,0.5,0.95),
      na.rm = TRUE
    )
  )

  # use plain quantiles for latitude
  latitude <- quantile(
    samples_par[,1],
    c(0.05,0.5,0.95),
    na.rm = TRUE
  )

  # sanity checks
  # - distance from previous point can't be further
  #  than flight speed (m/s) * 60 * 60 * 24 meters
  #  away
  #
  #  - increase search window by the above amount
  #   times the skipped steps
  #
  #  - how to create a circle in {sf}?
  #  - implement land mask (naturalearth?)
  #  - propagate last known location or set
  #   to NA?

  data.frame(
    latitude = latitude[2],
    latitude_ci_5 = latitude[1],
    latitude_ci_95 = latitude[3],
    longitude = longitude[2],
    longitude_ci_5 = longitude[1],
    longitude_ci_95 = longitude[3],
    grb = grb
  )

}
