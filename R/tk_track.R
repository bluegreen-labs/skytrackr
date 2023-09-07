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

tk_track<- function(
    par,
    data
    #land_mask
    ) {

  # split out model parameters
  lat <- par[1]
  lon <- par[2]

    illuminance <- skylight(
      longitude = lon,
      latitude = lat,
      date = data$date_time
    )$total_illuminance
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
  #previous_position,
  iterations,
  bbox
  ) {

  # calculate buffer distance
  #buffer <- (speed * 60 * 60 * 24 * step)/(110 * 1000)

  # buffer around previous location
  # bbox <- sf::st_as_sf(
  #   x = data.frame(
  #     lat = previous_position$latitude,
  #     lon = previous_position$longitude
  #   ),
  #   coords = c("lat","lon")
  #  ) |>
  #   sf::st_buffer(
  #     dist = buffer
  #   ) |>
  #   st_bbox()
  #
  # lower <- c(bbox['ymin'], bbox['xmin'])
  # upper <- c(bbox['ymax'], bbox['xmax'])

  lower <- c(bbox[2:1])
  upper <- c(bbox[4:3])

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
    upper = c(upper, 0.5)
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
  # longitude <- suppressWarnings(
  #   as.numeric(
  #     circular::quantile.circular(
  #       circular::as.circular(
  #         samples_par[,2],
  #         type = "angles",
  #         units ="degrees"
  #       ),
  #       c(0.05,0.5,0.95),
  #       na.rm = TRUE
  #     )
  #   )
  # )

  longitude <- quantile(
    samples_par[,2],
    c(0.05,0.5,0.95),
    na.rm = TRUE
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
    longitude = longitude[2],
    latitude = latitude[2],
    latitude_ci_5 = latitude[1],
    latitude_ci_95 = latitude[3],
    longitude_ci_5 = longitude[1],
    longitude_ci_95 = longitude[3],
    grb = grb,
    row.names = NULL
  )

}
