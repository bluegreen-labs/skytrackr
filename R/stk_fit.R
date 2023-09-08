#' Fit illuminance (lux) profile
#'
#' Fits a simulated lux profile to observed light logger data
#' to estimate locations (parameters).
#'
#' @param data a data frame containing date time and lux values
#' @param iterations number of optimization iterations
#' @param bbox bounding box of the location search domain given as
#'  c(xmin, ymin, xmax, ymax)
#'
#' @return an estimated illuminance based location (and its uncertainties)
#' @export

stk_fit <- function(
  data,
  iterations,
  bbox = c(-180, -90, 180, 90)
  ) {

  # set lower and upper parameter ranges
  # from bounding box settings
  lower <- c(bbox[2:1], 0)
  upper <- c(bbox[4:3], 20)

  # Bayesian optimization routine
  control <- list(
    sampler = 'DEzs',
    settings = list(
      burnin = iterations * 0.2,
      iterations = iterations * 0.8,
      message = FALSE
    )
  )

  # setup of the BT setup
  setup <- BayesianTools::createBayesianSetup(
    likelihood = function(random_par){
      do.call("likelihood",
              list(par = random_par,
                   data = data,
                   model = "log_lux"
              ))},
    # include an additional parameter
    # range for data uncertainty
    lower = c(lower, 0),
    upper = c(upper, 1)
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
  grb <- BayesianTools::gelmanDiagnostics(out)$mpsrf

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

  longitude <- stats::quantile(
    samples_par[,2],
    c(0.05,0.5,0.95),
    na.rm = TRUE
  )

  # use plain quantiles for latitude
  latitude <- stats::quantile(
    samples_par[,1],
    c(0.05,0.5,0.95),
    na.rm = TRUE
  )

  sky_conditions <- stats::median(
    samples_par[,3],
    na.rm = TRUE
    )

  # return data as a structured
  # data frame
  data.frame(
    longitude = longitude[2],
    latitude = latitude[2],
    latitude_ci_5 = latitude[1],
    latitude_ci_95 = latitude[3],
    longitude_ci_5 = longitude[1],
    longitude_ci_95 = longitude[3],
    sky_conditions = sky_conditions,
    grb = grb,
    row.names = NULL
  )

}
