#' Fit illuminance (lux) profile
#'
#' Fits a simulated lux profile to observed light logger data
#' to estimate locations (parameters).
#'
#' @param data a data frame containing date time and lux values
#' @param iterations number of optimization iterations
#' @param bbox bounding box of the location search domain given as
#'  c(xmin, ymin, xmax, ymax)
#' @param scale scale factor range due to cloudiness to use in optimization,
#'  when target values are not provided in lux this can be used to effectively
#'  implement a Hill-Ekstrom template fitting
#' @param control control settings for the Bayesian optimization, forwarded by
#'  skytrackr()
#'
#' @return an estimated illuminance based location (and its uncertainties)
#' @export

stk_fit <- function(
  data,
  iterations,
  bbox,
  scale,
  control
  ) {

  # set lower and upper parameter ranges
  # from bounding box settings add scale
  # factor for sky conditions
  lower <- c(bbox[2:1], scale[1])
  upper <- c(bbox[4:3], scale[2])

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
  # [suppress all output]
  out <- suppressWarnings(
    suppressMessages(
      BayesianTools::runMCMC(
        bayesianSetup = setup,
        sampler = control$sampler,
        settings = control$settings
      )
    )
  )

  # Gelman-Brooks-Rubin (GBR) potential
  # scale factors to check convergence
  # convergence between 1.05 and 1.1
  # only applies to DEzs as it has
  # 3 chains by default
  if (control$sampler == "DEzs") {
    grb <- suppressWarnings(BayesianTools::gelmanDiagnostics(out)$mpsrf)
  } else {
    grb <- NA
  }

  # sample the posterior distribution
  # with thinning factor is 10
  samples_par <- BayesianTools::getSample(
    out
  )

  # to deal with the date line use circular
  # quantiles to get median and CI for the
  # longitude
  longitude <- suppressWarnings(
    as.numeric(
      circular::quantile.circular(
        circular::as.circular(
          samples_par[,2],
          zero = pi,
          units ="degrees"
        ),
        c(0.05,0.5,0.95),
        na.rm = TRUE
      )
    )
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

  # return "best" fit parameters
  bf_par <- BayesianTools::MAP(out)$parametersMAP

  # return data as a structured
  # data frame
  data.frame(
    latitude = bf_par[1],
    longitude = bf_par[2],
    sky_conditions = bf_par[3],
    latitude_qt_50 = latitude[2],
    longitude_qt_50 = longitude[2],
    latitude_qt_5 = latitude[1],
    latitude_qt_95 = latitude[3],
    longitude_qt_5 = longitude[1],
    longitude_qt_95 = longitude[3],
    sky_conditions_qt_50 = sky_conditions,
    grb = grb,
    n = nrow(data),
    row.names = NULL
  )
}
