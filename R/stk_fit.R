#' Fit illuminance (lux) profile
#'
#' Fits a simulated lux profile to observed light logger data
#' to estimate locations (parameters).
#'
#' @param data A skytrackr data frame
#' @param roi A region of interest defined by a dynamic bounding box (set via
#'  the tolerance value and relative to the previous step)
#' @param loc The location of the previous step
#' @param scale Scale / sky condition factor covering the
#'  skylight() range of 1-10 (from clear sky to extensive cloud coverage)
#'  but can be extended for more flexibility to account for coverage by plumage,
#'  note that in case of non-physical accurate lux measurements values can have
#'  a range starting at 0.0001 (a multiplier instead of a divider).
#' @param control Control settings for the Bayesian optimization, generally
#'  should not be altered (defaults to a Monte Carlo method). For detailed
#'  information I refer to the BayesianTools package documentation.
#' @param step_selection A step selection function on the distance of a proposed
#'  move, step selection is specified on distance (in km) basis.
#' @param clip value over which lux values are clipped, to be set to the
#'  saturation value of your system when using the full diurnal profile (not only
#'  twilight) (default = NULL)
#'
#' @return An estimated illuminance based location (and its uncertainties).
#' @export

stk_fit <- function(
  data,
  roi,
  loc,
  scale,
  control,
  step_selection,
  clip
  ) {

  # bbox
  bbox <- roi |> sf::st_bbox()

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
                   model = "log_lux",
                   loc = loc,
                   roi = roi,
                   step_selection = step_selection,
                   clip = clip
              ))},
    # include an additional parameter
    # range for data uncertainty
    lower = c(lower, 0),
    upper = c(upper, 1)
  )

  # calculate the optimization
  # run and return results
  # [suppress all output]
  out <- #suppressWarnings(
    #suppressMessages(
      BayesianTools::runMCMC(
        bayesianSetup = setup,
        sampler = control$sampler,
        settings = control$settings
      )
    #)
  #)

  # Gelman-Brooks-Rubin (GBR) potential
  # scale factors to check convergence
  # convergence between 1.05 and 1.1
  # only applies to DEzs as it has
  # 3 chains by default
  if (control$sampler == "DEzs") {
    grd <- suppressWarnings(
      suppressMessages(
        BayesianTools::gelmanDiagnostics(out)$mpsrf
        )
      )
  } else {
    grd <- NA
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

  sky_conditions <- stats::quantile(
    samples_par[,3],
    c(0.05,0.5,0.95),
    na.rm = TRUE
  )
  # return "best" fit parameters
  bf_par <- BayesianTools::MAP(out)$parametersMAP

  # return data as a structured
  # data frame
  data.frame(
    latitude = bf_par[1],
    longitude = bf_par[2],
    sky_conditions = exp(bf_par[3]),
    latitude_qt_50 = latitude[2],
    longitude_qt_50 = longitude[2],
    sky_conditions_qt_50 = exp(sky_conditions[2]),
    latitude_qt_5 = latitude[1],
    latitude_qt_95 = latitude[3],
    longitude_qt_5 = longitude[1],
    longitude_qt_95 = longitude[3],
    sky_conditions_qt_5 = exp(sky_conditions[1]),
    sky_conditions_qt_95 = exp(sky_conditions[3]),
    grd = grd,
    n = nrow(data),
    row.names = NULL
  )
}
