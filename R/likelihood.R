
#' Log likelihood cost function
#'
#' Main cost function used during optimization, combining both the fit of the
#' illuminance data with the step-selection function.
#'
#' @param par a vector of parameter values, including one for the uncertainty
#'  on the target values
#' @param data nested data structure with validation data included
#' @param model model to run with data and par settings
#' @param loc previous modeled step location
#' @param roi region of interest with valid sampling locations
#' @param step_selection a step selection function on the distance of a proposed move
#' @param ... extra arguments to pass to the function
#' @return single log likelihood
#' @keywords model, optimization, cost function
#' @export

likelihood <- function(
    par,
    data,
    model,
    loc,
    roi,
    step_selection,
    ...
) {

  if(!missing(roi)){
    ancillary <- as.numeric(
      terra::extract(
        roi,
        data.frame(par[2],par[1]),
        ID = FALSE
      )
    )

    if(is.na(ancillary)){
      return(-Inf)
    }
  }

  # model parameters
  model_par <- par[1:(length(par)) - 1]

  # split out sd range parameter
  sd_range <- par[length(par)]

  # run model
  predicted <- do.call(
    model,
    list(
      data = data,
      par = model_par,
      ...
    )
  )

  # singlelikelihood
  singlelikelihoods <- stats::dnorm(
    predicted - data$lux,
    sd = sd_range,
    log = TRUE
  )

  # singlelikelihood for the predicted vs observed values
  sll <- sum(singlelikelihoods, na.rm = TRUE)

  # if not step_selection function is provided
  # return the single log likelihood on the skylight
  # model fit
  if(missing(step_selection) || is.null(step_selection)){
    return(sll)
  } else {
    # calculate distance for step (in km)
    dist <- geosphere::distGeo(loc, par[2:1])/1000

    # step selection function
    step <- step_selection(dist)

    # add mask parameters
    return(sll + log(step))
  }
}
