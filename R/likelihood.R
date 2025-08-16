
#' Log likelihood cost function for model optimization
#'
#' The function is aimed to be maximized, to use it with optimizers which
#' minimize cost functions wrap the function as such:
#' `cost = function(...){abs(likelihood(...))}`
#'
#' @param par a vector of parameter values, including one for the uncertainty
#'  on the target values
#' @param data nested data structure with validation data included
#' @param model model to run with data and par setings
#' @param loc previous modeled step location
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
    ...
) {

  # if(!missing(roi)){
  #   # Estimate intersects with ROI
  #   # if not drop
  #   intersects <- sf::st_intersects(
  #     sf::st_as_sf(
  #       data.frame(lon = par[2],lat = par[1]),
  #       coords = c("lon","lat")
  #     ) |> st_set_crs(4326),
  #     roi,
  #     sparse = FALSE
  #   )
  #
  #   if(!intersects){
  #     return(-Inf)
  #   }
  # }

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

  # add density and other parameters here
  # to be included in cost function

  density <- function(par, loc = loc){
    d_lat <- dnorm(par[1], mean = loc[2], sd = 1, log = TRUE)
    d_lon <- dnorm(par[2], mean = loc[1], sd = 1, log = TRUE)
    return(d_lat + d_lon)
  }

  d <- density(par, loc)

  return(sll + d)
}
