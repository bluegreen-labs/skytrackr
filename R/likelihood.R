
#' Log likelihood cost function for model optimization
#'
#' The function is aimed to be maximized, to use it with optimizers which
#' minimize cost functions wrap the function as such:
#' `cost = function(...){abs(likelihood(...))}`
#'
#' @param par a vector of parameter values, including one for the uncertainty
#'  on the target values
#' @param data nested data structure with validation data included
#' @param model model to run with data and par settings
#' @param ... extra arguments to pass to the function
#' @return single log likelihood
#' @keywords model, optimization, cost function
#' @export

likelihood <- function(
    par,
    data,
    model,
    ...
) {

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

  return(sum(singlelikelihoods, na.rm = TRUE))
}
