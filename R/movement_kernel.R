#' Convert roi mask to movement kernel
#'
#' Applies a distance matrix and weibull
#' density function to a region of interest
#'
#' This is approximative of probability density
#' data, good enough for now. Should be
#' corrected for cell spacing / count,
#' see Ranc et al. 2022 Ecology Letters
#'
#' @param roi region of interest raster file
#' @param shape weibull shape parameter
#' @param scale weibull scale parameter
#'
#' @returns weibull log likelihoods (approximation)
#' @export

movement_kernel <- function(
    roi,
    shape = 0.9,
    scale = 2
){

  im <- as.matrix(roi, wide = TRUE)

  # calculate distance from the center of the image
  offset <- ceiling(dim(im)[1] / 2)

  # define distance matrix
  # r is rounded to an integer by zonal
  r <- rast(
    sqrt((col(im) - offset)^2 + (row(im) - offset)^2),
    crs = terra::crs(roi),
    extent = ext(roi)
  )

  # multiply with original
  # to apply distance mask
  roi <- r * roi

  # apply weibull function
  roi <- app(
    roi,
    fun = \(x) dweibull(
      x,
      shape = shape,
      scale = scale,
      log = TRUE
    )
  )

  return(roi)
}

#' Step selection
#'
#' Step selection probability density function
#' based upon the distance between a reference
#' point (previous step) and a new parameter location.
#'
#' Step selection will follow a gamma distribution to
#' be defined with scale and shape parameters (either
#' through fitting known data, e.g. GPS data, or by
#' making reasonable assumption on behaviour).
#'
#' @param par (random) parameter location coordinates
#' @param ref reference location coordinates
#' @param shape gamma distribution shape parameter
#' @param scale gamma distribution scale parameter
#'
#' @returns log-likelihood of a step distance given a gamma distribution
#' @export

step_selection <- function(par, ref, shape = 1.02, scale = 150000){
  d <- dgamma(
    geosphere::distGeo(ref, par[2:1]),
    shape = shape,
    scale = scale,
    log = TRUE
  )
  return(d)
}
