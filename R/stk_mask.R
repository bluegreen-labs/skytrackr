#' A land mask
#'
#' returns a buffered land mask to constrain potential
#' model results
#'
#' @param buffer buffer in degrees
#'
#' @return buffered land mask as an sf object
#' @export

stk_mask <- memoise::memoise(function(buffer = 1.5) {

  sf::sf_use_s2(FALSE)
  mask <- readRDS(system.file("extdata/mask.rds", package="skytrackr"))

  land <- suppressMessages(suppressWarnings(
    mask |>
      sf::st_geometry() |>
      sf::st_union() |>
      sf::st_buffer(buffer)
    )
  )

  return(land)

})
