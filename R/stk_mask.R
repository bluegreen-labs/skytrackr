#' A land mask
#'
#' returns a buffered land mask to constrain potential
#' model results
#'
#' @param buffer buffer in degrees
#'
#' @return buffered land mask as an sf object
#' @export

stk_mask <- memoise::memoise(function(buffer = 0) {

  suppressMessages(
    suppressWarnings(
      sf::sf_use_s2(FALSE)
    )
  )

  land <- readRDS(system.file("extdata/mask.rds", package="skytrackr"))

  if(buffer > 0){
    land <- suppressMessages(suppressWarnings(
      land |>
        sf::st_geometry() |>
        sf::st_union() |>
        sf::st_buffer(buffer) |>
        sf::st_union()
      )
    )
  }

  return(
    land
  )

})
