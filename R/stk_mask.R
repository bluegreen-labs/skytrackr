#' A land mask
#'
#' returns a buffered land mask to constrain potential
#' model results
#'
#' @param buffer buffer in degrees
#'
#' @return buffered land mask as an sf object
#' @export

stk_mask <- memoise::memoise(
  function(
    buffer = 0,
    resolution = 1,
    bbox,
    sf = FALSE
){

  # read polygon data convert to sf (formally)
  land <- readRDS(system.file("extdata/mask.rds", package="skytrackr"))

  if(!missing(bbox)){
    # set global bounding box
    names(bbox) = c("xmin","ymin","xmax","ymax")
    bbox_sf <- st_as_sfc(
      sf::st_bbox(bbox)
    ) |>
      sf::st_set_crs(4326)

    land <- suppressWarnings({suppressMessages({
      land |>
        st_crop(bbox_sf)
    })})
  }

  if(buffer > 0){

      # set units as km
      units(buffer) <- "km"

      land <- land |>
        #sf::st_geometry() |>
        sf::st_transform(crs = "+proj=laea") |>
        sf::st_union() |>
        sf::st_buffer(buffer) |>
        sf::st_union()
  }

  # convert to lat / lon
  land <- land |>
    sf::st_transform(crs = "epsg:4326")

  # rasterize using terra

  # create empty reference matrix
  ref <- rast(
    xmin=-180,xmax=180,
    ymin=-90, ymax=90,
    crs = "epsg:4326",
    resolution = resolution
  )

  r_land <- terra::rasterize(
    terra::vect(land),
    ref
  ) |>
    terra::trim()

  if(!sf){
    return(r_land)
  } else {
    return(land)
  }
})
