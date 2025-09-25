#' Generate a land surface mask
#'
#' Returns a (buffered) land mask to constrain potential
#' model results.
#'
#' @param buffer The buffer distance from land areas (in km, default = 0
#'  excluding all water bodies).
#' @param resolution The resolution of the spatial grid in degrees, when exporting
#'  as a terra SpatRaster (default = 1).
#' @param bbox A bounding box of the mask to constrain the estimated location
#'  parameter space.
#' @param sf Return the land mask as an 'sf' polygon, not a rasterized map for.
#'  use in map plotting, not used for processing (default = FALSE)
#'
#' @return A buffered land mask as an 'sf' or 'terra' map object.
#' @export
#' @examples
#' \dontrun{
#' # define land mask with a bounding box
#' # and an off-shore buffer (in km), in addition
#' # you can specifiy the resolution of the resulting raster
#' mask <- stk_mask(
#'   bbox  =  c(-20, -40, 60, 60), #xmin, ymin, xmax, ymax
#'   buffer = 150, # in km
#'   resolution = 0.5 # map grid in degrees
#'   )
#' }

stk_mask <- memoise::memoise(
  function(
    buffer = 0,
    resolution = 1,
    bbox,
    sf = FALSE
){

  sf::sf_use_s2(FALSE)

  # read polygon data convert to sf (formally)
  #land <- readRDS(system.file("extdata/mask.rds", package="skytrackr"))

  if(!missing(bbox)){
    # set global bounding box
    names(bbox) = c("xmin","ymin","xmax","ymax")
    bbox_sf <- sf::st_as_sfc(
      sf::st_bbox(bbox)
    ) |>
      sf::st_set_crs(4326)

    land <- suppressWarnings({suppressMessages({
      land |>
        sf::st_crop(bbox_sf)
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
  ref <- terra::rast(
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
