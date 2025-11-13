rm(list = ls())
# load libraries and functions
options("dplyr.show_progress" = FALSE)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(BayesianTools)
library(sf)
library(terra)
library(stars)
library(geosphere)
library(patchwork)
set.seed(1)
lapply(
  list.files("R/", "*", full.names = TRUE),
  function(file){
    source(file)})

#---- DEzs MCMC approach ----

tol <- 2500

# define land mask with a bounding box
# and an off-shore buffer (in km), in addition
# you can specifiy the resolution of the resulting raster
mask <- stk_mask(
  bbox  =  c(-20, -40, 60, 60), #xmin, ymin, xmax, ymax
  buffer = 150, # in km
  resolution = 0.5 # map grid in degrees
)

# define a step selection distribution
ssf <- function(x, shape = 0.9, scale = 100, tolerance = tol){
  # normalize over expected range with km increments
  norm <- sum(stats::dgamma(1:tolerance, shape = shape, scale = scale))
  prob <- stats::dgamma(x, shape = shape, scale = scale) / norm
  return(prob)
}

df <- cc876 |> filter(date < "2021-08-10")
scale <- df |> stk_calibrate()

locations <- df |>
    skytrackr(
      mask = mask,
      plot = TRUE,
      #debug = TRUE,
      speed = c(0.0001, 20),
      start_location = c(51.08, 3.73), # Gent - lux file
      tolerance = tol, # in km
      scale = log(scale),
      range = c(0.09, 148),
      control = list(
        sampler = 'DEzs',
        settings = list(
          burnin = 100,
          iterations = 300,
          message = FALSE
        )
      ),
      clip = NULL,
      step_selection = ssf
    )

#saveRDS(locations, "analysis/demo_data.rds", compress = "xz")

# PIRASALI
# df <- stk_read_glf("inst/extdata/22LE_20200218.glf")
#
# df <- df |>
#   filter(
#     date >= "2018-10-10" & date <= "2019-02-27"
#   )

# test <- df |> filter(date == "2019-09-01")
#
# test$latitude <- 47.5
# test$longitude <- 8.25
#
# test <- bind_cols(test, skylight::skylight(
#   latitude = test$latitude,
#   longitude = test$longitude,
#   date = test$date_time,
#   sky_condition = 0.01
#   )
# ) |>
#   filter(
#     value > 0
#   )
#
#
# ggplot(test) +
#   geom_point(
#     aes(date_time, log(value))
#   ) +
#   geom_point(
#    aes(date_time, log(total_illuminance)),
#    col = "red"
#   )
