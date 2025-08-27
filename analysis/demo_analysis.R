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
library(patchwork)
lapply(list.files("R/","*.R", full.names = TRUE), source)
#library(skytrackr)

df <- stk_read_lux("data-raw/CC876_22Jun22_161546.lux")
df <- df |>
  dplyr::filter(
    (date >= "2021-08-02" & date <= "2022-05-09")
  )

#---- DEzs MCMC approach ----

bbox <- c(-20, -40, 60, 60)

mask <- stk_mask(
  bbox  =  bbox,
  buffer = 150,
  resolution = 0.5
)

locations <- df |>
  group_by(logger) |>
  do({
    skytrackr(
      .,
      mask = mask,
      plot = TRUE,
      start_location = c(51.08, 3.73),
      tolerance = 1500,
      scale = c(1,10),
      range = c(0.32, 10),
      control = list(
        sampler = 'DEzs',
        settings = list(
          iterations = 500,
          message = FALSE
        )
      )
    )
  })

#saveRDS(locations, "analysis/CC786_locations.rds", compress = "xz")
#stk_map(locations, buffer = 0, bbox = bbox)
