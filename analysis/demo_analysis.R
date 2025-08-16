rm(list = ls())
# load libraries and functions
options("dplyr.show_progress" = FALSE)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(BayesianTools)
library(sf)
library(patchwork)
lapply(list.files("R/","*.R", full.names = TRUE), source)
#library(skytrackr)

df <- stk_read_lux("data-raw/CC876_22Jun22_161546.lux")
df <- df |>
  dplyr::filter(
    (date >= "2021-08-01" & date <= "2022-05-09")
  )

#---- DEzs MCMC approach ----

bbox <- c(-20, -40, 60, 60)

locations <- df |>
  group_by(logger) |>
  do({
    skytrackr(
      .,
      plot = TRUE,
      land_mask = TRUE,
      buffer = 5,
      start_location = c(51.08, 3.73),
      tolerance = 15,
      scale = c(1,12),
      range = c(0.32, 10),
      bbox = c(-20, -40, 60, 60),
      control = list(
        sampler = 'DEzs',
        settings = list(
          iterations = 1000,
          message = FALSE
        )
      )
    )
  })

stk_map(locations, buffer = 0, bbox = bbox)
