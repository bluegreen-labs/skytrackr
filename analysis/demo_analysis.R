# load libraries and functions
options("dplyr.show_progress" = FALSE)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(skytrackr)

data <- stk_read_lux("data-raw/CC874_18Jun22_123407.lux")

# batch processing via pipe for multiple sites
locations <- data |>
  group_by(logger) |>
  do({
    skytrackr(
      .,
      iterations = 100,
      particles = 10,
      start_location = c(51.08, 3.73),
      tolerance = 11,
      plot = FALSE
    )
  })

saveRDS(locations, "data-raw/gent_locations_SMC.rds", compress = "xz")

#---- DEzs MCMC approach ----

locations <- data |>
  group_by(logger) |>
  do({
    skytrackr(
      .,
      iterations = 20000,
      start_location = c(51.08, 3.73),
      tolerance = 11,
      bbox = c(-20, -40, 60, 60),
      control = list(
          sampler = 'DEzs',
          settings = list(
              burnin = iterations * 0.2,
              iterations = iterations * 0.8,
              message = FALSE
          )
        ),
      plot = FALSE
    )
  })

saveRDS(locations, "data-raw/gent_locations_DEzs.rds", compress = "xz")
