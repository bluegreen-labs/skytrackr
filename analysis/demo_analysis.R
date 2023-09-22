# load libraries and functions
options("dplyr.show_progress" = FALSE)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(sf)
lapply(list.files("R/","*.R", full.names = TRUE), source)
#library(skytrackr)

df <- stk_read_lux("data-raw/CH760_11Jun23_051213.lux")

df <- df |>
  dplyr::filter(
    (date >= "2022-08-12" & date <= "2023-04-27")
  )

# # batch processing via pipe for multiple sites
# locations <- data |>
#   group_by(logger) |>
#   do({
#     skytrackr(
#       .,
#       iterations = 100,
#       particles = 10,
#       start_location = c(51.08, 3.73),
#       tolerance = 11,
#       plot = FALSE
#     )
#   })
#
# saveRDS(locations, "data-raw/gent_locations_SMC.rds", compress = "xz")

#---- DEzs MCMC approach ----

locations <- df |>
  group_by(logger) |>
  do({
    skytrackr(
      .,
      start_location = c(51.08, 3.73),
      tolerance = 11,
      bbox = c(-20, -40, 60, 60),
      land_mask = TRUE,
      plot = TRUE
    )
  })

#saveRDS(locations, "data-raw/gent_locations_DEzs.rds", compress = "xz")

