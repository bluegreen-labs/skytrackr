# load libraries and functions
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(skytrackr)
# source("R/skytrackr.R")
# source("R/log_lux.R")
# source("R/likelihood.R")
# source("R/stk_fit.R")
#source("R/stk_read.R")

files <- list.files(
  "~/Dropbox/Research_Projects/code_repository/bitbucket/apus_lunar_synchrony/data-raw/geolocators/Gent_Voorhaven_A_apus/",
  glob2rx("*.lux"),
  full.names = TRUE
)

files <- files[!grepl("drift", files)]
data <- lapply(files, function(file){
  stk_read_lux(file)
})

data <- do.call("rbind", data)

data <- data |>
  dplyr::filter(
    logger == "CC874"
  )

options("dplyr.show_progress" = FALSE)
# batch processing via pipe
locations <- data |>
  group_by(logger) |>
  do({
    skytrackr(
      .,
      iterations = 8000,
      start_location = c(51.08, 3.73),
      tolerance = 13,
      bbox = c(-20, -40, 60, 60), # reasonable search area global
      plot = TRUE
    )
  })

saveRDS(locations, "~/Desktop/gent_locations.rds", compress = "xz")

#----- more complex optimization ----

# Adaptive MCMC, prior optimization, delayed rejection (DRAM)
#
# locations <- data |>
#   group_by(logger) |>
#   do({
#     skytrackr(
#       .,
#       iterations = 8000,
#       start_location = c(51.08, 3.73),
#       tolerance = 13,
#       control = list(
#         sampler = 'Metropolis',
#         settings = list(
#           iterations = 20000,
#           adapt = T,
#           DRlevels = 2,
#           gibbsProbabilities = NULL,
#           temperingFunction = NULL,
#           optimize = T,
#           message = FALSE
#         )
#       ),
#       plot = TRUE
#     )
#   })
#
# saveRDS(locations, "data-raw/gent_locations_dram.rds", compress = "xz")
