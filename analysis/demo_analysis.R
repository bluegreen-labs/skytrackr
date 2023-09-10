# load libraries and functions
options("dplyr.show_progress" = FALSE)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(skytrackr)

# files <- list.files(
#   "~/Dropbox/Research_Projects/code_repository/bitbucket/apus_lunar_synchrony/data-raw/geolocators/Gent_Voorhaven_A_apus/",
#   glob2rx("*.lux"),
#   full.names = TRUE
# )
#
# files <- files[grepl("drift", files)]
#
# data <- lapply(files, function(file){
#   stk_read_lux(file)
# })
#
# data <- do.call("rbind", data)
#
# data <- data |>
#   dplyr::filter(
#     logger == "CC874"
#   )

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
