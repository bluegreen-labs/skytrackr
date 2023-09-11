# load libraries and functions
options("dplyr.show_progress" = FALSE)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(skytrackr)

files <- list.files(
  "~/Dropbox/Research_Projects/code_repository/bitbucket/apus_lunar_synchrony/data-raw/geolocators/Gent_Voorhaven_A_apus/",
  glob2rx("*.lux"),
  full.names = TRUE
)

files <- files[!grepl("drift", files)]
data <- lapply(files, function(file){
  stk_read_lux(file)
})

data <- do.call("rbind", data) |>
  filter(
    logger == "CC876",
    date >= "2021-08-02"
  )

locations <- data |>
  group_by(logger) |>
  do({
    skytrackr(
      .,
      #start_location = c(51.08, 3.73),
      #tolerance = 11,
      iterations = 20,
      particles = 100,
      range = c(0.32, 150),
      bbox = c(-20, -40, 60, 60)
    )
  })

saveRDS(locations, "data-raw/gent_locations_SMC_no_tolerance.rds", compress = "xz")

locations <- data |>
  group_by(logger) |>
  do({
    skytrackr(
      .,
      start_location = c(51.08, 3.73),
      range = c(0.32, 150),
      tolerance = 11,
      iterations = 20,
      particles = 100,
      bbox = c(-20, -40, 60, 60)
    )
  })

saveRDS(locations, "data-raw/gent_locations_SMC_tolerance.rds", compress = "xz")

locations <- data |>
  group_by(logger) |>
  do({
    skytrackr(
      .,
      #start_location = c(51.08, 3.73),
      #tolerance = 11,
      range = c(0.32, 150),
      bbox = c(-20, -40, 60, 60),
      control = list(
        sampler = 'DEzs',
        settings = list(
          burnin = 2000,
          iterations = 18000,
          message = FALSE
        )
      )
    )
  })

saveRDS(locations, "data-raw/gent_locations_DEzs_no_tolerance.rds", compress = "xz")


locations <- data |>
  group_by(logger) |>
  do({
    skytrackr(
      .,
      start_location = c(51.08, 3.73),
      tolerance = 11,
      range = c(0.32, 150),
      bbox = c(-20, -40, 60, 60),
      control = list(
        sampler = 'DEzs',
        settings = list(
          burnin = 2000,
          iterations = 18000,
          message = FALSE
        )
      )
    )
  })

saveRDS(locations, "data-raw/gent_locations_DEzs_tolerance.rds", compress = "xz")

