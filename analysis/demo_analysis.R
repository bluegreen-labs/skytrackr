# load libraries and functions
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

data <- do.call("rbind", data)

data <- data |>
  dplyr::filter(
    logger == "CC874",
    date == "2021-07-11"
  )

cc874 <- data

options("dplyr.show_progress" = FALSE)
# batch processing via pipe
locations <- data |>
  group_by(logger) |>
  do({
    skytrackr(
      .
    )
  })

# saveRDS(locations, "data/gent_locations.rds", compress = "xz")
