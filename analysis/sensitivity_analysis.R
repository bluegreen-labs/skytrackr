# load libraries and functions
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(skytrackr)
countries <- ne_countries(returnclass = "sf")

cc874 <- skytrackr::cc874

# introduce random low values to simulate occulusions (going into nests etc)
# with increasing fractions of the total dataset randomly set to the lowest
# value in the dataset (i.e. dark)

# default setup (only considering twilight)
sensitivity_default <- lapply(0:75, function(i){
  idx <- sample(1:nrow(cc874), nrow(cc874)*(i/100))
  cc874_noise <-cc874
  cc874_noise$lux[idx] <- min(cc874$lux)

  location <- cc874_noise |>
    skytrackr::skytrackr()
})

sensitivity_default <- do.call(
  "rbind",
  sensitivity_default
  )

saveRDS(sensitivity_default,"inst/extdata/sensitivity_default.rds")

# considering the full series excluding true night
sensitivity_full <- lapply(0:75, function(i){
  idx <- sample(1:nrow(cc874), nrow(cc874)*(i/100))
  cc874_noise <-cc874
  cc874_noise$lux[idx] <- min(cc874$lux)
  location <- cc874_noise |>
    skytrackr::skytrackr(
      range = c(0.32, 100000)
    )
})

sensitivity_full <- do.call(
  "rbind",
  sensitivity_full
  )

saveRDS(sensitivity_full, "inst/extdata/sensitivity_full.rds")

# nitghttime pollution
sensitivity_night <- lapply(0:75, function(i){
  idx <- sample(1:nrow(cc874), nrow(cc874)*(i/100))
  cc874_noise <-cc874
  cc874_noise$lux[idx] <- 250
  location <- cc874_noise |>
    skytrackr::skytrackr()
})

sensitivity_night <- do.call(
  "rbind",
  sensitivity_night
)

saveRDS(sensitivity_night, "inst/extdata/sensitivity_night.rds")
