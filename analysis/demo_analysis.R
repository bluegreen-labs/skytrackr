# load libraries and functions
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(BayesianTools)
library(rnaturalearth)
source("R/likelihood.R")
source("R/tk_read_lux.R")
source("R/tk_track.R")

speed <- 15
step <- 1

# scope
# buffer around prevoius location
circ <- sf::st_as_sf(
  x = data.frame(
    lat = 40,
    lon = 10
  ),
  coords = c("lat","lon"),
  crs = "epsg:4326"
) |>
  sf::st_buffer(
    dist = speed * 60 * 60 * 24 * step
  )

# set parameter ranges
# lat/lon
lower <- c(-33, -30)
upper <- c(55, 60)

raw_data <- tk_read_lux("~/Downloads/CH762_11Jun23_040200.lux")

out <- raw_data |>
  as_tibble() |>
  filter(
    date == "2022-12-20",
    #date < "2022-12-15",
    lux > 0.32
  ) |>
  mutate(
    lux = log(lux)
  ) |>
  filter(
    lux < 6
  ) |>
  group_by(date) |>
  do({
    fit_parameters(
      .,
      iterations = 2000
      )
  })

print(out)

leaflet::leaflet() |>
  leaflet::addProviderTiles(
    leaflet::providers$Esri.WorldImagery,
    group = "World Imagery"
    ) |>
  leaflet::addCircleMarkers(
    data = out,
    lng = ~longitude,
    lat = ~latitude,
    color = "red",
    popup = ~htmltools::htmlEscape(date)
  )

