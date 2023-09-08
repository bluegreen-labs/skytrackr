# load libraries and functions
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(sf)
library(patchwork)
library(BayesianTools)
library(rnaturalearth)
source("R/likelihood.R")
source("R/tk_read_lux.R")
source("R/tk_track.R")

#data <- tk_read_lux("~/Dropbox/Research_Projects/code_repository/bitbucket/apus_lunar_synchrony/data-raw/geolocators/Gent_Voorhaven_A_apus/CC874_18Jun22_123407driftadj.lux")
data <- tk_read_lux("~/Downloads/CH762_11Jun23_040200.lux")
data <- data |>
  filter(
    date >= "2021-08-20"
  )

fit_track <- function(
    data,
    start_position,
    iterations = 16000,
    floor = 0.32,
    ceiling = 150,
    bbox = c(-20, -40, 60, 55),
    buffer = 1,
    tolerance = 1500 * 1000
  ) {

  # preprocess data
  data <- data |>
    filter(
      lux > floor,
      lux < ceiling
    ) |>
    mutate(
      lux = log(lux)
    )

  # unique dates
  dates <- unique(data$date)

  # empty data frame
  locations <- data.frame()

  maps::map(
    xlim = bbox[c(1,3)],
    ylim = bbox[c(2,4)]
  )

  for (i in seq_len(length(dates))) {

     # create daily subset
     subs <- data |>
       filter(
         date == dates[i]
       )

     out <- fit_parameters(
       data = subs,
       iterations = iterations,
       bbox = bbox
       #mask = mask
     )

     # set date
     out$date <- dates[i]

     locations <- rbind(locations, out)
     print(locations)

     points(
       locations[,1:2],
       pch = 19,
       col = 'red'
       )
  }

  return(locations)
}

test <- fit_track(
 data,
 start_position = data.frame(
   latitude = 51.08,
   longitude = 3.73
 )
)

#saveRDS(test, "data/track_demo.rds", compress = "xz")
