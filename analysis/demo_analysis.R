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

data <- tk_read_lux("~/Dropbox/Research_Projects/code_repository/bitbucket/apus_lunar_synchrony/data-raw/geolocators/Gent_Voorhaven_A_apus/CC874_18Jun22_123407.lux")
#data <- tk_read_lux("~/Downloads/CH762_11Jun23_040200.lux")
data <- data |>
  filter(
    date >= "2021-08-20"
  )

test <- skytrack(
 data,
 start_position = data.frame(
   latitude = 51.08,
   longitude = 3.73
 ),
 bbox = c(-20, -40, 60, 55),
)

#saveRDS(test, "data/track_demo.rds", compress = "xz")
