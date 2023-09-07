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

sf_use_s2(FALSE)

data <- tk_read_lux("~/Downloads/CH762_11Jun23_040200.lux")
  # filter(
  #   date > "2022-10-15"
  # )

ggplot(
  data |>
    filter(
      lux > 0.32
    ) |>
    mutate(
      lux = log(lux)
    ) |>
    filter(
      lux < 6
    )
  ) +
  geom_tile(
    aes(
      as.Date(date),
      as.numeric(format(date_time, "%H")) + as.numeric(format(date_time, "%M"))/60,
      fill = lux
    )
  ) +
  scale_fill_viridis_c(
    option = "B"
  )

fit_track <- function(
    data,
    start_position, # start location
    iterations = 16000, # mcmc iterations
    floor = 0.32,
    ceiling = 400,
    bbox = c(-20, -33, 60, 55),
    tolerance = 1500 * 1000
  ) {

  # preprocess data
  data <- data |>
    filter(
      lux > floor
    ) |>
    mutate(
      lux = log(lux)
    ) |>
    filter(
      lux <= round(log(ceiling))
    )

  # unique dates
  dates <- unique(data$date)

  # empty data frame
  locations <- data.frame()

  for (i in seq_len(length(dates))) {
  #for (i in 1:100 ) {

     # create daily subset
     subs <- data |>
       filter(
         date == dates[i]
       )

     # if (i == 1) {
     #   previous_position <- start_position
     # } else {
     #     previous_position <- data.frame(
     #       latitude = locations$latitude[i-1],
     #       longitude = locations$longitude[i-1]
     # }

     out <- fit_parameters(
       data = subs,
       #previous_position = previous_position,
       iterations = iterations,
       bbox = bbox
     )

     # set date
     out$date <- dates[i]

    # if (i == 1){
    #   dist <- geosphere::distGeo(
    #     p1 = c(out$longitude, out$latitude),
    #     p2 = c(start_position$longitude, start_position$latitude)
    #   )
    #  } else {
    #   dist <- geosphere::distGeo(
    #     p1 = c(out$longitude, out$latitude),
    #     p2 = c(locations$longitude[i-1], locations$latitude[i-1])
    #   )
    #  }

    # set distance
    #out$dist <- dist

    # if (dist > tolerance) {
    #   if (i == 1) {
    #     out$longitude <- start_position$longitude
    #     out$latitude <- start_position$latitude
    #   } else {
    #     out$longitude <- locations$longitude[i-1]
    #     out$latitude <- locations$latitude[i-1]
    #   }
    #
    #   out$latitude_ci_5 <- NA
    #   out$latitude_ci_95 <- NA
    #   out$longitude_ci_5 <- NA
    #   out$longitude_ci_95 <- NA
    # }

     locations <- rbind(locations, out)
     print(locations)
     maps::map()
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

