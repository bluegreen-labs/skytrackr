rm(list = ls())
# load libraries and functions
options("dplyr.show_progress" = FALSE)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(BayesianTools)
library(sf)
library(terra)
library(stars)
library(patchwork)
lapply(list.files("R/","*.R", full.names = TRUE), source)
set.seed(1)

#library(skytrackr)

# df1 <- stk_read_lux("data-raw/CC874_18Jun22_123407.lux")
# df1 <-df1 |>
#   stk_screen_twl(filter = TRUE)
#
df2 <- stk_read_lux("data-raw/CC876_22Jun22_161546.lux")

# p <- df2 |> stk_profile()
# ggsave("profile_plot.png")
#
df2 <- df2 |>
  stk_screen_twl(filter = TRUE) |>
    filter(
      date >= "2021-08-01" & date <= "2022-05-15"
    )

# p <- df2 |> stk_profile()
# ggsave("profile_plot_trimmed.png")
#
# df <- bind_rows(df1, df2)

#df <- df1
df <- df2

# BADEN
# df <- stk_read_glf("inst/extdata/24MP_20200813.glf")
# df <- df |>
#   filter(
#     date >= "2019-08-31" & date <= "2020-04-15"
#   )

# PIRASALI
# df <- stk_read_glf("inst/extdata/22LE_20200218.glf")
#
# df <- df |>
#   filter(
#     date >= "2018-10-10" & date <= "2019-02-27"
#   )

# test <- df |> filter(date == "2019-09-01")
#
# test$latitude <- 47.5
# test$longitude <- 8.25
#
# test <- bind_cols(test, skylight::skylight(
#   latitude = test$latitude,
#   longitude = test$longitude,
#   date = test$date_time,
#   sky_condition = 0.01
#   )
# ) |>
#   filter(
#     value > 0
#   )
#
#
# ggplot(test) +
#   geom_point(
#     aes(date_time, log(value))
#   ) +
#   geom_point(
#    aes(date_time, log(total_illuminance)),
#    col = "red"
#   )


#---- DEzs MCMC approach ----

bbox <- c(-20, -40, 60, 60)

# define land mask
mask <- stk_mask(
  bbox  =  bbox,
  buffer = 150, # in km
  resolution = 0.5 # in degrees
)

tol <- 1500

# define land mask with a bounding box
# and an off-shore buffer (in km), in addition
# you can specifiy the resolution of the resulting raster
mask <- stk_mask(
  bbox  =  c(-20, -40, 60, 60), #xmin, ymin, xmax, ymax
  buffer = 150, # in km
  resolution = 0.5 # map grid in degrees
)

# define a step selection distribution
ssf <- function(x, shape = 0.9, scale = 100, tolerance = tol){
  # normalize over expected range with km increments
  norm <- sum(stats::dgamma(1:tolerance, shape = shape, scale = scale))
  prob <- stats::dgamma(x, shape = shape, scale = scale) / norm
  return(prob)
}

locations <- df |>
  group_by(logger) |>
  do({
    skytrackr(
      .,
      mask = mask,
      plot = FALSE,
      #start_location = c(47.5, 8.25), # Baden - glf file
      #start_location = c(36.33, 30.5), # Pirasali - glf file
      start_location = c(51.08, 3.73), # Gent - lux file
      tolerance = tol, # in km
      scale = log(c(0.00001, 50)),
      range = c(0.09, 148),
      #range = c(8, 9000),
      control = list(
        sampler = 'DEzs',
        settings = list(
          burnin = 250,
          iterations = 3000,
          message = FALSE
        )
      ),
      step_selection = ssf
    )
  })
