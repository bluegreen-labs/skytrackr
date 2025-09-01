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

df1 <- stk_read_lux("data-raw/CC874_18Jun22_123407.lux")
df1 <-df1 |>
  stk_screen_twl(filter = TRUE)
  # dplyr::filter(
  #   (date >= "2021-07-11" & date <= "2022-05-13")
  # )

df2 <- stk_read_lux("data-raw/CC876_22Jun22_161546.lux")

p <- df2 |> stk_profile()
ggsave("profile_plot.png")

df2 <-df2 |>
  stk_screen_twl(filter = TRUE)
  # dplyr::filter(
  #   (date >= "2021-08-02" & date <= "2022-05-09")
  # )

p <- df2 |> stk_profile()
ggsave("profile_plot_trimmed.png")

df <- bind_rows(df1, df2)

#---- DEzs MCMC approach ----

bbox <- c(-20, -40, 60, 60)

# define land mask
mask <- stk_mask(
  bbox  =  bbox,
  buffer = 150, # in km
  resolution = 1
)

# define a step selection distribution
ssf <- function(x, shape = 1.02, scale = 250, tolerance = 1500){
  # normalize over expected range with km increments
  norm <- sum(stats::dgamma(1:tolerance, shape = shape, scale = scale))
  prob <- stats::dgamma(x, shape = shape, scale = scale) / norm
}

locations <- df |>
  group_by(logger) |>
  do({
    skytrackr(
      .,
      mask = mask,
      plot = FALSE,
      start_location = c(51.08, 3.73),
      tolerance = 1500, # in km
      scale = c(1,20),
      range = c(0.1, 140),
      control = list(
        sampler = 'DEzs',
        settings = list(
          burnin = 500,
          iterations = 3000,
          message = FALSE
        )
      ),
      step_selection = ssf
    )
  })

saveRDS(locations, "analysis/locations.rds", compress = "xz")
p <- locations |>
  #filter(!equinox) |>
  group_by(logger) |>
  do(p = {
    plot(stk_map(., bbox = bbox))
  })



