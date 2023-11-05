# Particle filter explorations
library(skylight)
library(BayesianTools)
library(skytrackr)
library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)

# read in data for later
df <- stk_read_lux("data-raw/CC874_18Jun22_123407.lux") |>
  mutate(
    hour = as.numeric(format(date_time, "%H"))
  )

dates <- unique(df$date)
dates <- "2021-09-24"

#---- pre-processing of the sample grid ----

# create mask
mask <- stk_mask()

# expand regular grid
grid <- expand.grid(
  x = seq(-20,50,2),
  y = seq(-30,60,2)
)

# convert to sf
roi <-  sf::st_as_sf(
    grid,
    coords = c("x","y"),
    crs = "epsg:4326"
  )

coords <- roi |>
  sf::st_coordinates() |>
  as.data.frame() |>
  dplyr::rename(
    "lon" = "X",
    "lat" = "Y"
  )

#----- various scenarios ----

lapply(dates, function(set_date){

  # constraints as per default skytrackr settings
  df2 <- df |>
    dplyr::filter(
      measurement == "lux",
      date == set_date,
      #hour > hour[which(max(value) == value)],
      (value > 0.32 & value < 150)
    ) |>
    mutate(
      #value = value + rnorm(n(),mean=0, sd = 0.5),
      #value = max(0.33, value),
      value = log(value)
    )

  output_2 <- apply(coords, 1, function(coord){
    pred <- skylight(
      longitude = coord['lon'],
      latitude = coord['lat'],
      date = df2$date_time,
      sky_condition = 10
    )

    pred$total_illuminance[pred$total_illuminance < min(df$value)] <- min(df$value)

    obs <-df2$value
    data.frame(
      lon = coord['lon'],
      lat = coord['lat'],
      rmse = sqrt(mean((obs - log(pred$total_illuminance))^2))
    )
  }) |>
  bind_rows()

  #---- plotting stuff ---

  p <- ggplot() +
    geom_contour_filled(
      data = output_2,
      aes(
        x = lon,
        y = lat,
        z = rmse,
        fill = after_stat(level)
      )
    ) +
    geom_sf(
      data = mask,
      color = "white",
      fill = NA
    ) +
    coord_sf(
      xlim = c(-20, 50),
      ylim = c(-30, 60)
    ) +
    labs(
      title = set_date
    )

  print(p)
})
