# load libraries and functions
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(skytrackr)

# files <- list.files(
#   "~/Dropbox/Research_Projects/code_repository/bitbucket/apus_lunar_synchrony/data-raw/geolocators/Gent_Voorhaven_A_apus/",
#   glob2rx("*.lux"),
#   full.names = TRUE
# )
#
# files <- files[!grepl("drift", files)]
# data <- lapply(files, function(file){
#   stk_read_lux(file)
# })
#
# data <- do.call("rbind", data)
#
# # data <- data |>
# #   dplyr::filter(
# #     logger == "CC874"
# #   )
#
# options("dplyr.show_progress" = FALSE)
# # batch processing via pipe
# locations <- data |>
#   group_by(logger) |>
#   do({
#     skytrackr(
#       .,
#       bbox = c(-20, -40, 60, 55),
#       plot = FALSE
#     )
#   })
#
# saveRDS(locations, "data/gent_locations.rds", compress = "xz")

countries <- ne_countries(returnclass = "sf")
df <- locations |>
  filter(
    grb < 1.1,
    abs(latitude_ci_95 - latitude_ci_5) < 15
  )

p <- ggplot(df) +
  geom_sf(data = countries) +
  geom_path(
    aes(
      longitude,
      latitude
    )
  ) +
  coord_sf(
    xlim = c(-20, 60),
    ylim = c(-40, 55)
  ) +
  facet_wrap(~logger)

plot(p)
