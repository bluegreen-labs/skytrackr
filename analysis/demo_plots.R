# load libraries and functions
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(patchwork)
library(sf)
countries <- ne_countries(returnclass = "sf")

dezs <- readRDS("data-raw/gent_locations_DEzs_tolerance.rds") |>
  mutate(
    settings = "tolerance (11)"
  )
dezs_no <- readRDS("data-raw/gent_locations_DEzs_no_tolerance.rds") |>
  mutate(
    settings = "tolerance (not set)"
  )

dezs <-bind_rows(dezs, dezs_no)

smc <- readRDS("data-raw/gent_locations_SMC_tolerance.rds") |>
  mutate(
    settings = "tolerance (11)"
  )
smc_no <- readRDS("data-raw/gent_locations_SMC_no_tolerance.rds") |>
  mutate(
    settings = "tolerance (not set)"
  )

smc <-bind_rows(smc, smc_no)

p_smc <- ggplot(smc) +
  geom_sf(data = countries) +
  # geom_path(
  #   aes(
  #     longitude,
  #     latitude,
  #     colour = date
  #   )
  # ) +
  geom_segment(
    aes(
      x = longitude_qt_5,
      xend = longitude_qt_95,
      y = latitude,
      yend = latitude,
      #colour = site
    ),
    lwd = 0.8,
    alpha = 0.5
  ) +
  geom_segment(
    aes(
      x = longitude,
      xend = longitude,
      y = latitude_qt_5,
      yend = latitude_qt_95,
      #colour = site
    ),
    lwd = 0.8,
    alpha = 0.5
  ) +
  geom_point(
    aes(
      longitude,
      latitude,
      colour = date
    )
  ) +
  coord_sf(
    xlim = c(-20, 60),
    ylim = c(-40, 55)
  ) +
  theme_bw() +
  facet_wrap(settings~logger)

p_dezs <- ggplot(dezs) +
  geom_sf(data = countries) +
  # geom_path(
  #   aes(
  #     longitude,
  #     latitude,
  #     colour = date
  #   )
  # ) +
  geom_segment(
    aes(
      x = longitude_qt_5,
      xend = longitude_qt_95,
      y = latitude,
      yend = latitude,
      #colour = site
    ),
    lwd = 0.8,
    alpha = 0.5
  ) +
  geom_segment(
    aes(
      x = longitude,
      xend = longitude,
      y = latitude_qt_5,
      yend = latitude_qt_95,
      #colour = site
    ),
    lwd = 0.8,
    alpha = 0.5
  ) +
  geom_point(
    aes(
      longitude,
      latitude,
      colour = date
    )
  ) +
  coord_sf(
    xlim = c(-20, 60),
    ylim = c(-40, 55)
  ) +
  theme_bw() +
  facet_wrap(settings~logger)

final <- (p_smc + p_dezs +
  plot_layout(
    nrow = 2,
    guides = "collect"
  )  +
  plot_annotation(
    title = "",
    subtitle = "",
    caption = "",
    tag_levels = "a",
    tag_suffix = ")    ",
    tag_prefix = "("
  ) &
  theme_bw() +
  theme(
    panel.spacing.y = unit(0.01, "in"),
    legend.box.margin = margin(t = 1),
    legend.position = "bottom",
    legend.justification = "left",
    legend.title = element_text(
      family = "Montserrat",
      color = "grey30", size = 13
    )
  )
)

plot(final)

ggsave(
  plot = final,
  filename = "./smc_dezs_comparison.png",
  width = 10,
  height = 13,
  dpi = 150
  )
