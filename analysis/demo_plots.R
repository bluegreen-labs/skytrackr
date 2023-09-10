# load libraries and functions
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(patchwork)
library(sf)
countries <- ne_countries(returnclass = "sf")

smc <- readRDS("data-raw/gent_locations_SMC.rds")
dezs <- readRDS("data-raw/gent_locations_DEzs.rds")

p_smc <- ggplot(smc) +
  geom_sf(data = countries) +
  geom_path(
    aes(
      longitude,
      latitude,
      colour = date
    )
  ) +
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
  facet_wrap(~logger)

p_dezs <- ggplot(dezs) +
  geom_sf(data = countries) +
  geom_path(
    aes(
      longitude,
      latitude,
      colour = date
    )
  ) +
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
  facet_wrap(~logger)


final <- (p_smc + p_dezs +
  plot_layout(
    ncol = 2,
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
    legend.justification = "left",
    legend.title = element_text(
      family = "Montserrat",
      color = "grey30", size = 13
    )
  )
)

ggsave(
  plot = final,
  filename = "./smc_dezs_comparison.png",
  width = 11,
  dpi = 150
  )
