#' Plot skytrackr results
#'
#' Create a map of estimated locations
#'
#' @param df a data frame with locations produced by skytrackr()
#' @param bbox a bounding box
#' @param start_location start location as lat/lon to indicate
#'  the starting position of the track (optional)
#' @param roi region of interest under consideration, only used in
#'  dynamic plots during optimization (optional)
#'
#' @return a ggplot map of tracked locations
#' @export

stk_map <- function(
    df,
    bbox,
    start_location,
    roi
    ) {


   # convert to sf
   path <-  sf::st_as_sf(df, coords = c("longitude", "latitude")) |>
      sf::st_set_crs("EPSG:4326") |>
      sf::st_combine() |>
      sf::st_cast("LINESTRING") |>
      sf::st_transform(crs = "+proj=eqearth")

   points <- sf::st_as_sf(df, coords = c("longitude", "latitude")) |>
      sf::st_set_crs("EPSG:4326") |>
      sf::st_cast("POINT") |>
      sf::st_transform(crs = "+proj=eqearth")

   # mask polygon / crop to broad bounding box
   # region of interest
   m <- stk_mask(bbox = bbox, sf = TRUE)

   # transform to equal earth
   m <- m |>
      sf::st_transform(crs = "+proj=eqearth")

   # base plot call
   p <- ggplot2::ggplot(df)

   if(missing(roi)){
      p <- p +
         ggplot2::geom_sf(
            data = m,
            fill = "grey",
            inherit.aes = FALSE
         ) +
         ggplot2::theme_bw() +
         ggplot2::theme(
            legend.position = "bottom",
            panel.border = ggplot2::element_blank(),
            plot.margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0, unit = "pt")
         )
   }

   if(!missing(roi)){

      # intersection of roi with mask
      roi <-   m |>  sf::st_intersection(
         roi |> sf::st_transform(crs = "+proj=eqearth")
      )

     p <- p +
        ggplot2::geom_sf(
           data = m,
           fill = "grey"
        ) +
        ggplot2::geom_sf(
           data = roi,
           colour = NA,
           fill = "darkolivegreen1",
           lty = 2
        ) +
        ggplot2::geom_sf(
           data = m,
           fill = NA
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
           legend.position = "bottom",
           panel.border = ggplot2::element_blank()
        )
   }

   if(nrow(df) > 1) {

      p <- p +
         ggplot2::geom_sf(
            data = path,
            colour = "grey25",
            lty = 3
         ) +
         ggplot2::geom_sf(
            data = points,
            ggplot2::aes(
               shape = .data$equinox
            ),
            colour = "grey25"
         ) +
         ggplot2::scale_shape_manual(
            values = c(19, 1)
         ) +
         ggplot2::geom_sf(
            data = points |> dplyr::filter(date == date[nrow(points)]),
            colour = "black",
            pch = 1,
            size = 4
         )
   }

   if(!missing(start_location)){
      start <- sf::st_as_sf(
         data.frame(
            latitude = start_location[1],
            longitude = start_location[2]
         ),
         coords = c("longitude", "latitude")
      ) |>
         sf::st_set_crs("EPSG:4326") |>
         sf::st_cast("POINT") |>
         sf::st_transform(crs = "+proj=eqearth")

      p <- p +ggplot2::geom_sf(
         data = start,
         colour = "black",
         pch = 17,
         size = 3
      )
   }

   p_lat <- ggplot2::ggplot(df) +
     ggplot2::geom_ribbon(
       ggplot2::aes(
         y = .data$date,
         xmin = .data$latitude_qt_5,
         xmax = .data$latitude_qt_95
       ),
       fill = "grey85"
     ) +
     ggplot2::geom_path(
       ggplot2::aes(
         y = .data$date,
         x = .data$latitude_qt_50
       )
     )  +
      ggplot2::labs(
         x = "latitude"
      ) +
     ggplot2::theme_bw()

   p_lon <- ggplot2::ggplot(df) +
     ggplot2::geom_ribbon(
       ggplot2::aes(
         y = .data$date,
         xmin = .data$longitude_qt_5,
         xmax = .data$longitude_qt_95
       ),
       fill = "grey85"
     ) +
     ggplot2::geom_path(
       ggplot2::aes(
         y = .data$date,
         x = .data$longitude_qt_50
       )
     )  +
      ggplot2::labs(
         x = "longitude"
      ) +
     ggplot2::theme_bw()

   p_sky <- ggplot2::ggplot(df) +
      ggplot2::geom_ribbon(
         ggplot2::aes(
            y = .data$date,
            xmin = .data$sky_conditions_qt_5,
            xmax = .data$sky_conditions_qt_95
         ),
         fill = "grey85"
      ) +
     ggplot2::geom_path(
       ggplot2::aes(
         y = .data$date,
         x = .data$sky_conditions
       )
     ) +
     ggplot2::labs(
        x = "sky conditions"
     ) +
     ggplot2::theme_bw()

   p_final <- p + p_lat + p_lon + p_sky +
     patchwork::plot_layout(
       ncol = 4,
       widths = c(4, 1, 1, 1),
       axis_titles = "collect_y"
     ) +
    patchwork::plot_annotation(
       title = sprintf("%s (from %s to %s)",df$logger[1], df$date[1], df$date[-1])
    )

   return(p_final)
}
