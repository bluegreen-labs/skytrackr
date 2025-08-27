#' Plot skytrackr results
#'
#' Create a map of estimated locations
#'
#' @param df a data frame with locations produced by skytrackr()
#' @param buffer a land mask buffer value
#' @param bbox a bounding box
#'
#' @return a ggplot map of tracked locations
#' @export

stk_map <- function(
    df,
    buffer,
    bbox,
    start_location,
    roi
    ) {

   # convert to sf
   path <- st_as_sf(df, coords = c("longitude", "latitude")) |>
      sf::st_set_crs("EPSG:4326") |>
      sf::st_combine() |>
      sf::st_cast("LINESTRING") |>
      st_transform(crs = "+proj=eqearth")

   points <- st_as_sf(df, coords = c("longitude", "latitude")) |>
      sf::st_set_crs("EPSG:4326") |>
      sf::st_cast("POINT") |>
      sf::st_transform(crs = "+proj=eqearth")

   # mask polygon / crop to broad bounding box
   # region of interest
   m <- stk_mask(buffer = 0, bbox = bbox, sf = TRUE)

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
            panel.border = element_blank()
         )
   }

   if(!missing(roi)){

      # intersection of roi with mask
      roi <-   m |> st_intersection(
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
           panel.border = element_blank()
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
            aes(
               shape = equinox
            ),
            colour = "grey25"
         ) +
         ggplot2::scale_shape_manual(
            values = c(19, 1)
         ) +
         ggplot2::geom_sf(
            data = points |> filter(date == date[nrow(points)]),
            colour = "black",
            pch = 1,
            size = 4
         )
   }

   if(!missing(start_location)){
      start <- st_as_sf(
         data.frame(
            latitude = start_location[1],
            longitude = start_location[2]
         ),
         coords = c("longitude", "latitude")
      ) |>
         sf::st_set_crs("EPSG:4326") |>
         sf::st_cast("POINT") |>
         st_transform(crs = "+proj=eqearth")

      p <- p +ggplot2::geom_sf(
         data = start,
         colour = "black",
         pch = 17,
         size = 3
      )
   }

   p <- p +
      ggplot2::labs(
         title = sprintf(
            "%s (%s - %s)",
            df$logger[1],
            df$date[1],
            df$date[nrow(df)]
         )
      )

   p_lat <- ggplot2::ggplot(df) +
     ggplot2::geom_ribbon(
       ggplot2::aes(
         y = date,
         xmin = latitude_qt_5,
         xmax = latitude_qt_95
       ),
       fill = "grey85"
     ) +
     ggplot2::geom_path(
       ggplot2::aes(
         y = date,
         x = latitude_qt_50
       )
     )  +
      ggplot2::labs(
         x = "latitude"
      ) +
     ggplot2::theme_bw()

   p_lon <- ggplot2::ggplot(df) +
     ggplot2::geom_ribbon(
       ggplot2::aes(
         y = date,
         xmin = longitude_qt_5,
         xmax = longitude_qt_95
       ),
       fill = "grey85"
     ) +
     ggplot2::geom_path(
       ggplot2::aes(
         y = date,
         x = longitude_qt_50
       )
     )  +
      ggplot2::labs(
         x = "longitude"
      ) +
     ggplot2::theme_bw()

   p_sky <- ggplot2::ggplot(df) +
     ggplot2::geom_path(
       ggplot2::aes(
         y = date,
         x = sky_conditions
       )
     ) +
     ggplot2::labs(
        x = "sky conditions"
     ) +
     ggplot2::theme_bw()

   p_final <- p + p_lat + p_lon + p_sky +
     patchwork::plot_layout(
       ncol = 4,
       widths = c(4, 1, 1, 1)
     )

   return(p_final)
}
