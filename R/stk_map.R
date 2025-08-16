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
    buffer = 0,
    bbox,
    start_location,
    roi
    ) {

   # solstices
   df$doy <- as.numeric(format(df$date, "%j"))
   df$solstice <- ifelse(
      (df$doy > 266 - 10 & df$doy < 266 + 10) |
         (df$doy > 80 - 10 & df$doy < 80 + 10),
      TRUE, FALSE)

   p <- ggplot2::ggplot(df) +
      ggplot2::geom_sf(
         data = stk_mask(buffer = buffer)
      ) +
      ggplot2::geom_sf(
         data = stk_mask(buffer = 0)
      ) +
      ggplot2::labs(
         title = sprintf(
            "%s (%s)",
            df$logger[1],
            df$date[nrow(df)]
         )
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
         legend.position = "bottom"
      )

   if(nrow(df) > 1) {
     p <- p +
       ggplot2::geom_path(
         ggplot2::aes(
           longitude,
           latitude,
           colour = as.factor(solstice)
         )
       )

   p <- p +
    ggplot2::geom_point(
      ggplot2::aes(
        longitude,
        latitude,
        colour = as.factor(solstice)
         )
      )
   }

   if(!missing(start_location)){
     p <- p +
        ggplot2::annotate(
           "point",
           start_location[2],
           start_location[1],
           colour = "red"
      )
   }

   if(!missing(roi)){
     p <- p +
       ggplot2::geom_sf(
         data = roi,
         fill = NA,
         colour = "black"
       ) +
       ggplot2::annotate(
         "point",
         df$longitude[nrow(df)],
         df$latitude[nrow(df)],
         colour = "black",
         pch = 1,
         size = 3
       ) +
        ggplot2::coord_sf(
           xlim = c(bbox[1], bbox[3]),
           ylim = c(bbox[2], bbox[4])
        )
   } else {
      p <-p +
         ggplot2::coord_sf(
            xlim = c(bbox[1], bbox[3]),
            ylim = c(bbox[2], bbox[4])
         )
   }

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
         y = rev(date),
         xmin = rev(longitude_qt_5),
         xmax = rev(longitude_qt_95)
       ),
       fill = "grey85"
     ) +
     ggplot2::geom_path(
       ggplot2::aes(
         y = rev(date),
         x = rev(longitude_qt_50)
       )
     )  +
      ggplot2::labs(
         x = "longitude"
      ) +
     ggplot2::theme_bw()

   p_sky <- ggplot2::ggplot(df) +
     ggplot2::geom_path(
       ggplot2::aes(
         y = rev(date),
         x = rev(sky_conditions)
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
