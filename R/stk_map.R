#' Plot skytrackr results
#'
#' Create a map of estimated locations as a static or dynamic map.
#'
#' @param df A data frame with locations produced with the skytrackr() function
#' @param bbox A geographic bounding box provided as a vector with the format
#'  xmin, ymin, xmax, ymax.
#' @param start_location A start location as lat/lon to indicate
#'  the starting position of the track (optional)
#' @param roi A region of interest under consideration, only used in
#'  plots during optimization
#' @param dynamic Option to create a dynamic interactive graph rather than
#'  a static plot. Both the path as the locations are shown. The size
#'  of the points is proportional to the latitudinal uncertainty, while
#'  equinox windows are marked with red points. (default = FALSE)

#' @importFrom rlang .data

#' @return A ggplot map of tracked locations or mapview dynamic overview.
#' @export
#' @examples
#' \dontrun{
#'
#' #----- estimated locations on demo data ----
#' # define land mask with a bounding box
#' # and an off-shore buffer (in km), in addition
#' # you can specify the resolution of the resulting raster
#' mask <- stk_mask(
#'   bbox  =  c(-20, -40, 60, 60), #xmin, ymin, xmax, ymax
#'   buffer = 150, # in km
#'   resolution = 0.5 # map grid in degrees
#'   )
#'
#'   # define a step selection distribution/function
#'   ssf <- function(x, shape = 0.9, scale = 100, tolerance = 1500){
#'   norm <- sum(stats::dgamma(1:tolerance, shape = shape, scale = scale))
#'   prob <- stats::dgamma(x, shape = shape, scale = scale) / norm
#' }
#'
#' # estimate locations with default values
#' # and plot progress
#' locations <- cc876 |> skytrackr(plot = FALSE)
#'
#' #----- actual plotting routines ----
#' # static plot, with required bounding box
#' locations |> stk_map(bbox = c(-20, -40, 60, 60))
#'
#' # dynamic plot
#' locations |> stk_map(dynamic = TRUE)
#' }

stk_map <- function(
    df,
    bbox,
    start_location,
    roi,
    dynamic = FALSE
    ) {

   # show dynamic plot
   if(dynamic){

      df <- df |>
         dplyr::mutate(
            uncertainty = .data$latitude_qt_95 - .data$latitude_qt_5
         )

      points <- df |>
         sf::st_as_sf(
            coords = c("longitude","latitude"),
            crs = 4326
         )

      path <-  points |>
         dplyr::group_by(.data$logger) |>
         dplyr::summarise(do_union = FALSE) |>
         sf::st_cast("MULTILINESTRING")

      m <- mapview::mapview(
         path,
         map.types = "Esri.WorldImagery"
      )

      m <- m + mapview::mapview(
         points,
         popup = TRUE,
         zcol = "equinox",
         col.regions = c("grey", "white"),
         col = c("grey", "white"),
         alpha.regions = 0.8,
         cex = "uncertainty",
         label = NA
      )

      print(m)
      return(invisible())
   }

   # convert to sf
   path <-  df |>
            sf::st_as_sf(coords = c("longitude", "latitude")) |>
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
            lty = 3,
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
         x = .data$sky_conditions_qt_50
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
