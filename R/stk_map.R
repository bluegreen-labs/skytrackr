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

stk_map <- function(df, buffer, bbox) {
   p <- ggplot2::ggplot(df) +
    ggplot2::geom_sf(
      data = stk_mask(buffer = buffer)
      ) +
    ggplot2::geom_sf(
      data = stk_mask(buffer = 0)
      ) +
     ggplot2::labs(
       x = "",
       y = ""
     )

   if(nrow(df) > 1) {
     p <- p +
       ggplot2::geom_path(
         ggplot2::aes(
           "longitude",
           "latitude"
         )
       )
   }

   p <- p +
    ggplot2::geom_point(
      ggplot2::aes(
        "longitude",
        "latitude"
      )
    ) +
    ggplot2::coord_sf(
      xlim = c(bbox[1], bbox[3]),
      ylim = c(bbox[2], bbox[4])
    ) +
    ggplot2::theme_bw()
}
