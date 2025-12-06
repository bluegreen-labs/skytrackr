#' Cluster geolocator light levels
#'
#' Uses k-means to group geolocator light values
#' into consistent groups for further analysis
#'
#' @param df A skytrackr data frame
#' @param eps dbscan eps value, see `dbscan::dbscan()` (default = 0.1, condensed clusters)
#' @param plot plot the cluster results with respect to the diurnal pattern offset and (day)length values
#'
#' @return The original data frame with attached cluster labels.
#' @export

stk_cluster <- function(
    df,
    eps = 0.1,
    plot = TRUE
  ) {

  df_wide <- df |>
    stk_center() |>
    stk_filter(
      range = 1.5,
      filter = TRUE,
      verbose = FALSE
    ) |>
    dplyr::group_by(date) |>
    dplyr::summarize(
      date_num = as.numeric(date[1]),
      offset = offset[1],
      length =
        as.numeric(difftime(
          date_time[2],
          date_time[1],
          units = "hours")
        )
    )

  # split out date
  dates <- df_wide |>
    dplyr::select(
      "date"
    )

  # split out date
  df_wide <- df_wide |>
    dplyr::select(
      -"date",
    )

  # center values as based on distance
  # with widely different absolute values
  df_wide_scaled <- apply(df_wide, 2, scale)

  # dbscan clustering
  t <- dbscan::dbscan(
    df_wide_scaled,
    eps = eps,
    minPts = 3
  )$cluster

  # combine labels with dates
  output <- data.frame(
    date = dates,
    cluster = t
    )

  # combine with original timing (add hour field)
  # and convert to long format
  df_time <- df |>
    dplyr::filter(
     .data$measurement == "lux"
    ) |>
    dplyr::select(
      "logger",
      "date",
      "hour",
      "date_time"
    ) |>
    unique()

  tmp <- dplyr::left_join(
      df_time,
      output,
      by = "date"
    ) |>
    dplyr::select(
      "logger",
      "date",
      "hour",
      "cluster"
    ) |>
    tidyr::pivot_longer(
      cols = "cluster",
      names_to = "measurement",
      values_to = "value"
    )

  # merge with original data (add rows)
  df <- dplyr::bind_rows(df, tmp) |>
    dplyr::mutate(
      measurement = factor(
        .data$measurement,
        levels = sort(unique(.data$measurement))
      )
    )

  if(plot){
    p <- ggplot2::ggplot() +
      ggplot2::geom_point(
        data = df_wide,
        ggplot2::aes(
          offset,
          length,
          colour = as.factor(t)
        )
      ) +
      ggplot2::scale_colour_viridis_d(
        name = "cluster"
      ) +
      ggplot2::theme_minimal()
    print(p)
  }


  # return cluster object
  return(df)
}
