#' Cluster geolocator data
#'
#' Uses k-means clustering to group geolocator
#' covariates into consistent groups
#'
#' @param df a skytrackr data frame
#' @param centers number of k-means clusters to consider
#'
#' @return original data frame with attached cluster labels
#' @export

stk_cluster <- function(
    df,
    centers = 2
    ) {

  # convert from long to wide format
  df_wide <- df |>
    dplyr::filter(
      "measurement" != "lux"
    ) |>
    dplyr::select(
      "logger",
      "date",
      "hour",
      "measurement",
      "value"
    ) |>
    tidyr::pivot_wider(
      names_from = c("logger", "hour", "measurement"),
      values_from = "value"
    ) |>
    stats::na.omit()

  # split out date
  dates <- df_wide |>
    dplyr::select(
      "date"
    )

  # drop date
  df_wide <- df_wide |>
    dplyr::select(
      -"date"
    )

  # center values
  df_wide <- apply(df_wide, 2, scale)

  # calculate kmeans clustering
  # output
  output <- data.frame(
    date = dates,
    cluster = as.double(
      stats::kmeans(
        df_wide,
        centers = centers
        )$cluster
      )
  )

  # combine with original timing (add hour field)
  # and convert to long format
  tmp <- dplyr::left_join(df, output, by = "date") |>
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

  # return cluster object
  return(df)
}
