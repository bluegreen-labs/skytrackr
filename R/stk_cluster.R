#' Cluster geolocator co-variates
#'
#' Uses k-means and hierarchical clustering to group geolocator
#' covariates into consistent groups for visual analysis
#'
#' @param df A skytrackr data frame.
#' @param k The number of k-means/hierarchical clusters to consider.
#' @param method The method to use, "kmeans" (default), "hclust" can be set.
#'
#' @return The original data frame with attached cluster labels.
#' @export

stk_cluster <- function(
    df,
    k = 2,
    method = "kmeans"
    ) {

  # set seed for reproducibility
  set.seed(10)

  # convert from long to wide format
  df_wide <- df |>
    dplyr::filter(
      .data$measurement != "lux"
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

  # center values as based on distance
  # with widely different absolute values
  #df_wide <- apply(df_wide, 2, scale)

  if (method == "kmeans") {
    # calculate kmeans clustering
    # output
    output <- data.frame(
      date = dates,
      cluster = as.double(
        stats::kmeans(
          df_wide,
          centers = k,
          nstart = 10
        )$cluster
      )
    )
  } else {

   # calculate cluster tree
   cl <- stats::hclust(stats::dist(df_wide))

   # format output
   output <- data.frame(
       date = dates,
       cluster =
         stats::cutree(
           cl,
           k = k
         )
     )
  }

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

  tmp <- dplyr::left_join(df_time, output, by = "date") |>
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
