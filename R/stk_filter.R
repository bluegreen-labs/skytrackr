
#' Filter twilight values by range
#'
#' Filter out twilight values by range, and
#' returns the data frame with a twilight (TRUE/FALSE)
#' column or the data frame with only twilight values
#' selected (filtered out).
#'
#' Generally used for internal process, but can be
#' useful for visualizations of profiles as well.
#'
#' @param data a skytrackr compatible data frame
#' @param range a range c(min, max) of valid values in lux, or a single
#'  threshold value
#' @param smooth smooth the data using a hampel filter with a window size
#'  of 3, and a multiplier of the MAD of 3. Original values are substituted,
#'  the values replaced are flagged in an `outlier` column in the returned
#'  data frame (default = TRUE)
#' @param plot plot daily profiles with the range filter applied
#' @param filter if TRUE only twilight values are returned if
#'  FALSE the data frame is returned with an annotation column
#'  called 'twilight' for further processing.
#' @param verbose Give detailed feedback (TRUE or FALSE, default = TRUE)
#'
#' @returns a skytrackr compatible data frame, either filtered
#'  to only include twilight values selected by the range parameter
#'  or with an additional 'twilight' column to annotate these values.
#' @export
#' @examples
#'
#' # filter values using the preset range, only annotate
#' df <- cc876 |> stk_filter(range = c(1.5, 400))
#'
#' # filter values using the preset range, only retain filtered values
#' df <- cc876 |> stk_filter(range = c(1.5, 400), filter = TRUE)

stk_filter <- function(
    data,
    range,
    smooth = FALSE,
    plot = FALSE,
    filter = FALSE,
    verbose = TRUE
){

  if (verbose){

    cli::cli_div(
      theme = list(
        rule = list(
          color = "darkgrey",
          "line-type" = "single",
          "margin-bottom" = 1,
          "margin-top" = 1
        ),
        span.strong = list(color = "black"))
    )
    cli::cli_rule(
      left = "{.strong Filtering data}",
      #right = "{.pkg skytrackr v{packageVersion('skytrackr')}}",

    )
    cli::cli_end()
  }

  # only select the light data
  data <- data |>
    dplyr::filter(
      .data$measurement == "lux"
    )

  # range check
  if (range[1] < min(data$value, na.rm = TRUE)){
    range[1] <- min(data$value, na.rm = TRUE)
    if(verbose){
      cli::cli_bullets(c("!" = "Minimum range value out of range, set to {range[1]}"))
    }
  }

  # set twilight mode if necessary
  twilight <- FALSE
  if (length(range) == 1){
    range <- c(range, Inf)
    twilight <- TRUE
    if(verbose){
      cli::cli_bullets(c("!" = "No maximum range provided, switching to twilight mode!"))
    }
  }

  # Hampel value with a window of 3
  if (smooth){
    if(verbose){
      cli::cli_bullets(c(
        ">" = "Smoothing the data using a Hampel filter",
        "i" = "Outliers will be replaced with interpolated values."
      )
      )
    }

    # calculate metrics for the Hampel filter
    window <- 3
    median <- zoo::rollmedian(data$value, window, fill = NA)
    mad <- zoo::rollapply(
      data$value,
      window,
      mad,
      align = "center",
      fill = NA,
      na.rm = TRUE
    )

    # calculate outliers with MAD
    data$outlier <- ifelse(
      abs(data$value - median) > 2.5 * mad, TRUE, FALSE
    )

    # substitute original values with median
    data <- data |>
      dplyr::mutate(
        value = ifelse(.data$outlier, median, .data$value)
      )
  }

  # center the data but only add
  # offset columns and arrange on
  # the hour offset column (wrapping
  # the diurnal profile where required)
  data <- data |>
    stk_center() |>
    dplyr::group_by(.data$logger, .data$date) |>
    dplyr::arrange(.data$hour_centered)

  # filter data day by day on the centered profiles
  data <- data |>
    dplyr::group_by(.data$logger, .data$date) |>
    dplyr::do({

      # copy data
      df <- .data

      # select values in date range
      tmp_selected <- ifelse(
        df$value > range[1],
        TRUE,
        FALSE
      )

      # set first and last twilight
      first_twl <- suppressWarnings(min(which(tmp_selected)))
      last_twl <- suppressWarnings(max(which(tmp_selected)))

      selected <- rep(FALSE, nrow(df))

      # forward pass
      for(i in which(tmp_selected)){
        if(df$value[i] <= range[2]){
          selected[i] <- TRUE
        } else {
          break
        }
      }

      # backward pass
      for(i in rev(which(tmp_selected))){
        if(df$value[i] <= range[2]){
          selected[i] <- TRUE
        } else {
          break
        }
      }

      df$selected <- selected

      # check for twilight mode
      if(twilight){
        twl <- df |>
          dplyr::mutate(
            diff_sel = c(diff(.data$selected),0),
            .groups = "drop"
          )

        first_twl <- which(twl$diff_sel == 1) + 1
        last_twl <- which(twl$diff_sel == -1)

        df$selected <- FALSE
        df$selected[c(first_twl, last_twl)] <- TRUE
      }

      df
    })

  # plot
  if(plot){
    p1 <- data |>
      ggplot2::ggplot() +
      ggplot2::geom_point(
        ggplot2::aes(
          .data$hour,
          log(.data$value),
          colour = .data$selected
        ),
        na.rm = TRUE
      ) +
      ggplot2::labs(
        x = "hour",
        y = "log(lux)"
      ) +
      ggplot2::scale_color_manual(
        values = c("black","red")
      ) +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(~.data$logger)

    p2 <- data |>
      ggplot2::ggplot() +
      ggplot2::geom_tile(
        ggplot2::aes(
          .data$date,
          .data$hour,
          fill = log(.data$value)
        ),
        na.rm = TRUE
      ) +
      ggplot2::geom_contour(
        ggplot2::aes(
          .data$date,
          .data$hour,
          z = as.numeric(.data$selected)
        ),
        lwd = 0.3,
        colour = "white",
        na.rm = TRUE
      ) +
      ggplot2::scale_fill_viridis_c(
        na.value = NA
      ) +
      ggplot2::labs(
        x = "Date",
        y = "hour"
      ) +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(~.data$logger)

    plot(p1 / p2)
  }

  # only retain twilight values
  if (filter){
    data <- data |>
      dplyr::filter(.data$selected)
  }

  # ungroup data
  data <- data |>
    dplyr::ungroup()

  return(data)
}
