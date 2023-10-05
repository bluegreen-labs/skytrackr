#---- test functions ----

test_that("test functions without task ids", {

  location <- skytrackr::cc874 |>
    skytrackr::skytrackr(
      iterations = 10,
      particles = 3,
      start_location = c(51.08, 3.73),
      tolerance = 11,
      plot = FALSE
    )

  expect_type(location, "list")

  # no start
  location <- skytrackr::cc874 |>
    skytrackr::skytrackr(
      iterations = 10,
      particles = 3,
      tolerance = 11,
      plot = FALSE
    )

  expect_type(location, "list")

  location_mask <- skytrackr::stk_read_lux(
    system.file("extdata/cc874.lux", package="skytrackr")) |>
    skytrackr::skytrackr(
      iterations = 10,
      particles = 3,
      start_location = c(51.08, 3.73),
      tolerance = 11,
      land_mask = TRUE,
      plot = TRUE
    )
  expect_type(location_mask, "list")
})

# remove plot
file.remove("Rplots.pdf")

test_that("test clustering + plots", {

  df <- skytrackr::stk_read_lux(
    system.file("extdata/cc874.lux", package="skytrackr")
    )

  cl <- stk_cluster(df, k = 2, center = TRUE)
  p <- stk_profile(cl)
  p_night <- stk_profile(cl, center = "night")

  expect_type(cl, "list")
  expect_type(p, "list")
  expect_type(p_night, "list")
})
