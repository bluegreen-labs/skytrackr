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

  location_mask <- skytrackr::cc874 |>
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
