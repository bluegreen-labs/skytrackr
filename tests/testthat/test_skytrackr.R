#---- test functions ----

bbox <- c(-20, -40, 60, 60)

# define land mask
mask <- stk_mask(
  bbox  =  bbox,
  buffer = 150, # in km
  resolution = 0.5
)

# define a step selection distribution
# define a step selection distribution
ssf <- function(x, shape = 1.06, scale = 250, tolerance = 1500){
  # normalize over expected range with km increments
  norm <- sum(stats::dgamma(1:tolerance, shape = shape, scale = scale))
  prob <- stats::dgamma(x, shape = shape, scale = scale) / norm
}

test_that("test helper functions", {
  m <- stk_mask(
    bbox  =  bbox,
    buffer = 150, # in km
    resolution = 0.5
  )

  expect_type(m, "S4")

  m <- stk_mask(
    bbox  =  bbox,
    sf = TRUE
  )

  expect_type(m, "list")
})

test_that("test optimizations", {

  location <- skytrackr::cc876 |>
    skytrackr(
      mask = mask,
      plot = FALSE,
      start_location = c(51.08, 3.73),
      tolerance = 1500, # in km
      scale = c(1,10),
      range = c(0.32, 10),
      control = list(
        sampler = 'DEzs',
        settings = list(
          iterations = 10,
          message = FALSE
        )
      ),
      step_selection = ssf
    )

  expect_type(location, "list")
})


test_that("test no start", {
  # no start
  expect_error(skytrackr::cc876 |>
    skytrackr::skytrackr(
      mask = mask,
      plot = FALSE,
      tolerance = 1500, # in km
      scale = c(1,10),
      range = c(0.32, 10),
      control = list(
        sampler = 'DEzs',
        settings = list(
          iterations = 10,
          message = FALSE
          )
        ),
      step_selection = ssf
    )
  )
})

test_that("read from file and optimize", {
  location <- skytrackr::stk_read_lux(
      system.file("extdata/cc876.lux", package="skytrackr")
    ) |>
    skytrackr::skytrackr(
      mask = mask,
      plot = TRUE,
      start_location = c(51.08, 3.73),
      tolerance = 1500, # in km
      scale = c(1,10),
      range = c(0.32, 10),
      control = list(
        sampler = 'DEzs',
        settings = list(
          iterations = 10,
          message = FALSE
        )
      ),
      step_selection = ssf
    )
  expect_type(location, "list")
})



test_that("test reading data", {
  df <- skytrackr::stk_read_lux(
    system.file("extdata/cc876.lux", package="skytrackr")
  )
  expect_s3_class(df, "data.frame")
})

test_that("test plots", {

  df <- skytrackr::stk_read_lux(
     system.file("extdata/cc876.lux", package="skytrackr")
     )

  p <- stk_profile(df)
  p_night <- stk_profile(df, center = "night")

  expect_s3_class(p, "ggplot")
  expect_s3_class(p_night, "ggplot")
})
