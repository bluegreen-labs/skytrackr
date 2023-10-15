# test custom priors
library(BayesianTools)

density = function(par){
  d1 = dunif(par[1], -2,6, log =TRUE)
  d2 = dnorm(par[2], mean= 2, sd = 3, log =TRUE)
  return(d1 + d2)
}

sf::sf_use_s2(FALSE)
land <- readRDS(system.file("extdata/mask.rds", package="skytrackr"))

l <- land |>
  st_geometry() |>
  st_union()

nc = st_read(system.file("shape/nc.shp", package="sf"))
sf::st_sample(nc, 30, type = "regular")

plot(land, col = "red")
plot(l, add = TRUE)

density = function(par){
  d1 = dunif(par[1], -2,6, log =TRUE)
  d2 = dnorm(par[2], mean= 2, sd = 3, log =TRUE)
  return(d1 + d2)
}

sampler = function(n=1){
  d1 = runif(n, -2,6)
  d2 = rnorm(n, mean= 20, sd = 3)
  return(cbind(d1,d2))
}

prior <- createPrior(
  density = density,
  sampler = sampler,
  lower = c(-10,-20),
  upper = c(10,20),
  best = NULL
  )

print(prior)
