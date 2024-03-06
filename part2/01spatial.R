library(tidymodels)
tidymodels_prefer()
library(sf)
library(mapview)
library(ggplot2)
library(patchwork)
library(spatialsample)
source(here::here("data-raw/lp.R"))

class(landprice)
landprice$geometry[1]

mapview(landprice)
mapview(landprice,
        zcol = "gas",
        alpha.regions = 0.3,
        cex = 2.6,
        lwd = 0.4,
        legend = FALSE)

# landprice |>
#   filter(price > 1000000) |>
#   mapview(zcol = "price")

landprice <-
  landprice |>
  select(names(lp_supply)) |>
  mutate(gas = as.factor(gas)) |>
  filter(!is.na(fire))

# Normal sample -----------------------------------------------------------
# ref) part1/01first_step.R
set.seed(123)
lp_split <-
  initial_split(landprice, prop = 0.8, strata = gas)
lp_train <- training(lp_split)
lp_test <- testing(lp_split)
set.seed(123)
lp_folds <-
  vfold_cv(lp_train, v = 10, strata = gas)

mapview(analysis(lp_folds$splits[[1]]), col.regions = "#3A5BA0") +
  mapview(assessment(lp_folds$splits[[1]]), col.regions = "#FFA500")

lp_folds$splits |>
  purrr::map(
    function(x) {
      ggplot() +
        geom_sf(data = analysis(x), color = "#3A5BA0", alpha = 0.5) +
        geom_sf(data = assessment(x), color = "#FFA500", alpha = 0.5)
    }
  ) |>
  patchwork::wrap_plots(ncol = 5)

# Spatial sample ----------------------------------------------------------
set.seed(123)
folds <-
  spatial_clustering_cv(lp_train, v = 10)
autoplot(folds)

folds$splits |>
  purrr::map(
    function(x) {
      ggplot() +
        geom_sf(data = analysis(x), color = "#3A5BA0", alpha = 0.5) +
        geom_sf(data = assessment(x), color = "#FFA500", alpha = 0.5)
    }
  ) |>
  patchwork::wrap_plots(ncol = 5)

set.seed(123)
folds <-
  spatial_block_cv(lp_train, v = 10)
autoplot(folds$splits[[1]])
