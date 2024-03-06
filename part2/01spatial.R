library(tidymodels)
tidymodels_prefer()
library(sf)
library(mapview)
library(ggplot2)
library(patchwork)
targets::tar_load(names = c(lp_supply_sf))

class(lp_supply_sf)
lp_supply_sf$geometry[1]

mapview(lp_supply_sf)
mapview(lp_supply_sf,
        zcol = "gas",
        alpha.regions = 0.3,
        cex = 2.6,
        lwd = 0.4,
        legend = FALSE)

# lp_supply_sf |>
#   filter(price > 1000000) |>
#   mapview(zcol = "price")

# Normal sample -----------------------------------------------------------
# ref) part1/01first_step.R
targets::tar_load(lpsp_folds)

mapview(analysis(lpsp_folds$splits[[1]]), col.regions = "#3A5BA0") +
  mapview(assessment(lpsp_folds$splits[[1]]), col.regions = "#FFA500")

lpsp_folds$splits |>
  purrr::map(
    function(x) {
      ggplot() +
        geom_sf(data = analysis(x), color = "#3A5BA0", alpha = 0.5) +
        geom_sf(data = assessment(x), color = "#FFA500", alpha = 0.5)
    }
  ) |>
  patchwork::wrap_plots(ncol = 5)

# Spatial sample ----------------------------------------------------------
targets::tar_load(names = c(lpsp_folds_cluster, lpsp_folds_block))
autoplot(lpsp_folds_cluster)

lpsp_folds_cluster$splits |>
  purrr::map(
    function(x) {
      ggplot() +
        geom_sf(data = analysis(x), color = "#3A5BA0", alpha = 0.5) +
        geom_sf(data = assessment(x), color = "#FFA500", alpha = 0.5)
    }
  ) |>
  patchwork::wrap_plots(ncol = 5)

autoplot(lpsp_folds_cluster$splits[[1]])
