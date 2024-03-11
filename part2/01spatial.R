library(tidymodels)
tidymodels_prefer()
library(sf)
library(mapview)
library(ggplot2)
library(patchwork)
targets::tar_load(names = c(lp_supply_sf))
theme_set(theme_bw(base_size = 12))

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
ggsave("images/fold_cv.png", last_plot(), width = 12, height = 6, dpi = 300)


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

# autoplot(lpsp_folds_cluster$splits[[1]])

ggsave("images/spatial_cluster_cv.png", last_plot(), width = 12, height = 6, dpi = 300)

lpsp_folds_block$splits |>
  purrr::map(
    function(x) {
      ggplot() +
        geom_sf(data = analysis(x), color = "#3A5BA0", alpha = 0.5) +
        geom_sf(data = assessment(x), color = "#FFA500", alpha = 0.5)
    }
  ) |>
  patchwork::wrap_plots(ncol = 5)
ggsave("images/spatial_block_cv.png", last_plot(), width = 12, height = 6, dpi = 300)

