library(tidymodels)
tidymodels_prefer()
targets::tar_load(names = c(lp_metrics, rf_fit_rs_sp, lpsp_final_fit))
#################################
collect_metrics(rf_fit_rs_sp)
collect_metrics(lpsp_final_fit)
show_best(lpsp_final_fit, metric = "accuracy")

collect_predictions(rf_fit_rs_sp) |>
  group_by(id) |>
  lp_metrics(truth = gas, estimate = .pred_class)

targets::tar_load(rf_fit_rs_spcluster)
collect_metrics(rf_fit_rs_spcluster)
collect_predictions(rf_fit_rs_spcluster) |>
  group_by(id) |>
  lp_metrics(truth = gas, estimate = .pred_class)


# AOA ---------------------------------------------------------------------
targets::tar_load(names = c(lpsp_train, lpsp_test, lpsp_final_fit))
library(sf)
library(tidymodels)
library(waywiser)

# f <- formula(gas ~ price + above_floor + dist_from_st)
# lm_model <-
#   lm(f, data = lp_train |>
#        mutate(gas = as.numeric(gas)-1))
# lp_train |>
#   mutate(sewer = as.factor(sewer)) |>
#   recipe(f, data = _) |>
#   step_dummy(sewer) |>
#   prep() |>
#   bake(new_data = lp_train)

# targets::tar_load(names = c(lp_train, lp_test))
# waywiser::ww_area_of_applicability(
#   f,
#   data = lpsp_train |>
#     mutate(gas = as.numeric(gas)-1),
#   testing = lpsp_test |>
#     mutate(gas = as.numeric(gas)-1),
#   importance = vip::vi_model(lm_model),
#   na_rm = TRUE)

targets::tar_load(tree_fit)
targets::tar_load(names = c(tree_spec, rf_spec))
targets::tar_load(names = c(lp_train, lp_test))

library(vip)
pfun <- function(object, newdata) {
  predict(object, new_data = newdata)$.pred_class
}
# pfun(tree_fit, lp_train |>
#        mutate(gas = as.numeric(gas)-1))
targets::tar_load(rf_final_fit)

# targets::tar_load(rf_final_fit)
# xx2 <-
#   rf_final_fit |>
#   extract_fit_parsnip()
# xx2 |> vip::vi_model()
# pfun(xx2, lp_train |>
#        mutate(gas = as.numeric(gas)-1))

# pfun(rf_fit, lp_train |>
#        mutate(gas = as.numeric(gas)-1))
importance

# tree_fit |>
#   vip::vi_model()
# pfun(tree_fit, lp_train)
# Area-of-applicability thresholdが0に近いと?

targets::tar_load(lp_supply_aoa)
lp_aoa_pref36 <-
  cbind(
    lp_supply_sf_prf36 |>
    mutate(gas = as.numeric(gas)-1,
           sewer = as.numeric(sewer)),
  predict(aoa, lp_supply_sf_prf36 |>
            mutate(gas = as.numeric(gas)-1,
                   sewer = as.numeric(sewer))))


# plot(lp_aoa["aoa"])

# FALSEのポイントがモデルの予測範囲外
ggplot() +
  aes() +
  geom_sf(data = lp_supply_aoa |>
            filter(aoa == TRUE), color = "gray", alpha = 0.05) +
  geom_sf(data = lp_supply_aoa |>
            filter(aoa == FALSE), color = "#FFA500", alpha = 0.5)

# 多くのポイントを予測可能な範囲に含む

mapview::mapview(lp_aoa_pref36 |>
                   filter(aoa == FALSE), col.regions = "#FFA500") +
  mapview::mapview(lp_aoa_pref36 |>
                   filter(aoa == TRUE), col.regions = "gray")
mapview::mapview(lp_supply_shikoku_aoa |>
                   filter(aoa == FALSE), col.regions = "#FFA500") +
  mapview::mapview(lp_supply_shikoku_aoa |>
                     filter(aoa == TRUE), col.regions = "gray")

lp_aoa |>
  filter(aoa == FALSE) |>
  ggplot() +
  aes(x = price, y = dist_from_st, color = factor(gas)) +
  geom_point()

ww_area_of_applicability(f,
                         lp_train |>
                           mutate(gas = as.numeric(gas)-1,
                                  sewer = as.numeric(sewer),
                                  fire = as.numeric(as.factor(fire))),
                         lp_test |>
                           mutate(gas = as.numeric(gas)-1,
                                  sewer = as.numeric(sewer),
                                  fire = as.numeric(as.factor(fire))),
                         importance = importance)

importance <- vip::vi_permute(
  rf_final_fit |>
    extract_workflow(),
  target = "gas",
  metric = "accuracy",
  pred_wrapper = pfun,
  train = lp_train)
ww_area_of_applicability(gas ~ .,
                                lp_train |>
                                  select(c("gas", "price", "water", "sewer", "above_floor", "under_floor", "dist_from_st",
                                           "fire")) |>
                                  mutate(gas = as.numeric(gas)-1,
                                         water = as.numeric(water),
                                         sewer = as.numeric(sewer),
                                         fire = as.numeric(as.factor(fire))),
                                lp_test |>
                                  select(c("gas", "price", "water", "sewer", "above_floor", "under_floor", "dist_from_st",
                                           "fire")) |>
                                  mutate(gas = as.numeric(gas)-1,
                                         water = as.numeric(water),
                                         sewer = as.numeric(sewer),
                                         fire = as.numeric(as.factor(fire))),
                                importance = importance)


waywiser::ww_area_of_applicability(
  gas ~ .,
  data = lp_train |>
    mutate(gas = as.numeric(gas)-1) |>
    mutate(fire = as.numeric(as.factor(fire))),
  testing = lp_test |>
    mutate(gas = as.numeric(gas)-1) |>
    mutate(fire = as.numeric(as.factor(fire))),
  importance = vip::vi_model(tree_fit),
  na_rm = TRUE)
#
# lp_aoa <-
#   waywiser::ww_area_of_applicability(
#     gas ~ price + sewer + above_floor + under_floor + dist_from_st,
#     lpsp_train |>
#       mutate(gas = as.integer(gas)-1) |>
#       select(gas, price, sewer, above_floor, under_floor, dist_from_st),
#     lpsp_test |>
#       mutate(gas = as.integer(gas)-1) |>
#       select(gas, price, sewer, above_floor, under_floor, dist_from_st),
#     importance = lpsp_final_fit |>
#       extract_fit_engine() |>
#       vip::vi_model(),
#     na_rm = TRUE)


