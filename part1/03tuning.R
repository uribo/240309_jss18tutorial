# source("part1/02evaluation.R")
# ハイパーパラメータ
# tune
rf_spec <-
  rand_forest(min_n = tune(),
              trees = tune(),
              mode = "classification") |>
  set_engine("randomForest")

rf_wflow <-
  workflow(gas ~ ., rf_spec)
rf_wflow

set.seed(123)
rf_res <-
  tune_grid(
    rf_wflow,
    lp_folds,
    grid = 5
  )
# ref) fit_resamples()

show_best(rf_res, metric = "roc_auc")

best_parameter <-
  select_best(rf_res, metric = "roc_auc")
best_parameter

rf_wflow <-
  finalize_workflow(rf_wflow, best_parameter)
final_fit <-
  last_fit(rf_wflow, lp_split)
collect_metrics(final_fit)
