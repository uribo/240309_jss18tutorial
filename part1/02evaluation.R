# source(here::here("part1/01first_step.R"))
# yardstick ---------------------------------------------------------------
augment(tree_fit, new_data = lp_train) |>
  relocate(gas, .pred_class, .pred_TRUE, .pred_FALSE)
# confusion matrix
augment(tree_fit, new_data = lp_train) |>
  # select(gas, .pred_class) |>
  conf_mat(truth = gas, estimate = .pred_class) |>
  autoplot("heatmap")
# accurary
# データが不均衡である点に注意
augment(tree_fit, new_data = lp_train) |>
  accuracy(truth = gas, estimate = .pred_class)
augment(tree_fit, new_data = lp_train) |>
  sensitivity(truth = gas, estimate = .pred_class)
augment(tree_fit, new_data = lp_train) |>
  specificity(truth = gas, estimate = .pred_class)

lp_metrics <- metric_set(accuracy, sensitivity, specificity)
class(lp_metrics)
augment(tree_fit, new_data = lp_train) |>
  lp_metrics(truth = gas, estimate = .pred_class)
augment(tree_fit, new_data = lp_train) |>
  group_by(fire) |>
  lp_metrics(truth = gas, estimate = .pred_class)

# ROC curve
augment(tree_fit, new_data = lp_train) |>
  roc_auc(truth = gas, .pred_FALSE)
augment(tree_fit, new_data = lp_train) |>
  roc_curve(truth = gas, .pred_FALSE) |>
  autoplot()

# 過学習への注意
tree_fit |>
  augment(new_data = lp_train) |>
  accuracy(truth = gas, estimate = .pred_class) # resubstitution estimate
tree_fit |>
  augment(new_data = lp_test) |>
  accuracy(truth = gas, estimate = .pred_class) # generalization estimate
# テストデータの利用は、モデルの評価のためにのみ利用すること

tree_fit |>
  augment(new_data = lp_train) |>
  brier_class(truth = gas, .pred_FALSE)
tree_fit |>
  augment(new_data = lp_test) |>
  brier_class(truth = gas, .pred_FALSE)

# 交差検証法の導入
lp_folds <-
  vfold_cv(lp_train, v = 10, strata = gas)
lp_folds$splits[1:3]

lp_res <-
  fit_resamples(lp_wflow, lp_folds)
lp_res
lp_res |>
  collect_metrics() |>
  select(.metric, mean, n)

# 性能評価
ctrl_lp <-
  control_resamples(save_pred = TRUE)
lp_res <-
  fit_resamples(lp_wflow, lp_folds, control = ctrl_lp)
lp_preds <-
  lp_res |>
  collect_predictions()
lp_preds
lp_preds |>
  group_by(id) |>
  lp_metrics(truth = gas, estimate = .pred_class)
# rm(lp_res)

# ランダムフォレスト
rf_spec <-
  rand_forest(trees = 1000, mode = "classification") |>
  set_engine("ranger")
rf_wflow <-
  workflow(gas ~ ., rf_spec)
rf_res <-
  fit_resamples(rf_wflow, lp_folds, control = ctrl_lp)
collect_metrics(rf_res)
final_fit <-
  last_fit(rf_wflow, lp_split)
final_fit
collect_metrics(final_fit)
extract_workflow(final_fit)

collect_predictions(final_fit) |>
  group_by(id) |>
  lp_metrics(truth = gas, estimate = .pred_class)
