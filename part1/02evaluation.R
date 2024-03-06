library(tidymodels)
tidymodels_prefer()
library(conflicted)
targets::tar_load(names = c(lp_train, lp_test, tree_fit, lp_metrics, lp_folds))
# yardstick ---------------------------------------------------------------
augment(tree_fit, new_data = lp_train) |>
  dplyr::relocate(gas, .pred_class, .pred_TRUE, .pred_FALSE)
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
lp_folds$splits[1:3]

targets::tar_load(names = c(tree_wflow, lp_fit_resample_res, lp_ctrl))
# tree_wflow
lp_fit_resample_res |>
  collect_metrics() |>
  select(.metric, mean, n)

# 性能評価
targets::tar_load(names = c(lp_preds))
lp_preds |>
  group_by(id) |>
  lp_metrics(truth = gas, estimate = .pred_class)
# rm(lp_fit_resample_res_ctrl)

# ランダムフォレスト
targets::tar_load(names = c(rf_spec, rf_fit_resample_res, lp_final_fit))
# rf_spec
collect_metrics(rf_fit_resample_res)
lp_final_fit
collect_metrics(lp_final_fit)
extract_workflow(lp_final_fit)

collect_predictions(lp_final_fit) |>
  group_by(id) |>
  lp_metrics(truth = gas, estimate = .pred_class)
