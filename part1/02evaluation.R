library(tidymodels)
tidymodels_prefer()
library(conflicted)
targets::tar_load(names = c(lp_train, lp_test, tree_fit, lp_metrics, lp_folds,
                            lp_tree_augment))
# yardstick ---------------------------------------------------------------
lp_tree_augment |>
  dplyr::relocate(gas, .pred_class, .pred_TRUE, .pred_FALSE)
# accurary
lp_tree_augment |>
  group_by(fire) |>
  lp_metrics(truth = gas, estimate = .pred_class)

# ROC curve
lp_tree_augment |>
  roc_auc(truth = gas, .pred_FALSE)
lp_tree_augment |>
  roc_curve(truth = gas, .pred_FALSE) |>
  autoplot()

# 過学習への注意
lp_tree_augment |>
  accuracy(truth = gas, estimate = .pred_class) # resubstitution estimate
tree_fit |>
  augment(new_data = lp_test) |>
  accuracy(truth = gas, estimate = .pred_class) # generalization estimate
# テストデータの利用は、モデルの評価のためにのみ利用すること

lp_tree_augment |>
  brier_class(truth = gas, .pred_FALSE)
tree_fit |>
  augment(new_data = lp_test) |>
  brier_class(truth = gas, .pred_FALSE)

# 交差検証法の導入

# ランダムフォレスト
targets::tar_load(names = c(rf_spec, rf_fit_rs, lp_final_fit))
collect_metrics(rf_fit_rs)
lp_final_fit
collect_metrics(lp_final_fit)
extract_workflow(lp_final_fit)

collect_predictions(lp_final_fit) |>
  group_by(id) |>
  lp_metrics(truth = gas, estimate = .pred_class)
