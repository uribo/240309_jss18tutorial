library(tidymodels)
tidymodels_prefer()
targets::tar_load(names = c(rf_tune_spec, rf_tune_wflow))
# ハイパーパラメータ
# tune
rf_tune_spec
rf_tune_wflow

targets::tar_load(names = c(lp_folds, rf_fit_tune_res))
# all.equal(
#   rf_fit_tune_res,
#   {
#     set.seed(123)
#     tune_grid(
#       rf_tune_wflow,
#       lp_folds,
#       grid = 5)
#   }
# )

# ref) fit_resamples()

show_best(rf_fit_tune_res, metric = "roc_auc")
targets::tar_read(lp_tune_best_parameter)
targets::tar_load(rf_final_fit)
collect_metrics(rf_final_fit)
