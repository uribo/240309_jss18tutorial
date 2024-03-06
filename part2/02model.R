library(tidymodels)
tidymodels_prefer()
targets::tar_load(names = c(lp_metrics, rf_fit_resample_res_sp, lpsp_final_fit))
collect_metrics(rf_fit_resample_res_sp)
collect_metrics(lpsp_final_fit)
show_best(lpsp_final_fit, metric = "accuracy")

collect_predictions(rf_fit_resample_res_sp) |>
  group_by(id) |>
  lp_metrics(truth = gas, estimate = .pred_class)

targets::tar_load(rf_fit_rs_spcluster)
collect_metrics(rf_fit_rs_spcluster)
collect_predictions(rf_fit_rs_spcluster) |>
  group_by(id) |>
  lp_metrics(truth = gas, estimate = .pred_class)

# mlr ---------------------------------------------------------------------
library(mlr3verse)
library(mlr3spatiotempcv)
targets::tar_load(names = c(lpsp_task, lpsp_folds_mlr))
learner <- lrn("classif.ranger")
rr_spcv_rf <- resample(task = lpsp_task,
                       learner = learner,
                       resampling = lpsp_folds_mlr)
rr_spcv_rf$aggregate(measures = msr("classif.acc"))
