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
library(mlr3spatiotempcv)
task <- as_task_classif_st(lp_train,
                        target = "gas",
                        positive = "TRUE")
learner <- mlr3::lrn("classif.ranger")
resampling = mlr3::rsmp("repeated_spcv_coords", folds = 5, repeats = 100)

rr_spcv_rf = mlr3::resample(task = task,
                             learner = learner,
                             resampling = resampling)
