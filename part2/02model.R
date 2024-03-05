# source(here::here("part2/01spatial.R"))
rf_spec <-
  rand_forest(trees = 1000, mode = "classification") |>
  set_engine("randomForest")
rf_wflow <-
  workflow(gas ~ price + water + sewer + above_floor + under_floor + dist_from_st + fire, rf_spec)
ctrl_lp <-
  control_resamples(save_pred = TRUE)

rf_res <-
  fit_resamples(rf_wflow, lp_folds, control = ctrl_lp)
collect_metrics(rf_res)
final_fit <-
  last_fit(rf_wflow, lp_split)
collect_metrics(final_fit)
show_best(final_fit, metric = "accuracy")

lp_metrics <-
  metric_set(accuracy, sensitivity, specificity)

collect_predictions(final_fit) |>
  group_by(id) |>
  lp_metrics(truth = gas, estimate = .pred_class)

####
rf_res <-
  fit_resamples(rf_wflow, folds, control = ctrl_lp)
collect_metrics(rf_res)
final_fit <-
  last_fit(rf_wflow, lp_split)
collect_metrics(final_fit)
show_best(final_fit, metric = "accuracy")


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
