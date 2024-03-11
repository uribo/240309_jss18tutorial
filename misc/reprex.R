# options(
#   reprex.highlight.hl_style  = "base16/google-dark")
library(styler)
library(reprex)
ggplot2::theme_set(ggplot2::theme_bw(base_size = 20))
reprex({
  library(tidymodels)
}, venue = "rtf", tidyverse_quiet = FALSE)
reprex({
  lag(ldeaths, 1)
  library(dplyr)
  lag(ldeaths, 1)
}, venue = "rtf")
reprex({
  library(tidymodels)
  tidymodels_prefer()
  library(conflicted) # 関数の衝突が発生していると、優先順位を指定するように促すメッセージを出力してくれる
  conflict_scout() # 現在の衝突状況を確認（tidymodels内での競合）
}, venue = "rtf")
reprex({
  library(DALEX)
}, venue = "rtf")
# dark
reprex({
  targets::tar_load(names = c(lp_supply))
  dplyr::glimpse(lp_supply)
}, venue = "rtf", wd = getwd())
reprex({
  library(dplyr)
  library(ggplot2)
  targets::tar_load(lp_supply)
  lp_supply |>
    count(gas) |>
    ggplot() +
    aes(gas, n) +
    geom_bar(stat = "identity")
  lp_supply |>
    ggplot() +
    aes(gas, price) +
    geom_violin() +
    scale_y_log10()
  lp_supply |>
    ggplot() +
    aes(price, above_floor, color = gas) +
    geom_jitter(alpha = 0.5) +
    scale_x_log10()
  lp_supply |>
    ggplot() +
    aes(price, dist_from_st, color = gas) +
    geom_jitter(alpha = 0.5) +
    scale_x_log10() +
    scale_y_log10()
}, venue = "rtf", wd = getwd())
reprex::reprex(
  {
    set.seed(123)
    lp_split <-
      initial_split(lp_supply, prop = 0.8, strata = gas)
    lp_split
    #> <Training/Testing/Total>
    #> <1449/363/1812>
    lp_train <- training(lp_split)
    lp_test <- testing(lp_split)
  },
  venue = "rtf"
)
reprex::reprex({
  library(parsnip)
  #> 1. モデルの選択
  decision_tree()
  logistic_reg() # ロジスティック回帰モデルを指定、自動的にモードがclassificationになる
  linear_reg()
  #> 2. エンジンの指定

  show_engines("decision_tree")

  decision_tree() |>
    set_engine("rpart")

  # 3. モード（タスク）... 「回帰」または「分類」を指定
  tree_spec <-
    decision_tree(cost_complexity = 0.002) |>
    set_engine("rpart") |>
    set_mode("classification")
  tree_spec
}, venue = "rtf")
reprex({
  # rand_forest(mtry = 10, trees = 2000) |>
  #   set_engine("{engine}") |>
  #   set_mode("regression")
  # ranger::ranger(x, y,
  #                mtry = min_cols(~10, x), num.trees = 2000, ...)
  # randomForest::randomForest(x, y, mtry = min_cols(~10, x), ntree = 2000)
  # sparklyr::ml_random_forest(x, formula,
  #                            type = "regression",
  #                            feature_subset_strategy = "10",
  #                            num_trees = 2000,
  #                            ...)
  rand_forest(mtry = 10, trees = 2000) |>
    set_engine("ranger") |>
    set_mode("regression") |>
    translate()
  #> Random Forest Model Specification (regression)
  #>
  #> Main Arguments:
  #>   mtry = 10
  #> trees = 2000
  #>
  #> Computational engine: ranger
  #>
  #> Model fit template:
  #>   ranger::ranger(x = missing_arg(), y = missing_arg(), weights = missing_arg(),
  #>                  mtry = min_cols(~10, x), num.trees = 2000, num.threads = 1,
  #>                  verbose = FALSE, seed = sample.int(10^5, 1))
}, venue = "rtf")
reprex({
  tree_wflow <-
    workflow() |>
    add_formula(gas ~ .) |>
    add_model(tree_spec)
  workflow(gas ~ ., spec = tree_spec)

  workflow(gas ~ ., spec = tree_spec) |>
    fit(data = lp_train)
}, venue = "rtf")
reprex({
  library(tidymodels)
  targets::tar_load(names = c(tree_wflow, lp_train, lp_test))
  tree_fit <-
    tree_wflow |>
    fit(data = lp_train)
  tree_fit
  predict(tree_fit, new_data = lp_test)
}, venue = "rtf", wd = getwd())
# dark
reprex({
  tree_fit |>
    extract_fit_engine() |>
    rpart.plot::rpart.plot(roundint = FALSE)
}, venue = "rtf")
# dark
reprex({
  tree_fit |>
    extract_fit_engine() |>
    vip::vip()
}, venue = "rtf")
reprex({
  tree_exp <-
    DALEXtra::explain_tidymodels(tree_fit,
                               data = lp_test,
                               y = as.numeric(lp_test$gas)-1,
                               label = "lp_tree")
  DALEX::explain(tree_fit,
                 data = lp_test,
                 y = as.numeric(lp_test$gas)-1,
                 label = "lp_tree")
  DALEX::model_performance(tree_exp)
}, venue = "rtf")

reprex({
  targets::tar_load(tree_exp)
  DALEX::model_performance(tree_exp)
}, venue = "rtf", wd = getwd())
reprex({
  library(DALEX)
  # library(workflows)
  # targets::tar_load(names = c(tree_exp))
  tree_effect <-
    model_parts(tree_exp, type = "variable_importance")
  plot(tree_effect, show_boxplots = TRUE)
}, venue = "rtf")
reprex({
  predict(tree_fit, new_data = lp_test[1, ])
  #> # A tibble: 1 × 1
  #> .pred_class
  #> <fct>
  #>   1 FALSE
  predict(tree_exp, lp_test[1, ])
  #> TRUE
  #>0.4736842
}, venue = "rtf")
reprex({
  predict_parts(tree_exp,
                new_observation = lp_test[3, ]) |>
    plot()
}, venue = "rtf")
reprex({
  library(tidymodels)
  targets::tar_load(names = c(lp_train, tree_fit))
  lp_tree_augment <-
    augment(tree_fit, new_data = lp_train)
  lp_tree_augment |>
    # 学習データにモデル予測結果が列として与えられている
    # 確認のために一部の列だけを表示
    select(starts_with(".pred_"), gas)
}, venue = "rtf", wd = getwd())

reprex({
  library(tidymodels)
  targets::tar_load(names = c(lp_train, tree_fit, tree_exp, lp_metrics, lp_tree_augment))
  lp_tree_augment |>
    conf_mat(truth = gas, estimate = .pred_class)

  # データが不均衡である点に注意
  lp_tree_augment |>
    accuracy(truth = gas, estimate = .pred_class)
  lp_tree_augment |>
    sensitivity(truth = gas, estimate = .pred_class)
  lp_tree_augment |>
    specificity(truth = gas, estimate = .pred_class)

  lp_metrics <-
    metric_set(accuracy, sensitivity, specificity)
  class(lp_metrics)
  lp_tree_augment |>
    lp_metrics(truth = gas, estimate = .pred_class)
})
reprex({
  lp_tree_augment |>
    conf_mat(truth = gas, estimate = .pred_class) |>
    autoplot("heatmap")
}, venue = "rtf")
reprex({
  library(tidymodels)
  targets::tar_load(names = c(tree_fit, lp_test, lp_metrics))
  tree_fit |>
    augment(new_data = lp_test) |>
    lp_metrics(truth = gas, estimate = .pred_class)
}, venue = "rtf", wd = getwd())
reprex({
  library(rsample)
  targets::tar_load(lp_train)
  # 2回の繰り返し、10分割交差検証
  lp_folds <- vfold_cv(lp_train, v = 10, repeats = 2, strata = gas)
  lp_folds$splits[1:3]
}, venue = "rtf", wd = getwd())
reprex({
  library(tidymodels)
  targets::tar_load(names = c(tree_wflow, lp_folds))
  lp_fit_rs <-
    fit_resamples(tree_wflow, lp_folds)
  lp_fit_rs
  lp_fit_rs$.metrics[[1]]
  # lp_fit_rs$.metrics |>
  #   purrr::map(\(x) x[1, 3]) |>
  #   purrr::reduce(c) |>
  #   purrr::flatten_dbl() |>
  #   mean()
  lp_fit_rs |>
    collect_metrics()
}, venue = "rtf", wd = getwd())
reprex({
  library(tidymodels)
  targets::tar_load(names = c(tree_wflow, lp_folds, lp_metrics))
  lp_ctrl <-
    # リサンプリングデータに対する予測値を列に保存するため
    control_resamples(save_pred = TRUE)
  lp_preds <-
    fit_resamples(tree_wflow, lp_folds, control = lp_ctrl) |>
    # リサンプリングデータを展開して一つのデータフレームに
    collect_predictions()
  lp_preds
  lp_preds |>
    group_by(id) |>
    lp_metrics(truth = gas, estimate = .pred_class)
}, venue = "rtf", wd = getwd())
reprex({
  library(tidymodels)
  targets::tar_load(names = c(tree_wflow, lp_rec))
  tree_wflow |>
    remove_formula() |>
    add_recipe(lp_rec)
}, venue = "rtf", wd = getwd())
reprex({
  library(tidymodels)
  targets::tar_load(names = c(lp_folds, lp_ctrl, lp_split))
  rf_spec <-
    rand_forest(trees = 1000,
              mode = "classification") |>
    set_engine("randomForest")
  rf_spec

  rf_wflow <-
    workflow(gas ~ .,
             rf_spec)
  rf_wflow

  rf_fit_rs <-
    fit_resamples(rf_wflow, lp_folds, control = lp_ctrl)

  rf_fit_rs |>
    collect_metrics()

}, venue = "rtf", wd = getwd())
reprex({
  library(tidymodels)
  targets::tar_load(names = c(lp_folds, lp_ctrl, lp_split))
  rf_spec <-
    rand_forest(trees = 1000,
              mode = "classification") |>
    set_engine("randomForest")
  rf_spec

  rf_wflow <-
    workflow(gas ~ .,
             rf_spec)
  rf_wflow

  rf_fit_rs <-
    fit_resamples(rf_wflow, lp_folds, control = lp_ctrl)

  rf_fit_rs |>
    collect_metrics()

}, venue = "rtf", wd = getwd())
reprex({
  library(tidymodels)
  targets::tar_load(names = c(lp_folds, lp_rec))
  rf_tune_spec <-
    rand_forest(min_n = tune(),
              trees = tune(),
              mode = "classification") |>
    set_engine("randomForest")
  rf_tune_spec

  rf_tune_wflow <-
    workflow() |>
    add_model(rf_tune_spec) |>
    add_recipe(lp_rec)
  rf_fit_tune_res <-
    tune_grid(rf_tune_wflow,
              lp_folds,
              grid = 5)
  rf_fit_tune_res
}, venue = "rtf", wd = getwd())
reprex({
  library(tune)
  targets::tar_load(names = c(rf_fit_tune_res, rf_fit_tune_res))
  rf_fit_tune_res
  lp_tune_best_parameter <-
    select_best(rf_fit_tune_res, metric = "roc_auc")
  lp_tune_best_parameter
}, venue = "rtf", wd = getwd())
reprex({
  library(tidymodels)
  targets::tar_load(names = c(rf_tune_wflow, lp_tune_best_parameter, lp_split))
  finalize_workflow(rf_tune_wflow, lp_tune_best_parameter) |>
    last_fit(lp_split) |>
    collect_metrics()
}, venue = "rtf", wd = getwd())
reprex({
  library(tidymodels)
  targets::tar_load(names = c(lp_train, lp_test))
  lp_rec <-
    recipe(gas ~ ., data = lp_train) |>
    step_zv(all_predictors()) |>
    step_log(price, dist_from_st, offset = 0.01) |>
    step_normalize(all_numeric_predictors()) |>
    step_dummy(all_nominal_predictors())
  lp_rec

  prep(lp_rec) |>
    # 影響をうける変数とその効果について確認
    # stepの順番を指定
    tidy(number = 1)

  prep(lp_rec) |>
    bake(new_data = lp_train) |>
    glimpse()

}, venue = "rtf", wd = getwd())
reprex({
  library(tidymodels)
  targets::tar_load(names = wf_set_fit)
  wf_set_fit |>
    rank_results() |>
    pull(wflow_id) |>
    _[1]
}, venue = "rtf", wd = getwd())
reprex({
  targets::tar_load(lp_supply)
  lp_supply$water |> unique()
}, venue = "rtf", wd = getwd())

# mlr3
reprex({
  library(dplyr)
  library(mlr3verse)
  targets::tar_load(names = lp_supply)

  lp_supply_chr2fct <-
    lp_supply |>
    mutate(across(where(is.character), as.factor))
  # タスクの定義
  lp_task <-
    as_task_classif(lp_supply_chr2fct, target = "gas")
  # データ分割（層化抽出、80%を学習データに）
  set.seed(123)
  lp_split_mlr <-
    partition(lp_task, stratify = TRUE, ratio = 0.8)
  # 学習器の構築
  tree_learner <-
    lrn("classif.rpart", cp = 0.002)
  # 評価指標の定義
  lp_metrics_mlr <-
    msrs(c("classif.acc", "classif.sensitivity", "classif.specificity"))
  # リサンプリングデータの用意（2回の繰り返し5分割交差検証法）
  lp_folds_mlr <-
    rsmp("repeated_cv", repeats = 2, folds = 5)
  rr <-
    resample(lp_task, tree_learner, lp_folds_mlr)
  acc <- rr$score(lp_metrics_mlr)
  acc[, .(iteration, classif.specificity)]
  rr$aggregate(msr("classif.acc"))
  rr$aggregate(msr("classif.sensitivity"))
  rr$aggregate(msr("classif.specificity"))
}, venue = "rtf", wd = getwd())
reprex({
  library(tidymodels)
  targets::tar_load(names = c(lp_folds, lp_rec, tree_spec, rf_spec))
  wf_set_fit <-
    workflow_set(preproc = list(none = gas ~ ., prep = lp_rec),
               models = list(tree_spec, rf_spec),
               cross = TRUE) |>
    workflow_map("fit_resamples",
                 resamples = lp_folds)
  wf_set_fit |>
    rank_results()
}, venue = "rtf", wd = getwd())
reprex({
  library(tidymodels)
  targets::tar_load(names = c(rf_spec, lp_rec))
  rf_wflow <-
    workflow() |>
    add_model(rf_spec) |>
    add_recipe(lp_rec)
  rf_wflow
}, venue = "rtf", wd = getwd())


# part2 -------------------------------------------------------------------
reprex({
  library(sf)
  library(dplyr)
  targets::tar_load(lp_supply_sf)
  lp_supply_sf |>
    glimpse()
}, venue = "rtf", wd = getwd())
reprex({
  mapview::mapview(lp_supply_sf,
          zcol = "gas",
          alpha.regions = 0.3,
          cex = 2.6,
          lwd = 0.4,
          legend = TRUE)
  lp_supply_sf |>
    filter(price > 1000000) |>
    mapview::mapview(zcol = "price")
}, venue = "rtf")
reprex({
  library(spatialsample)
  set.seed(123)
  lpsp_split <-
    initial_split(lp_supply_sf, prop = 0.8, strata = gas)
  lpsp_train <-
    training(lpsp_split)
  lpsp_test <-
    testing(lpsp_split)
  set.seed(123)
  spatial_clustering_cv(lpsp_train, v = 10)
}, venue = "rtf")
reprex({
  library(tidymodels)
  targets::tar_load(names = c(rf_wflow, lpsp_folds_cluster, lp_ctrl))
  fit_resamples(rf_wflow, lpsp_folds_cluster, control = lp_ctrl) |>
    collect_metrics()
}, venue = "rtf", wd = getwd())
reprex({
  library(mlr3verse)
  library(mlr3spatiotempcv)
  lpsp_task <-
    as_task_classif_st(lpsp_train,
                     target = "gas",
                     positive = "TRUE")
  lpsp_folds_mlr <-
    rsmp("repeated_spcv_coords", folds = 5, repeats = 100)
  rr_spcv_rf <-
    resample(task = lpsp_task,
           learner = lrn("classif.ranger"),
           resampling = lpsp_folds_mlr)
}, venue = "rtf")
reprex({
  library(mlr3verse)
  targets::tar_load(names = c(rr_spcv_rf))
  rr_spcv_rf$aggregate(measures = msr("classif.acc"))
  rr_spcv_rf$aggregate(measures = msr("classif.sensitivity"))
  rr_spcv_rf$aggregate(measures = msr("classif.specificity"))
}, venue = "rtf", wd = getwd())
reprex({
  library(tidymodels)
  library(sf)
  # 第一部での最後のモデル
  targets::tar_load(names = c(rf_final_fit, lp_supply_sf_shikoku, lp_test))
  extract_workflow(rf_final_fit) |>
    augment(new_data = lp_test) |>
    brier_class(truth = gas, .pred_FALSE)
  extract_workflow(rf_final_fit) |>
    # fireがNAのものを除外
    augment(new_data = lp_supply_sf_shikoku |>
              dplyr::filter(!is.na(fire))) |>
    brier_class(truth = gas, .pred_FALSE)
}, venue = "rtf", wd = getwd())
reprex({
  library(tidymodels)
  targets::tar_load(names = c(rf_fit, lp_train))
  importance <-
    vip::vi_permute(
    rf_fit,
    target = "gas",
    metric = "accuracy",
    pred_wrapper = function(object, newdata) {
      predict(object, new_data = newdata)$.pred_class
    },
    train = lp_train |>
      select(gas, sewer, price, above_floor, dist_from_st))
  importance
}, venue = "rtf", wd = getwd())
reprex({
  library(tidymodels)
  targets::tar_load(names = c(f, lp_train, lp_test, importance))
  lp_aoa <-
    waywiser::ww_area_of_applicability(
      f,
      lp_train |>
        mutate(gas = as.numeric(gas) - 1,
               sewer = as.numeric(sewer),
               fire = as.numeric(as.factor(fire))),
      lp_test |>
        mutate(gas = as.numeric(gas) - 1,
               sewer = as.numeric(sewer),
               fire = as.numeric(as.factor(fire))),
      importance = importance
    )
  lp_aoa
}, venue = "rtf", wd = getwd())
reprex({
  lp_supply_aoa <-
    bind_cols(
    lp_supply_sf |>
      mutate(gas = as.numeric(gas)-1,
             sewer = as.numeric(sewer)),
    predict(lp_aoa, lp_supply_sf |>
              mutate(gas = as.numeric(gas)-1,
                     sewer = as.numeric(sewer))))
  mapview::mapview(lp_supply_aoa |>
                     filter(aoa == TRUE), col.regions = "gray") +
    mapview::mapview(lp_supply_aoa |>
                       filter(aoa == FALSE), col.regions = "#FFA500")
}, venue = "rtf")
