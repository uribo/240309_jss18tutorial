library(targets)
source(here::here("data-raw/lp.R"))
tar_option_set(
  packages = c("conflicted",
               "tidymodels",
               "mlr3verse", "paradox",
               "sf", "spatialsample"),
  seed = 123)
conflicted::conflict_prefer("filter", "dplyr")

lp_data <-
  list(
    tar_target(
      landprice_raw,
      seq.int(8, 14) |>
        purrr::map(
          function(x) {
            x <- sprintf("%02d", x)
            kuniumi::read_ksj_l02(
              here::here(glue::glue("data-raw/L02-20_{x}_GML/L02-20_{x}.geojson")),
              .year = 2020)
          }
        ) |>
        dplyr::bind_rows()
    ),
    tar_target(
      landprice,
      landprice_raw |>
        dplyr::filter(!`基準地行政区域コード` %in% c("13361", "13362", "13363", "13364", "13381", "13382", "13401", "13402", "13421", "13900")) |>
        dplyr::select(!c(tidyselect::contains("前年度基準地コード"),
                  "基準地コード_見出し番号",
                  "年度",
                  tidyselect::contains("前面道路"),
                  tidyselect::contains("側道"),
                  "選定年次ビット",
                  "利用現況",
                  "基準地行政区域コード",
                  "住居表示",
                  "共通地点",
                  "間口比率",
                  "奥行比率",
                  "形状",
                  "公園区分",
                  "森林区分",
                  "駅名",
                  "周辺の土地利用の状況",
                  "建ぺい率",
                  "容積率",
                  tidyselect::num_range("調査価格_", 1983:2020),
                  tidyselect::starts_with("属性移動"))) |>
        sf::st_transform(crs = 4326) |>
        purrr::set_names(c("id", "price", "administration",
                           "area", "status", "structure",
                           "water", "gas", "sewer",
                           "above_floor",
                           "under_floor",
                           "dist_from_st",
                           "use_type", "fire", "city_plan",
                           "geometry"))
    ),
    tar_target(
      lp_supply,
      landprice |>
        sf::st_drop_geometry() |>
        dplyr::select(price, water, gas, sewer,
                      above_floor, under_floor,
                      dist_from_st, fire) |>
        dplyr::mutate(gas = as.factor(gas)) |>
        dplyr::filter(!is.na(fire))
    ),
    tar_target(
      lp_supply_sf,
      landprice |>
        dplyr::select(names(lp_supply)) |>
        dplyr::mutate(gas = as.factor(gas)) |>
        dplyr::filter(!is.na(fire))
    )
  )

part1_tm <-
  list(
    tar_target(
      lp_split,
      {
        set.seed(123)
        initial_split(lp_supply, prop = 0.8, strata = gas)
      }
    ),
    tar_target(
      lp_train,
      training(lp_split)
    ),
    tar_target(
      lp_test,
      testing(lp_split)
    ),
    tar_target(
      tree_spec,
      decision_tree(cost_complexity = 0.002) |>
        set_engine("rpart") |>
        set_mode("classification")
    ),
    tar_target(
      tree_wflow,
      workflow() |>
        add_formula(gas ~ .) |>
        add_model(tree_spec)
    ),
    tar_target(
      tree_fit,
      workflow(gas ~ ., spec = tree_spec) |>
        fit(data = lp_train)
    ),
    tar_target(
      tree_exp,
      DALEXtra::explain_tidymodels(tree_fit,
                                   data = lp_test,
                                   y = as.numeric(lp_test$gas),
                                   label = "lp_tree")
    ),
    tar_target(
      lp_perform,
      DALEX::model_performance(tree_exp)
    ),
    tar_target(
      lp_metrics,
      metric_set(accuracy, sensitivity, specificity)
    ),
    tar_target(
      lp_folds,
      # 2回の繰り返し、10分割交差検証
      vfold_cv(lp_train, v = 10, repeats = 2, strata = gas)
    ),
    tar_target(
      lp_fit_rs,
      fit_resamples(tree_wflow, lp_folds)
    ),
    tar_target(
      lp_ctrl,
      control_resamples(save_pred = TRUE)
    ),
    tar_target(
      lp_fit_rs_ctrl,
      fit_resamples(tree_wflow, lp_folds, control = lp_ctrl)
    ),
    tar_target(
      lp_preds,
      lp_fit_rs_ctrl |>
        collect_predictions()
    ),
    tar_target(
      rf_spec,
      rand_forest(trees = 1000,
                  mode = "classification") |>
        set_engine("randomForest")
    ),
    tar_target(
      rf_wflow,
      workflow(gas ~ price + water + sewer + above_floor + under_floor + dist_from_st + fire,
               rf_spec)
    ),
    tar_target(
      rf_fit_rs,
      fit_resamples(rf_wflow, lp_folds, control = lp_ctrl)
    ),
    tar_target(
      lp_final_fit,
      last_fit(rf_wflow, lp_split)
    ),
    tar_target(
      rf_tune_spec,
      rand_forest(min_n = tune(),
                  trees = tune(),
                  mode = "classification") |>
        set_engine("randomForest")
    ),
    tar_target(
      rf_tune_wflow,
      workflow(gas ~ ., rf_tune_spec)
    ),
    tar_target(
      rf_fit_tune_res,
      {
        set.seed(123)
        tune_grid(
          rf_tune_wflow,
          lp_folds,
          grid = 5)
      }
    ),
    tar_target(
      lp_tune_best_parameter,
      select_best(rf_fit_tune_res, metric = "roc_auc")
    ),
    tar_target(
      rf_tune_wflow_fin,
      finalize_workflow(rf_tune_wflow, lp_tune_best_parameter)
    ),
    tar_target(
      rf_final_fit,
      last_fit(rf_tune_wflow_fin, lp_split)
    )
  )

part1_mlr <-
  list(
    tar_target(
      lp_supply_chr2fct,
      lp_supply |>
        mutate(across(where(is.character), as.factor))
    ),
    tar_target(
      lp_task,
      as_task_classif(lp_supply_chr2fct, target = "gas")
    ),
    tar_target(
      lp_split_mlr,
      {
        set.seed(123)
        partition(lp_task, stratify = TRUE, ratio = 0.8)
      }
    ),
    tar_target(
      tree_learner,
      lrn("classif.rpart", cp = 0.002)
    ),
    tar_target(
      lp_metrics_mlr,
      msrs(c("classif.acc", "classif.sensitivity", "classif.specificity"))
    ),
    tar_target(
      lp_folds_mlr,
      rsmp("repeated_cv", repeats = 2, folds = 5)
    ),
    tar_target(
      rf_tune_learner,
      mlr3extralearners::lrn("classif.randomForest",
                             ntree = to_tune(500, 2000),
                             mtry = to_tune(1, 50))
    )
  )

part2_tm <-
  list(
    tar_target(
      lpsp_split,
      {
        set.seed(123)
        initial_split(lp_supply_sf, prop = 0.8, strata = gas)
      }
    ),
    tar_target(
      lpsp_train,
      training(lpsp_split)
    ),
    tar_target(
      lpsp_test,
      testing(lpsp_split)
    ),
    tar_target(
      lpsp_folds,
      {
        set.seed(123)
        vfold_cv(lpsp_train, v = 10, repeats = 1, strata = gas)
      }
    ),
    tar_target(
      lpsp_folds_cluster,
      {
        set.seed(123)
        spatial_clustering_cv(lpsp_train, v = 10)
      }
    ),
    tar_target(
      lpsp_folds_block,
      {
        set.seed(123)
        spatial_block_cv(lpsp_train, v = 10)
      }
    ),
    tar_target(
      rf_fit_rs_sp,
      fit_resamples(rf_wflow, lpsp_folds, control = lp_ctrl)
    ),
    tar_target(
      lpsp_final_fit,
      last_fit(rf_wflow, lpsp_split)
    ),
    tar_target(
      rf_fit_rs_spcluster,
      fit_resamples(rf_wflow, lpsp_folds_cluster, control = lp_ctrl)
    )
  )

part2_mlr <-
  list(
    tar_target(
      lpsp_task,
      as_task_classif_st(lpsp_train,
                         target = "gas",
                         positive = "TRUE"),
      packages = "mlr3spatiotempcv"
    ),
    tar_target(
      lpsp_folds_mlr,
      rsmp("repeated_spcv_coords", folds = 5, repeats = 100),
      packages = "mlr3spatiotempcv"
    )
  )

list(
  lp_data,
  part1_tm,
  part1_mlr,
  part2_tm,
  part2_mlr
)

# targets::tar_make()
