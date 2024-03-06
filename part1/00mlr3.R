library(dplyr)
library(mlr3verse)
mlr3verse_info()
library(randomForest)
library(GGally)
library(conflicted)
conflict_scout()
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("combine", "randomForest")
conflict_prefer("explain", "DALEX")
targets::tar_load(names = c(lp_supply_chr2fct, lp_task, lp_split_mlr))

# 1. Tasks ----------------------------------------------------------------
# データセット、扱う問題の種類（Target, Properties）を指定
# mlr3でサンプルとして用意されているtasks
mlr_tasks
task <-
  tsk("penguins")
task$properties
task$class_names
mlr_tasks$get("penguins")

# 自前のタスクを定義
lp_task
# autoplot(lp_task, type = "pairs")
lp_task$data(rows = c(1, 5, 10), cols = lp_task$feature_names) # data.table


# 2. learners -------------------------------------------------------------
# 学習モデルに関する情報（アルゴリズム、パラメータ、パッケージなど）
learner <-
  lrn("classif.rpart")
learner
learner$param_set$values

targets::tar_load(names = c(tree_learner))
tree_learner$param_set$values
names(lp_split_mlr)

# lp_supply_chr2fct |>
#   count(gas) |>
#   mutate(prop = n / sum(n))
#
# lp_task$data()[lp_split_mlr$train] |>
#   count(gas) |>
#   mutate(prop = n / sum(n))
# lp_task$data()[lp_split_mlr$test] |>
#   count(gas) |>
#   mutate(prop = n / sum(n))

# 学習器を用いて学習を実行する
tree_learner$train(lp_task, row_ids = lp_split_mlr$train)
tree_learner$model

tree_learner$predict(lp_task, row_ids = lp_split_mlr$test)$confusion
prediction <-
  tree_learner$predict(lp_task, row_ids = lp_split_mlr$test)
prediction

# autoplot(prediction)
prediction$confusion
# 3. Evaluation -----------------------------------------------------------
as.data.table(mlr_measures)[task_type == "classif" & predict_type == "response" & task_properties != "twoclass"] |>
  glimpse()
# as.data.table(msr())

msr("classif.ce")

prediction$score(msr("classif.acc"))
prediction$score(msr("classif.ce"))

targets::tar_load(lp_metrics_mlr)
prediction$score(lp_metrics_mlr)

# モデルの解釈
# 1. 変数重要度
# library(iml)
library(DALEX)
#   explain(tree_learner,
#           data = lp_task$data(),
#           y = as.numeric(lp_task$truth()),
#           label = "rpart_classif")

vip::vip(tree_learner$model)



# nrow(lp_task$data()[lp_split_mlr$test, ])
# length(lp_task$truth()[lp_split_mlr$test])
# length(as.numeric(lp_task$data()[lp_split_mlr$test, ][gas == TRUE][["gas"]]))
# tree_exp_mlr <-
#   DALEXtra::explain_mlr3(
#   tree_learner,
#   data = lp_task$data()[lp_split_mlr$test, ],
#   y = as.numeric(lp_task$truth()[lp_split_mlr$test]),
#   label = "lp_tree_mlr")
# model_performance(tree_exp_mlr)

#  DALEXtra::explain_mlr3(tree_learner, data = lp_task$data(), y = as.numeric(lp_task$truth()), label = "lp_tree_mlr")

# 4. Resampling -----------------------------------------------------------
# mlr_resamplings
as.data.table(mlr_resamplings) |>
  as_tibble()

targets::tar_load(lp_folds_mlr)
lp_folds_mlr$instantiate(lp_task)

rr <-
  resample(lp_task, learner, lp_folds_mlr)
rr

acc <- rr$score(lp_metrics_mlr)
acc[, .(iteration, classif.specificity)]
rr$aggregate(msr("classif.specificity"))
# type = "histogram"
autoplot(rr, measure = msr("classif.acc"), type = "boxplot")
# ?mlr3viz:::autoplot.ResampleResult

# 5. Tuning ----------------------------------------------------------------
# detach("package:mlr3extralearners", character.only = TRUE)
lrn() # 152
library(mlr3extralearners)
lrn() # 171

lrn("classif.ksvm")$param_set |>
  as.data.table()
targets::tar_load(rf_tune_learner)
rf_tune_learner$param_set$ids()
rf_tune_learner$param_set$values

instance <- ti(
  task = lp_task,
  learner = rf_tune_learner,
  resampling = lp_folds_mlr,
  measures = lp_metrics_mlr,
  terminator = trm("none"))
tnr("random_search")

lp_tuner <- tnr("grid_search", resolution = 5, batch_size = 10)
lp_tuner

# run
lp_tuner$optimize(instance)

rf_tune_learner$param_set$values <-
  instance$result_learner_param_vals
rf_tune_learner$train(lp_task)
as.data.table(instance$archive)


# lp_tuner$param_set


# 6. Feature selection ----------------------------------------------------

# 7. Pipelines ------------------------------------------------------------
