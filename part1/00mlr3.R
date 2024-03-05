library(mlr3)
library(GGally)
glimpse(penguins)


# 1. Tasks ----------------------------------------------------------------
# データセット、扱う問題の種類（Target, Properties）を指定
# mlr3でサンプルとして用意されているtasks
mlr_tasks
task <-
  tsk("penguins")
task$properties
task$class_names


mlr_tasks$get("penguins")
task$data(rows = c(1, 5, 10), cols = task$feature_names) # data.table

# autoplot(task, type = "pairs")

# as_task_classif(penguins, target = "species")


# 2. learners -------------------------------------------------------------
# 学習モデルに関する情報（アルゴリズム、パラメータ、パッケージなど）
learner <-
  lrn("classif.rpart")
learner
learner$param_set$values

learner <-
  lrn("classif.rpart", xval = 3, cp = 0.2, maxdepth = 5)
learner$param_set$values

split <-
  partition(task)
names(split)


# 学習器を用いて学習を実行する
learner$train(task, row_ids = split$train)
learner$model

prediction <-
  learner$predict(task, row_ids = split$test)
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

measures <- msrs(c("classif.acc", "classif.ce"))
prediction$score(measures)

# 4. Resampling -----------------------------------------------------------
# mlr_resamplings
as.data.table(mlr_resamplings) |>
  as_tibble() |> View()

# 2回の繰り返し、5分割交差検証
rcv25 <-
  rsmp("repeated_cv", repeats = 2, folds = 5)
rcv25$instantiate(task)

rr <-
  resample(task, learner, rcv25)
rr

acc <- rr$score(msr("classif.ce"))
acc[, .(iteration, classif.ce)]
rr$aggregate(msr("classif.ce"))
autoplot(rr, measure = msr("classif.acc"), type = "boxplot")

# 5. Tuning ----------------------------------------------------------------
library(mlr3extralearners)
library(paradox)
lrn()

lrn("classif.ksvm")$param_set |>
  as.data.table() |> View()
learner <- lrn("classif.randomForest",
               ntree = to_tune(500, 2000),
               mtry = to_tune(1, 50))
learner$param_set$ids()

library(mlr3tuning)
instance = ti(
  task = task,
  learner = learner,
  resampling = rsmp("cv", folds = 3),
  measures = msr("classif.ce"),
  terminator = trm("none")
)
tnr("random_search")
tnr("grid_search")

tuner <- tnr("grid_search", resolution = 5, batch_size = 10)
tuner
tuner$param_set


# 6. Feature selection ----------------------------------------------------

# 7. Pipelines ------------------------------------------------------------
