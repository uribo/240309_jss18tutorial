###############################
# 都道府県地価調査データを使ったガス供給状況の予測
# 3つのモデルを作成する
# 1. ロジスティック回帰
# 2. 決定木
# 3. ランダムフォレスト
###############################
library(tidymodels)
# tidymodelsで使う関数と他の関数が競合しない（tidymodelsを優先する）ようにする
tidymodels_prefer()
library(DALEX)
library(conflicted)
targets::tar_load(names = c(lp_supply,
                            lp_split,
                            lp_train, lp_test,
                            lp_wflow,
                            lp_perform))
lp_supply |> count(gas)

# lp_supply |>
#   ggplot() +
#   aes(gas, price) +
#   geom_boxplot()

# lp_supply |>
#   filter(price < 125e3) |>
#   filter(price < 64e3) |>
#   count(gas)


# rsample -----------------------------------------------------------------
list(lp_train, lp_test) |>
  purrr::map(~ dim(.x))

# EDAでデータの特徴を探る
# [ADVANCED] 時系列データの分割

# parsnip -----------------------------------------------------------------
# 1. モデルの選択
logistic_reg() # 自動的にmodeがclassificationになる

# 2. エンジンの指定
# lm, glm, glmnet
parsnip::show_engines("linear_reg")

logistic_reg() |>
  set_engine("glmnet")
logistic_reg() |>
  set_engine("stan")

# 3. mode（回帰または分類）
# decision_tree() |>
#   set_mode("classification")
# parsnip::show_engines("decision_tree")
# rpartを選択する

# workflow ----------------------------------------------------------------
# pre-processとmodelを組みあわせる

  # tree_depth =
  # min_n =


targets::tar_load(names = c(tree_spec, tree_fit))
# 同じ出力を得る
all.equal(
  lp_wflow,
  workflow(gas ~ ., spec = tree_spec)
)
waldo::compare(
  tree_fit,
  lp_wflow |>
    fit(data = lp_train))

# テストデータに対する予測
predict(tree_fit, new_data = lp_test) |>
  count(.pred_class)
lp_test |>
  count(gas)

# 解釈
targets::tar_load(tree_exp)
tree_fit |>
  extract_fit_engine() |>
  rpart.plot::rpart.plot(roundint = FALSE)
tree_fit |>
  extract_fit_engine() |>
  vip::vip()
# 説明モデル分析
# Global
plot(lp_perform, geom = "roc")
tree_effect <-
  model_parts(tree_exp, type = "variable_importance")
plot(tree_effect, show_boxplots = TRUE)
# 重要度を損失関数の差分として評価
model_parts(tree_exp, type = "difference") |>
  plot(tree_effect, show_boxplots = FALSE)

# Local
# 単一の観測に対してモデルがどのように動作するか
predict(tree_exp, lp_test[1, ])
plot(predict_parts(tree_exp, new_observation = lp_test[6, ]))
plot(predict_parts(tree_exp, new_observation = lp_test[8, ]))
plot(predict_parts(tree_exp, new_observation = lp_test[10, ]))
