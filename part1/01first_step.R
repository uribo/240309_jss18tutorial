###############################
# 都道府県地価調査データを使ったガス供給状況の予測
# 3つのモデルを作成する
# 1. ロジスティック回帰
# 2. 決定木
# 3. ランダムフォレスト
###############################
library(tidymodels)
source(here::here("data-raw/lp.R"))
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
set.seed(123)
lp_split <-
  initial_split(lp_supply, prop = 0.8, strata = gas)
lp_train <- training(lp_split)
glimpse(lp_train)
lp_test <- testing(lp_split)

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

# workflow ----------------------------------------------------------------
# pre-processとmodelを組みあわせる
tree_spec <-
  decision_tree(cost_complexity = 0.002) |>
  set_engine("rpart") |>
  set_mode("classification")

# tree_spec |>
#   fit(gas ~ ., data = lp_train)

# 同じ出力を得る
lp_wflow <-
  workflow() |>
  add_formula(gas ~ .) |>
  add_model(tree_spec)
lp_wflow |>
  fit(data = lp_train)
tree_fit <-
  workflow(gas ~ ., spec = tree_spec) |>
  fit(data = lp_train)
# テストデータに対する予測
predict(tree_fit, new_data = lp_test) |>
  count(.pred_class)
lp_test |>
  count(gas)

tree_fit |>
  extract_fit_engine() |>
  rpart.plot::rpart.plot(roundint = FALSE)
tree_fit |>
  extract_fit_engine() |>
  vip::vip()

DALEXtra::explain_tidymodels(tree_fit,
                   data = lp_test |>
                     select(price,
                            gas,
                            above_floor, under_floor,
                            dist_from_st, fire),
                   y = lp_test$gas)
