###############################
# 都道府県地価調査データを使ったガス供給状況の予測
# 2つのモデルを作成する
# 1. 決定木
# 2. ランダムフォレスト
###############################
library(tidymodels)
# tidymodelsで使う関数と他の関数が競合しない（tidymodelsを優先する）ようにする
tidymodels_prefer()
library(DALEX)
library(ranger)
# library(ggpubr)
# DALEX::install_dependencies()
library(conflicted)
targets::tar_load(names = c(lp_supply,
                            lp_split,
                            lp_train, lp_test,
                            tree_wflow,
                            lp_perform))
glimpse(lp_supply)

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

# rsample -----------------------------------------------------------------
set.seed(123)
lp_split <-
  initial_split(lp_supply, prop = 0.8, strata = gas)
lp_split
lp_train <- training(lp_split)
lp_test <- testing(lp_split)


# parsnip -----------------------------------------------------------------
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

# workflow ----------------------------------------------------------------
# pre-processとmodelを組みあわせる

  # tree_depth =
  # min_n =
targets::tar_load(names = c(tree_spec, tree_fit))
# 同じ出力を得る
all.equal(
  tree_wflow,
  workflow(gas ~ ., spec = tree_spec)
)
waldo::compare(
  tree_fit,
  tree_wflow |>
    fit(data = lp_train))

# テストデータに対する予測
targets::tar_read(tree_pred)
lp_test |>
  count(gas)

# 解釈
targets::tar_load(tree_exp)
# 説明モデル分析
# Global
plot(lp_perform, geom = "roc")
# 重要度を損失関数の差分として評価
model_parts(tree_exp, type = "difference") |>
  plot(tree_effect, show_boxplots = FALSE)
