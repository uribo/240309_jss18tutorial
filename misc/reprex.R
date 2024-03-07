library(styler)
library(reprex)
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
