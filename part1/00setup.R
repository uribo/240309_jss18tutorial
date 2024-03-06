# tidymodels --------------------------------------------------------------
library(tidymodels)
library(ranger)
# library(censored) # 生存時間解析のためのparsnipの拡張（扱わない）
# library(poissonreg)
# library(brulee)
library(tidyclust) # クラスタリングのためのparsnipの拡張
library(bonsai) # 木モデルのためのparsnipの拡張

# recipes拡張パッケージ
# library(embed)
# library(textrecipes)
# library(themis)

# DALEX -------------------------------------------------------------------
library(DALEX)
library(ggpubr)
# install_dependencies()
apartments_lm <- lm(m2.price ~ ., data = apartments)
anova(apartments_lm)

apartments_test$m2.price[1:6]
predict(apartments_lm, apartments_test[1:6,])

predicted_apartments_lm <- predict(apartments_lm, apartments_test)
sqrt(mean((predicted_apartments_lm - apartments_test$m2.price)^2))

apartments_lm_exp <- explain(model = apartments_lm,
                             data = apartments_test[,-1],
                             y = apartments_test$m2.price,
                             label = "Linear Regression")

# x <- predict_parts(...)
# plot(x)
