# tidymodels --------------------------------------------------------------
library(tidymodels)


# mlr ---------------------------------------------------------------------
library(mlr3)


# DALEX -------------------------------------------------------------------
library(DALEX)
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
