library(caret)
library(recipes)

train3_dummy <- cbind(train2_pc, train2_dummy %>% select(1:28, 119))

train3_dummy_full_rank <- train3_dummy %>% 
  select(-tariff_plan_8, -payment_method_post_account,
         -gender_B, -activ_area_4, -activ_chan_9)

train2_dummy_full_rank <- train2_dummy %>% 
  select(-tariff_plan_8, -payment_method_post_account,
         -gender_B, -activ_area_4, -activ_chan_9)

#### Linear Regression ####

mod1 <- lm(y ~ ., train2_dummy_pc_full_rank)

summary(mod1)

RMSE <- sqrt(mean(mod1$residuals ^ 2))
RMSE


ggplot(train2_dummy_pc) +
  geom_point(aes(PC1, mod1$fitted.values), color = "red") +
  geom_point(aes(PC1, y))

#### PCR 10-fold CV ####

train_control <- trainControl(
  method = "cv", 
  number = 10,
  index = folds
)

pcr_cv <- list()
for(i in 1:90) {
  set.seed(123)
  pcr_cv[[i]] <- train(
    y ~ ., 
    data = train3_dummy_full_rank %>% select(1:i, 91:114), 
    method = "lm", 
    trControl = train_control
  )
}

pcr_cv_res <- map_df(pcr_cv, function(x) x$results)

# pcr_cv <- train(
#   y ~ ., 
#   data = train3_dummy_full_rank, 
#   method = "lm", 
#   trControl = train_control
# )

pcr_cv$results
pcr_cv$bestTune
pcr_cv$finalModel
print(mod1_cv)

# 10-fold CV model is the same as standard model because 
# linear regression is low variance - high bias


#### 10-fold CV with preprocessing ####

set.seed(123)

train_control <- trainControl(
  method = "cv", 
  number = 10,
  index = folds,
)

pre_process <- c("YeoJohnson", "center", "scale")
mod1_prep_cv <- train(
  y ~ ., 
  data = train2_dummy_full_rank, 
  method = "lm", 
  trControl = train_control,
  preProcess = pre_process)

mod1_prep_cv$results
mod1_prep_cv$bestTune
mod1_prep_cv$finalModel
print(mod1_prep_cv)


#### 10-fold CV on raw data ####

set.seed(123)

folds <- createFolds(train2$y, k = 10, list = TRUE, returnTrain = TRUE)

train_control <- trainControl(
  method = "cv", 
  number = 10,
  index = folds)

pre_process <- c("YeoJohnson", "center", "scale")

mod1_prep_cv <- train(
  y ~ ., 
  data = train2_dummy_full_rank, 
  method = "lm", 
  trControl = train_control,
  preProcess = pre_process)

mod1_prep_cv$results
mod1_prep_cv$bestTune
mod1_prep_cv$finalModel
print(mod1_prep_cv)




