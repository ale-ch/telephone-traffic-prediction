tr_anom <- tr %>% 
  mutate(anomaly = 0)
tr_anom$anomaly[which(train$y01 != gbm_train_fit)] <- 1

train_anom <- train %>% 
  mutate(anomaly = 0)
train_anom$anomaly[which(train$y01 != gbm_train_fit)] <- 1


train <- train_anom %>% 
  select(-y01)

set.seed(123)
anom_rf <- randomForest::randomForest(
  x = train %>% select(-anomaly),
  y = as.factor(train$anomaly)
)


pred <- predict(anom_rf, train)
actual <- train$anomaly
table(actual, pred)
(table(actual, pred)[1, 2] + 
    table(actual, pred)[2, 1]) / nrow(train)





valid_pred <- predict(anom_rf, valid)

valid$anomaly <- valid_pred
val$anomaly <- valid_pred





train <- tr_anom %>% 
  select(all_of(vimp_sub1), y, anomaly)


set.seed(123)
rf1 <- randomForest::randomForest(
  x = train %>% select(-y),
  y = log1p(train$y)
)

sum((log1p(train$y) - predict(rf1, train)) ^ 2) * 1.25



valid <- val %>% 
  select(all_of(vimp_sub1), y)
valid$anomaly <- as.numeric(predict(anom_rf, valid))

sum((log1p(valid$y) - predict(rf1, valid)) ^ 2) * 5















