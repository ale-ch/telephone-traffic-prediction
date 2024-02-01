library(caret)
library(ranger)

set.seed(123)

tr_id <- sample(nrow(train2), nrow(train2) * 0.80)
tr <- train2[tr_id, ]
val <- train2[-tr_id, ]


folds <- createFolds(tr$y, k = 10, list = TRUE, returnTrain = TRUE)

res_full <- data.frame(
  fold = 1:length(folds),
  train_loss = vector("numeric", length(folds)), 
  valid_loss = vector("numeric", length(folds))
)


for(i in 1:(nrow(res_full))) {
  fold <- folds[[res_full$fold[i]]]
  
  train_fold <- tr[fold, ]
  valid_fold <- tr[-fold, ]
  
  fit <- ranger(
    formula         = log1p(y) ~ ., 
    data            = train_fold,
    importance      = "impurity"
  )
  
  pred_train <- predict(fit, train_fold)$predictions
  pred_valid <- predict(fit, valid_fold)$predictions
  
  res_full$train_loss[i] <- mean((log1p(train_fold$y) - pred_train) ^ 2)
  res_full$valid_loss[i] <- mean((log1p(valid_fold$y) - pred_valid) ^ 2)
}


write_csv(res_full, "current_version/models/rf_all_cv.csv")

res_full %>% 
  summarize(
    mean_val_loss = mean(valid_loss),
    mean_tr_loss = mean(train_loss)) 


