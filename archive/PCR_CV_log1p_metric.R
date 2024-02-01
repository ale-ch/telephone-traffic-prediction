library(recipes)
library(caret)
source("current_version/utilities/balance_cv_folds.R")
source("current_version/utilities/data_cleaning.R")
rm(test, test2, test2_dummy)
theme_set(theme_minimal())

set.seed(123)
folds <- createFolds(train$y, k = 10, list = TRUE, returnTrain = TRUE)

dummies <- dummyVars(~ ., train2, sep = "_", fullRank = TRUE)
train_full_rank <- as.data.frame(predict(dummies, newdata = train2))
train_full_rank[, c(1:9, 11:23)] <- 
  map(train_full_rank[, c(1:9, 11:23)], as.factor)


customSummary <- function(data, lev = NULL, model = NULL) {
  pred <- data[, "pred"]
  obs <- data[, "obs"]
  
  pred_nz <- ifelse(pred < 0, 0, pred)
  
  log1p_SSE <- sum((log1p(obs) - log1p(pred_nz)) ^ 2)
  RMSE <- caret::RMSE(pred_nz, obs)
  c(log1p_SSE = log1p_SSE, RMSE = RMSE)
}


#### PCR - CV ####
train_control <- trainControl(
  method = "cv", 
  number = 10,
  index = folds,
  #predictionBounds = c(0, NA),
  summaryFunction = customSummary
)

set.seed(123)
pcr_cv <- train(
  y ~ ., 
  data = train_full_rank, 
  method = "pcr", 
  trControl = train_control,
  preProcess = c("center", "scale"),
  tuneLength = 90
)

pcr_cv_res <- pcr_cv$results
pcr_cv_res

pred <- pcr_cv$finalModel$fitted.values[,,5]
pred_nz <- ifelse(pred < 0, 0, pred)

sum((log1p(train$y) - log1p(pred_nz)) ^ 2)
