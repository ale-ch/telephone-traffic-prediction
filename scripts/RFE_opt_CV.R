library(ranger)
library(caret)

set.seed(123)

tr_id <- sample(nrow(train2), nrow(train2) * 0.80)
tr <- train2[tr_id, ]
val <- train2[-tr_id, ]

set.seed(1)
folds <- createFolds(tr$y, k = 10, list = TRUE, returnTrain = TRUE)

X_sub <- readRDS("current_version/models/RFE_subsets.rda")

X_opt <- X_sub[41:50]

res_full <- data.frame(
  fold = rep(1:length(folds), length(X_opt)),
  train_loss = vector("numeric", length(folds) * length(X_opt)), 
  valid_loss = vector("numeric", length(folds) * length(X_opt)),
  X_sub = rep(1:10, 10)
)


for(i in 1:(nrow(res_full))) {
  fold <- folds[[res_full$fold[i]]]
  X <- X_opt[[res_full$X_sub[i]]]
  
  train_fold <- tr[fold, ]
  valid_fold <- tr[-fold, ]
  
  fit_sub <- ranger(
    formula         = log1p(y) ~ ., 
    data            = train_fold[, c(X, "y")],
    importance      = "impurity"
  )
  
  pred_train <- predict(fit_sub, train_fold)$predictions
  pred_valid <- predict(fit_sub, valid_fold)$predictions
  
  res_full$train_loss[i] <- mean((log1p(train_fold$y) - pred_train) ^ 2)
  res_full$valid_loss[i] <- mean((log1p(valid_fold$y) - pred_valid) ^ 2)
}


# write_csv(res_full, "current_version/models/RFE_opt_cv.csv")
res_full <- read_csv("current_version/models/RFE_opt_cv.csv")

res_full %>% 
  mutate(X_sub = as.factor(X_sub)) %>% 
  group_by(X_sub) %>% 
  summarise(
    mean_val_loss = mean(valid_loss),
    mean_tr_loss = mean(train_loss)) %>% 
  arrange(mean_val_loss)


X_opt_id <- res_full %>% 
  mutate(X_sub = as.factor(X_sub)) %>% 
  group_by(X_sub) %>% 
  summarise(
    mean_val_loss = mean(valid_loss),
    mean_tr_loss = mean(train_loss)) %>% 
  arrange(mean_val_loss) %>% 
  select(X_sub) %>% 
  head(1) %>% 
  as.numeric()


X <- X_opt[[X_opt_id]]

# saveRDS(X, "current_version/models/RFE_best.rda")
X <- readRDS("current_version/models/RFE_best.rda")

fit_opt <- ranger(
  formula         = log1p(y) ~ ., 
  data            = train2[, c(X, "y")],
  importance      = "impurity"
)







