library(caret)

set.seed(123)

tr_id <- sample(nrow(train2), nrow(train2) * 0.80)
tr <- train2[tr_id, ]
val <- train2[-tr_id, ]


to_remove <- caret::findCorrelation(cor(tr %>% select(where(is.numeric))), names = TRUE)

tr <- tr %>% select(-all_of(to_remove))

folds <- createFolds(tr$y, k = 10, list = TRUE, returnTrain = TRUE)
thresh <- c(0.01, 0.02, 0.03, 0.05, 0.10)

res_full <- data.frame(
  fold = rep(1:length(folds), length(thresh)),
  train_loss = vector("numeric", length(folds) * length(thresh)), 
  valid_loss = vector("numeric", length(folds) * length(thresh)),
  thresh = rep(thresh, each = length(folds)),
  X_sub = 1:(length(folds) * length(thresh))
)

X_sub <- vector("list", length(folds) * length(thresh))


for(i in 1:(nrow(res_full))) {
  fold <- folds[[res_full$fold[i]]]
  
  train_fold <- tr[fold, ]
  valid_fold <- tr[-fold, ]
  
  fit <- ranger(
    formula         = log1p(y) ~ ., 
    data            = train_fold,
    importance      = "impurity"
  )
  
  vimp <- fit$variable.importance
  norm_vimp <- sort(vimp / max(vimp), TRUE)
  X <- names(norm_vimp[which(norm_vimp > res_full$thresh[i])])
  X_sub[[i]] <- X
  
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


# saveRDS(X_sub, "current_version/models/RFE_cor_filter_subsets.rda")
# write_csv(res_full, "current_version/models/RFE_cor_fiter_results.csv")

res_full <- read_csv("current_version/models/RFE_cor_fiter_results.csv")
X_sub <- readRDS("current_version/models/RFE_cor_filter_subsets.rda")

res_full %>% 
  mutate(thresh = as.factor(thresh)) %>% 
  group_by(thresh) %>% 
  summarise(
    mean_val_loss = mean(valid_loss),
    mean_tr_loss = mean(train_loss)) %>% 
  arrange(mean_val_loss)

res_full %>% 
  filter(thresh == 0.1) %>% 
  arrange(valid_loss)








