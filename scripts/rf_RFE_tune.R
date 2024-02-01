library(caret)
library(ranger)

X <- readRDS("current_version/models/RFE_best.rda")

set.seed(123)

tr_id <- sample(nrow(train2), nrow(train2) * 0.80)
tr <- train2[tr_id, ] %>% select(all_of(c(X, "y")))

folds <- createFolds(tr$y, k = 10, list = TRUE, returnTrain = TRUE)


n_features <- length(tr) - 1
hyper_grid <- expand.grid(
  num.trees = c(100, n_features * 10, 500),
  mtry = floor(c(sqrt(n_features), n_features * c(.05, .15, .25, .333, .4))),
  train_loss = NA,
  valid_loss = NA
)

hyper_grid <- apply(hyper_grid, 2, function(x) rep(x, each = 10)) %>% 
  as.data.frame() 

hyper_grid$fold <- rep(1:length(folds), nrow(hyper_grid) / length(folds))


# execute full cartesian grid search

for(i in 1:nrow(hyper_grid)) {
  
  fold <- folds[[hyper_grid$fold[i]]]
  
  train_fold <- tr[fold, ]
  valid_fold <- tr[-fold, ]

  fit <- ranger(
    formula         = log1p(y) ~ ., 
    data            = train_fold, 
    num.trees       = hyper_grid$num.trees[i],
    mtry            = hyper_grid$mtry[i],
  )

  pred_train <- predict(fit, train_fold)$predictions
  pred_valid <- predict(fit, valid_fold)$predictions
  
  hyper_grid$train_loss[i] <- mean((log1p(train_fold$y) - pred_train) ^ 2)
  hyper_grid$valid_loss[i] <- mean((log1p(valid_fold$y) - pred_valid) ^ 2)
  
}


write_csv(hyper_grid, "current_version/models/rf_RFE_hyper_grid.csv")


hyper_grid$model <- rep(1:18, each = 10)


hyper_grid %>% 
  group_by(model) %>% 
  summarize(
    mean_train_loss = mean(train_loss),
    mean_valid_loss = mean(valid_loss),
  ) %>% 
  arrange(mean_valid_loss)


hyper_grid %>% 
  group_by(model) %>% 
  summarize(
    mean_train_loss = mean(train_loss),
    mean_valid_loss = mean(valid_loss),
  ) %>% 
  arrange(mean_valid_loss) %>% 
  head(1)


hyper_grid %>% 
  filter(model == 12)








