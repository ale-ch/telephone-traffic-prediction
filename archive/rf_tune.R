library(ranger)

tr <- train2 %>% select(all_of(vimp_sub1), y)
set.seed(123)
train_id <- sample(nrow(tr), nrow(tr) * 0.8)
train <- tr[train_id, ]
valid <- tr[-train_id, ]


n_features <- length(train) - 1
hyper_grid <- expand.grid(
  num.trees = c(100, n_features * 10, 500),
  mtry = floor(n_features * c(.05, .15, .25, .333, .4)),
  min.node.size = c(1, 3, 5, 10), 
  sample.fraction = c(.5, .60, .8),
  replace = c(TRUE, FALSE),
  train_loss = NA,
  valid_loss = NA
)

# execute full cartesian grid search
print(Sys.time())
for(i in 1:nrow(hyper_grid)) {
  tic()
  print(paste("Model", i, "is being trained."))
  
  fit <- ranger(
    formula         = log1p(y) ~ ., 
    data            = train, 
    num.trees       = hyper_grid$num.trees[i],
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$min.node.size[i], 
    sample.fraction = hyper_grid$sample.fraction[i],
    replace         = hyper_grid$replace[i]
  )
  
  print(paste("Model", i, "has been trained successfully."))
  toc()
  
  pred_train <- predict(fit, train)$predictions
  pred_valid <- predict(fit, valid)$predictions
  
  hyper_grid$train_loss[i] <- sum((log1p(train$y) - pred_train) ^ 2) * 1.25
  hyper_grid$valid_loss[i] <- sum((log1p(valid$y) - pred_valid) ^ 2) * 5
  
  write_csv(hyper_grid, "current_version/models/tuning_results/rf_tune_hyper_grid.csv")
}
print(Sys.time())
   
hyper_grid <- read_csv("current_version/models/tuning_results/rf_tune_hyper_grid.csv")


# assess top 10 models
hyper_grid %>%
  arrange(valid_loss)
