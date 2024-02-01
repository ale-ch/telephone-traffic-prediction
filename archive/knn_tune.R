library(kknn)
library(recipes)
library(tidyverse)

tr <- train2 %>% select(all_of(vimp_sub1), y)

blueprint <- recipe(y ~ ., data = tr) %>%
  step_nzv(all_nominal()) %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
  step_nzv() %>% 
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes())

tr <- bake(prep(blueprint), tr)

set.seed(123)
train_id <- sample(nrow(tr), nrow(tr) * 0.8)
train <- tr[train_id, ]
valid <- tr[-train_id, ]


hyper_grid <- expand.grid(
  k = floor(seq(1, nrow(train) / 3, length.out = 20)),
  kernel = c("rectangular", "triangular", "epanechnikov",
             "biweight", "triweight", "cos", "inv",  
             "gaussian", "rank", "optimal"),
  valid_loss = NA
)


for(i in 1:nrow(hyper_grid)) {
  
  print(paste("Model", i, "is being trained."))
  
  knn_fit <- kknn(
    formula = log1p(y) ~ ., 
    train = train, 
    test = valid, 
    k = hyper_grid$k[i],
    kernel = as.character(hyper_grid$kernel[i])
  )
  
  print(paste("Model", i, "has been trained."))
  
  valid_loss <- sum((log1p(valid$y) - knn_fit$fitted.values) ^ 2) * 5
  hyper_grid$valid_loss[i] <- valid_loss
  
  write_csv(hyper_grid, "current_version/models/tuning_results/knn_tune_hyper_grid.csv")
  
  print(hyper_grid[i, ])
}



hg_knn <- read_csv("current_version/models/tuning_results/knn_tune_hyper_grid.csv")


knn_fit <- kknn(
  formula = log1p(y) ~ ., 
  train = tr, 
  test = tr, 
  k = 215,
  kernel = "triweight"
)

sum((log1p(tr$y) - knn_fit$fitted.values) ^ 2)


