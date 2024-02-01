library(recipes)
library(caret)
source("current_version/utilities/balance_cv_folds.R")
source("current_version/utilities/data_cleaning.R")
rm(train, test, test2, test2_dummy)
theme_set(theme_minimal())

dummies <- dummyVars(~ ., train2, sep = "_")
train_dummy <- as.data.frame(predict(dummies, newdata = train2))
train_dummy <- map_df(train_dummy, function(x) {
  if(length(levels(as.factor(x))) == 2) as.factor(x) else x
}) %>% as.data.frame()

rm(dummies, train2_dummy)

#### ENET Model - CV ####

set.seed(123)
folds <- createFolds(train_dummy$y, k = 10, list = TRUE, returnTrain = TRUE)

train_control <- trainControl(
  method = "cv", 
  number = 10,
  index = folds
)

hyper_grid <- expand.grid(
  lambda = seq(0, 1, 0.1),
  fraction = c(0.1, 0.5, 0.9)
)

set.seed(123)
enet_cv <- train(
  y ~ .,
  trControl = train_control,
  data = train_dummy,
  method = 'enet',
  tuneGrid = hyper_grid,
  preProcess = c("zv", "YeoJohnson", "center", "scale")
)

s <- length(enet_cv$finalModel$L1norm)
enet_cv_coef <- enet_cv$finalModel$beta.pure[s, ]

enet_cv_coef <- as.data.frame(enet_cv_coef) %>% 
  mutate(var = rownames(.)) %>% 
  rename("coef" = "enet_cv_coef")

rownames(enet_cv_coef) = NULL

plot(enet_cv)

print(enet_cv)

enet_cv_res <- enet_cv$results

# write_csv(enet_cv_res, "current_version/models/tuning_results/enet_cv/enet_cv_res.csv")
# write_csv(enet_cv_coef, "current_version/models/tuning_results/enet_cv/enet_cv_coef.csv")
# saveRDS(enet_cv, "current_version/models/tuning_results/enet_cv/enet_cv.rda")

enet_cv_res <- read_csv("current_version/models/tuning_results/enet_cv/enet_cv_res.csv")
enet_cv_coef <- read_csv("current_version/models/tuning_results/enet_cv/enet_cv_coef.csv")
enet_cv <- readRDS("current_version/models/tuning_results/enet_cv/enet_cv.rda")



#### ENET Model - Balancing - CV ####

# TAKES TOO LONG TO COMPUTE --- IGNORE 
#sub <- 1:1000
#
#set.seed(123)
#folds <- createFolds(train_dummy$y[sub], k = 10, list = TRUE, returnTrain = TRUE)
#
#df <- train_dummy[sub, ] %>% 
#  mutate(
#    y01 = ifelse(y > 0, 1, 0),
#    id = as.numeric(rownames(.))
#  ) %>% 
#  select(id, y01)
#
#p <- seq(0.05, 0.3, 0.01)
#bal_folds <- lapply(p, function(prop) balance_cv_folds(y01 ~ id, df, folds, prop))
#train_control <- lapply(bal_folds, 
#                        function(bal_fold) {
#                          train_control <- trainControl(
#                            method = "cv", 
#                            number = 10,
#                            index = bal_fold$train_folds_bal,
#                            indexOut = bal_fold$valid_folds
#                          )
#                        })
#
#hyper_grid <- expand.grid(
#  lambda = seq(0, 1, 0.1),
#  fraction = c(0.1, 0.5, 0.9)
#)
#
#enet_bal_cv <- list()
#for(i in 1:length(p)) {
#  set.seed(123)
#  enet_bal_cv[[i]] <- train(
#    y ~ ., 
#    data = train_dummy[sub, ], 
#    method = "enet", 
#    trControl = train_control[[i]],
#    tuneGrid = hyper_grid,
#    preProcess = c("zv", "YeoJohnson", "center", "scale")
#  )
#  
#}


#### LASSO -- CV ####

library(glmnet)
library(bestNormalize)
library(grt)

Y <- train2$y
X <- model.matrix(y ~ ., train2)[, -1]

set.seed(123)
folds <- createFolds(train_dummy$y, k = 10, list = FALSE, returnTrain = TRUE)

set.seed(123)
lasso_cv <- cv.glmnet(
  x = X,
  y = Y,
  foldid = folds,
  type.measure = "mse",
  alpha = 1,
  family = "poisson",
  nlambda = 100
)

# saveRDS(lasso_cv, "current_version/models/tuning_results/lasso_cv/lasso_cv.rda")

lasso_cv <- readRDS("current_version/models/tuning_results/lasso_cv/lasso_cv.rda")

plot(lasso_cv, xvar = "lambda")

print(lasso_cv)
lasso_cv$glmnet.fit
lasso_cv$glmnet.fit$beta
lasso_cv$glmnet.fit
lasso_cv$lambda.1se

coef(lasso_cv, s = lasso_cv$lambda.1se)
