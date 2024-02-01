source("current_version/utilities/data_cleaning.R")
rm(test, test2, test2_dummy, train2_dummy)
theme_set(theme_minimal())

library(gbm)

train <- train2 %>% select(all_of(vimp_sub1), y)

set.seed(123)
train_id <- sample(nrow(train), nrow(train) * 0.8)
training <- train[train_id, ]
validation <- train[-train_id, ]

training <- training %>% mutate(y01 = ifelse(y > 0, 1, 0))
validation <- validation %>% mutate(y01 = ifelse(y > 0, 1, 0))

gbm_sub1 <- gbm(
  y01 ~ .,
  data = training,
  distribution = "bernoulli",
  n.trees = 1000
)

range(gbm_sub1$fit)
summary(gbm_sub1)

train_fit <- ifelse(predict(gbm_sub1, training) > 0, 1, 0)
valid_fit <- ifelse(predict(gbm_sub1, validation) > 0, 1, 0)

valid_y01 <- validation$y01
table(valid_y01, valid_fit)

#### BERNOULLI GBM -- 10-FOLD CV ####


train <- train2 %>% select(all_of(vimp_sub1), y)

set.seed(123)
train_id <- sample(nrow(train), nrow(train) * 0.8)
training <- train[train_id, ]
validation <- train[-train_id, ]

training <- training %>% mutate(y01 = ifelse(y > 0, 1, 0))
validation <- validation %>% mutate(y01 = ifelse(y > 0, 1, 0))

set.seed(123)
gbm_bern_cv <- gbm(
  y01 ~ .,
  data = training,
  distribution = "bernoulli",
  n.trees = 1000,
  cv.folds = 10
)

# saveRDS(gbm_bern_cv, "I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/gbm_bern_cv.rda")
gbm_bern_cv <- readRDS("I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/gbm_bern_cv.rda")

plot(gbm_bern_cv$cv.error, type = "l")

range(gbm_bern_cv$fit)
summary(gbm_bern_cv)

train_fit <- ifelse(predict(gbm_bern_cv, training) > 0, 1, 0)
valid_fit <- ifelse(predict(gbm_bern_cv, validation) > 0, 1, 0)

valid_y01 <- validation$y01
table(valid_y01, valid_fit)



