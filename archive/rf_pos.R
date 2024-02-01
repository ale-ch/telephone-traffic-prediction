library(randomForest)
library(gbm)
source("current_version/utilities/data_cleaning.R")
rm(test, test2, test2_dummy)
theme_set(theme_minimal())


set.seed(123)
train_id <- sample(nrow(train2), nrow(train2) * 0.8)
train <- train2[train_id, ]
valid <- train2[-train_id, ]


vimp_rf <- readRDS("current_version/models/vimp_rf.rda")

vimp_df <- data.frame(
  var = names(sort(vimp_rf$importance[, "IncNodePurity"], decreasing = TRUE)),
  importance = unname(sort(vimp_rf$importance[, "IncNodePurity"], decreasing = TRUE))
) %>% 
  mutate(norm_importance = importance / max(importance))

vimp_sub1 <- vimp_df %>% 
  filter(norm_importance > 0.01) %>% 
  select(var) %>% 
  unlist() %>% 
  unname()




set.seed(123)

train <- train2 %>% select(all_of(vimp_sub1), y)

set.seed(123)
train_id <- sample(nrow(train), nrow(train) * 0.8)
training <- train[train_id, ]
validation <- train[-train_id, ]

training_pos <- training %>% filter(y > 0)


rf_sub1_pos <- randomForest::randomForest(
  x = training_pos %>% select(-y),
  y = log1p(training_pos$y)
)

saveRDS(rf_sub1_pos, "I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/rf_sub1_pos.rda")
rf_sub1_pos <- readRDS("I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/rf_sub1_pos.rda")



rf_train_fit <- unname(predict(rf_sub1_pos, training_pos))

valid_pos <- validation[which(valid_fit > 0), ]
rf_valid_fit <- predict(rf_sub1_pos, valid_pos)

valid_fit[which(valid_fit > 0)] <- rf_valid_fit

sum((log1p(validation$y) - valid_fit) ^ 2) * 5

cbind(validation$y, expm1(valid_fit)) %>% View()

#######################################

train <- train2 %>% select(all_of(vimp_pos_sub2), y)

set.seed(123)
train_id <- sample(nrow(train), nrow(train) * 0.8)
training <- train[train_id, ]
validation <- train[-train_id, ]

training_pos <- training %>% filter(y > 0)


rf_pos_sub2 <- randomForest::randomForest(
  x = training_pos %>% select(-y),
  y = log1p(training_pos$y)
)

# saveRDS(rf_pos_sub2, "I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/rf_pos_sub2.rda")
rf_pos_sub2 <- readRDS("I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/rf_pos_sub2.rda")


valid_pos <- validation[which(valid_fit > 0), ]
rf_valid_fit <- unname(predict(rf_pos_sub2, training_pos))

valid_fit[which(valid_fit > 0)] <- rf_valid_fit

sum((log1p(validation$y) - valid_fit) ^ 2) * 5












