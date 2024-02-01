library(recipes)
library(caret)
library(glmnet)

# source("current_version/utilities/balance_cv_folds.R")
source("current_version/utilities/data_cleaning.R")

rm(test, test2, test2_dummy)
theme_set(theme_minimal())


set.seed(123)
train_id <- sample(nrow(train2), nrow(train2) * 0.8)
train <- train2[train_id, ]
valid <- train2[-train_id, ]


set.seed(123)
vimp_rf <- randomForest::randomForest(
  x = train2 %>% select(-y),
  y = log1p(train2$y),
  importance = TRUE
)

# saveRDS(vimp_rf, "current_version/models/vimp_rf.rda")
vimp_rf <- readRDS("current_version/models/vimp_rf.rda")

vimp_df <- data.frame(
  var = names(sort(vimp_rf$importance[, "IncNodePurity"], decreasing = TRUE)),
  importance = unname(sort(vimp_rf$importance[, "IncNodePurity"], decreasing = TRUE))
) %>% 
  mutate(norm_importance = importance / max(importance))


vimp_df %>% 
  ggplot() +
  geom_col(aes(reorder(var, importance), importance)) +
  coord_flip()

###############

vimp_sub10 <- vimp_df %>% 
  filter(norm_importance > 0.1) %>% 
  select(var) %>% 
  unlist() %>% 
  unname()

vimp_sub5 <- vimp_df %>% 
  filter(norm_importance > 0.05) %>% 
  select(var) %>% 
  unlist() %>% 
  unname() 

vimp_sub3 <- vimp_df %>% 
  filter(norm_importance > 0.03) %>% 
  select(var) %>% 
  unlist() %>% 
  unname()

vimp_sub2 <- vimp_df %>% 
  filter(norm_importance > 0.02) %>% 
  select(var) %>% 
  unlist() %>% 
  unname()

vimp_sub1 <- vimp_df %>% 
  filter(norm_importance > 0.01) %>% 
  select(var) %>% 
  unlist() %>% 
  unname()


########

set.seed(123)

rf_vimp_sub10 <- randomForest::randomForest(
  x = train %>% select(all_of(vimp_sub10), -y),
  y = log1p(train$y)
)


sum((predict(rf_vimp_sub10, valid) - log1p(valid$y)) ^ 2)


set.seed(123)

rf_vimp_sub5 <- randomForest::randomForest(
  x = train %>% select(all_of(vimp_sub5), -y),
  y = log1p(train$y)
)

sum((log1p(valid$y) - predict(rf_vimp_sub5, valid)) ^ 2)


set.seed(123)

rf_vimp_sub3 <- randomForest::randomForest(
  x = train %>% select(all_of(vimp_sub3), -y),
  y = log1p(train$y)
)

sum((log1p(valid$y) - predict(rf_vimp_sub3, valid)) ^ 2)


set.seed(123)

rf_vimp_sub2 <- randomForest::randomForest(
  x = train %>% select(all_of(vimp_sub2), -y),
  y = log1p(train$y)
)

sum((log1p(valid$y) - predict(rf_vimp_sub2, valid)) ^ 2)

set.seed(123)

rf_vimp_sub1 <- randomForest::randomForest(
  x = train %>% select(all_of(vimp_sub1), -y),
  y = log1p(train$y)
)

sum((log1p(valid$y) - predict(rf_vimp_sub1, valid)) ^ 2)


# saveRDS(rf_vimp_sub10, "current_version/models/rf_vimp_sub10.rda")
# saveRDS(rf_vimp_sub5, "current_version/models/rf_vimp_sub5.rda")
# saveRDS(rf_vimp_sub3, "current_version/models/rf_vimp_sub3.rda")
# saveRDS(rf_vimp_sub2, "current_version/models/rf_vimp_sub2.rda")
# saveRDS(rf_vimp_sub1, "current_version/models/rf_vimp_sub1.rda")


rf_vimp_sub10 <- readRDS("current_version/models/rf_vimp_sub10.rda")
rf_vimp_sub5 <- readRDS("current_version/models/rf_vimp_sub5.rda")
rf_vimp_sub3 <- readRDS("current_version/models/rf_vimp_sub3.rda")
rf_vimp_sub2 <- readRDS("current_version/models/rf_vimp_sub2.rda")
rf_vimp_sub1 <- readRDS("current_version/models/rf_vimp_sub1.rda")



sum((log1p(valid$y) - predict(rf_vimp_sub10, valid)) ^ 2)  * 5
sum((log1p(valid$y) - predict(rf_vimp_sub5, valid)) ^ 2) * 5
sum((log1p(valid$y) - predict(rf_vimp_sub3, valid)) ^ 2) * 5
sum((log1p(valid$y) - predict(rf_vimp_sub2, valid)) ^ 2) * 5
sum((log1p(valid$y) - predict(rf_vimp_sub1, valid)) ^ 2) * 5

sum((log1p(train$y) - predict(rf_vimp_sub10, train)) ^ 2)  * 1.25
sum((log1p(train$y) - predict(rf_vimp_sub5, train)) ^ 2) * 1.25
sum((log1p(train$y) - predict(rf_vimp_sub3, train)) ^ 2) * 1.25
sum((log1p(train$y) - predict(rf_vimp_sub2, train)) ^ 2) * 1.25
sum((log1p(train$y) - predict(rf_vimp_sub1, train)) ^ 2) * 1.25

####################
library(heatmaply)


heatmaply_cor(
  cor(train2 %>% 
        select(all_of(vimp_sub1), -which(sapply(train2, is.factor)))),
  symm = TRUE,
  cexRow = .0001, 
  cexCol = .0001, 
  branches_lwd = .1,
  showticklabels = c(FALSE, FALSE)
)

heatmaply_cor(
  cor(train2 %>% 
        select(all_of(vimp_sub2), -which(sapply(train2, is.factor)))),
  symm = TRUE,
  cexRow = .0001, 
  cexCol = .0001, 
  branches_lwd = .1,
  showticklabels = c(FALSE, FALSE)
)

heatmaply_cor(
  cor(train2 %>% 
        select(all_of(vimp_sub3), -which(sapply(train2, is.factor)))),
  symm = TRUE,
  cexRow = .0001, 
  cexCol = .0001, 
  branches_lwd = .1,
  showticklabels = c(FALSE, FALSE)
)


heatmaply_cor(
  cor(train2 %>% 
        select(all_of(vimp_sub5), -which(sapply(train2, is.factor)))),
  symm = TRUE,
  cexRow = .0001, 
  cexCol = .0001, 
  branches_lwd = .1,
  showticklabels = c(FALSE, FALSE)
)

heatmaply_cor(
  cor(train2 %>% 
        select(all_of(vimp_sub10), -which(sapply(train2, is.factor)))),
  symm = TRUE,
  cexRow = .0001, 
  cexCol = .0001, 
  branches_lwd = .1,
  showticklabels = c(FALSE, FALSE)
)


