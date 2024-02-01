#### RF - PCA ####

##### All variables PCA #####

train <- cbind(train2_pc, train2 %>% 
                 select(all_of(names(which(map_lgl(train2, is.factor)))), y))


set.seed(123)
vimp_pc_rf <- randomForest::randomForest(
  x = train %>% select(-y),
  y = log1p(train$y),
  importance = TRUE
)

# saveRDS(vimp_pc_rf, "I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/vimp_pc_rf.rda")
vimp_pc_rf <- readRDS("I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/vimp_pc_rf.rda")


vimp_pc_df <- data.frame(
  var = names(sort(vimp_pc_rf$importance[, "IncNodePurity"], decreasing = TRUE)),
  importance = unname(sort(vimp_pc_rf$importance[, "IncNodePurity"], decreasing = TRUE))
) %>% 
  mutate(norm_importance = importance / max(importance))


vimp_pc_df %>% 
  ggplot() +
  geom_col(aes(reorder(var, importance), importance)) +
  coord_flip()


vimp_pc_df[1:22, "var"]

set.seed(123)
train_id <- sample(nrow(train), nrow(train) * 0.8)
training <- train[train_id, ]
validation <- train[-train_id, ]

pc_rf <- randomForest::randomForest(
  x = training %>% select(all_of(vimp_pc_df[1:22, "var"]), -y),
  y = log1p(training$y)
)

sum((log1p(validation$y) - predict(pc_rf, validation)) ^ 2) * 5

# saveRDS(vimp_pc_rf, "I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/pc_rf.rda")
pc_rf <- readRDS("I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/pc_rf.rda")


##### Important Variables PCA #####

factor_cols <- names(which(map_lgl(train2, is.factor)))
vimp_sub1_num <- vimp_sub1[which(!(vimp_sub1 %in% factor_cols))]

train_sub1_pca <- prcomp(train2[, vimp_sub1_num], center = TRUE, scale = TRUE)
train_sub1_pca <- as.data.frame(train_sub1_pca$x)

train <- cbind(train_sub1_pca, train2[, factor_cols], y = train2[, "y"])

set.seed(123)
vimp_sub1_pc_rf <- randomForest::randomForest(
  x = train %>% select(-y),
  y = log1p(train$y),
  importance = TRUE
)

# saveRDS(vimp_sub1_pc_rf, "I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/vimp_sub1_pc_rf.rda")
vimp_sub1_pc_rf <- readRDS("I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/vimp_sub1_pc_rf.rda")


vimp_sub1_pc_df <- data.frame(
  var = names(sort(vimp_sub1_pc_rf$importance[, "IncNodePurity"], decreasing = TRUE)),
  importance = unname(sort(vimp_sub1_pc_rf$importance[, "IncNodePurity"], decreasing = TRUE))
) %>% 
  mutate(norm_importance = importance / max(importance))


vimp_sub1_pc_df %>% 
  ggplot() +
  geom_col(aes(reorder(var, importance), importance)) +
  coord_flip()



selected <- vimp_sub1_pc_df %>% 
  filter(norm_importance > 0.03) %>% 
  select(var) %>% 
  unlist() %>% 
  unname()

set.seed(123)
train_id <- sample(nrow(train), nrow(train) * 0.8)
training <- train[train_id, ]
validation <- train[-train_id, ]

sub_pc_rf <- randomForest::randomForest(
  x = training %>% select(all_of(selected), -y),
  y = log1p(training$y)
)

# saveRDS(sub_pc_rf, "I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/sub_pc_rf.rda")
sub_pc_rf <- readRDS("I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/sub_pc_rf.rda")


sum((log1p(validation$y) - predict(sub_pc_rf, validation)) ^ 2) * 5



