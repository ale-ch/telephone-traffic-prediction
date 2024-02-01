factor_cols <- names(which(map_lgl(train2, is.factor)))

train_sub10 <- train2 %>% select(all_of(vimp_sub10), 
                                -all_of(factor_cols), y)

train_int_sub10 <- model.matrix( ~ . ^ 2, data = train_sub10)[, -1] %>% 
  as.data.frame() %>% 
  select(-which(str_detect(names(.), ":y"))) %>% 
  select(-vimp_sub10[which(!(vimp_sub10 %in% factor_cols))])


train <- cbind(train2[, vimp_sub1], train_int_sub10)

#### INTERACTIONS VIMP #### 

set.seed(123)
vimp_rf_sub1_int <- randomForest::randomForest(
  x = train %>% select(-y),
  y = log1p(train$y),
  importance = TRUE
)

# saveRDS(vimp_rf_sub1_int, "I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/vimp_rf_sub1_int.rda")
vimp_rf_sub1_int <- readRDS("I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/vimp_rf_sub1_int.rda")


vimp_rf_sub1_int_df <- data.frame(
  var = names(sort(vimp_rf_sub1_int$importance[, "IncNodePurity"], decreasing = TRUE)),
  importance = unname(sort(vimp_rf_sub1_int$importance[, "IncNodePurity"], decreasing = TRUE))
) %>% 
  mutate(
    norm_importance = importance / max(importance)) 


vimp_rf_sub1_int_df %>% 
  ggplot() +
  geom_col(aes(reorder(var, importance), importance)) +
  coord_flip()

##

vimp_int_sub1 <- vimp_rf_sub1_int_df %>% 
  filter(norm_importance > 0.01) %>% 
  select(var) %>% 
  unlist() %>% 
  unname()


vimp_int_sub2 <- vimp_rf_sub1_int_df %>% 
  filter(norm_importance > 0.02) %>% 
  select(var) %>% 
  unlist() %>% 
  unname()


#### RF INT SUB1 ####

train <- train %>% select(all_of(vimp_int_sub1), y)

set.seed(123)
train_id <- sample(nrow(train), nrow(train) * 0.8)
training <- train[train_id, ]
validation <- train[-train_id, ]

rf_int_sub1 <- randomForest::randomForest(
  x = training %>% select(-y),
  y = log1p(training$y)
)

# saveRDS(rf_int_sub1, "I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/rf_int_sub1.rda")
rf_int_sub1 <- readRDS("I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/rf_int_sub1.rda")


sum((log1p(validation$y) - predict(rf_int_sub1, validation)) ^ 2) * 5

#### RF INT SUB2 ####

train <- train %>% select(all_of(vimp_int_sub2), y)

set.seed(123)
train_id <- sample(nrow(train), nrow(train) * 0.8)
training <- train[train_id, ]
validation <- train[-train_id, ]

rf_int_sub2 <- randomForest::randomForest(
  x = training %>% select(-y),
  y = log1p(training$y)
)

# saveRDS(rf_int_sub2, "I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/rf_int_sub2.rda")
rf_int_sub2 <- readRDS("I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/rf_int_sub2.rda")


sum((log1p(validation$y) - predict(rf_int_sub2, validation)) ^ 2) * 5


#### RF INT SCALED ####
set.seed(123)

vimp_int_sub1 <- vimp_rf_sub1_int_df %>% 
  filter(norm_importance > 0.01) %>% 
  select(var) %>% 
  unlist() %>% 
  unname()

train <- train %>% select(all_of(vimp_int_sub1), y)

train_scaled <- cbind(apply(train %>% select(-factor_cols, -y), 2, scale), 
                      train %>% select(factor_cols, y))

set.seed(123)
train_id <- sample(nrow(train), nrow(train) * 0.8)
training <- train_scaled[train_id, ]
validation <- train_scaled[-train_id, ]


rf_int_scaled <- randomForest::randomForest(
  x = training %>% select(-y),
  y = log1p(training$y)
)

# saveRDS(rf_int_scaled, "I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/rf_int_scaled.rda")
rf_int_scaled <- readRDS("I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/rf_int_scaled.rda")


sum((log1p(validation$y) - predict(rf_int_scaled, validation)) ^ 2) * 5




#### Interactions + Vimp Sub 1% - Main FX -- RF ####

train <- cbind(train2 %>% select(all_of(vimp_sub1), -all_of(vimp_sub10)), 
               train_int_sub10)

set.seed(123)
train_id <- sample(nrow(train), nrow(train) * 0.8)
training <- train[train_id, ]
validation <- train[-train_id, ]

rf_sub1_int_no_main <- randomForest::randomForest(
  x = training %>% select(-y),
  y = log1p(training$y),
  importance = TRUE
)

sum((log1p(validation$y) - predict(rf_sub1_int_no_main, validation)) ^ 2) * 5



# saveRDS(rf_sub1_int_no_main, "I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/rf_sub1_int_no_main.rda")
rf_sub1_int_no_main <- readRDS("I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/rf_sub1_int_no_main.rda")

