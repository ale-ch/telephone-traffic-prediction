#### Interactions - PCA - RF ####

train_sub5 <- train2 %>% select(all_of(vimp_sub5), 
                                -all_of(factor_cols), y)

train_int_sub5 <- model.matrix( ~ . ^ 2, data = train_sub5)[, -1] %>% 
  as.data.frame() %>% 
  select(-which(str_detect(names(.), ":y"))) %>% 
  select(-vimp_sub5[which(!(vimp_sub5 %in% factor_cols))])

train <- cbind(train_int_sub5, train2 %>% 
                 select(all_of(vimp_sub1[which(!(vimp_sub1 %in% factor_cols))])))

train_pca <- prcomp(train %>% select(-y), center = TRUE, scale = TRUE)

sds <- train_pca$sdev
pct_var <- (sds ^ 2) / sum(sds ^ 2) * 100
pct_var

which(pct_var >= 0.05)

train_pca <- as.data.frame(train_pca$x[, which(pct_var >= 0.05)])
train_pca <- cbind(train_pca, train2 %>% select(y, all_of(factor_cols)))


set.seed(123)
train_id <- sample(nrow(train_pca), nrow(train_pca) * 0.8)
training <- train_pca[train_id, ]
validation <- train_pca[-train_id, ]

set.seed(123)
int_pc_rf <- randomForest::randomForest(
  x = training %>% select(-y),
  y = log1p(training$y),
  importance = TRUE
)

sum((log1p(validation$y) - predict(int_pc_rf, validation)) ^ 2) * 5

# saveRDS(int_pc_rf, "I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/int_pc_rf.rda")
int_pc_rf <- readRDS("I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/int_pc_rf.rda")


#### Interactions - PCA - RF - version 2  ####

train_sub5 <- train2 %>% select(all_of(vimp_sub5), 
                                -all_of(factor_cols), y)

train_int_sub5 <- model.matrix( ~ . ^ 2, data = train_sub5)[, -1] %>% 
  as.data.frame() %>% 
  select(-which(str_detect(names(.), ":y"))) %>% 
  select(-vimp_sub5[which(!(vimp_sub5 %in% factor_cols))])


int_sub5_pca <- prcomp(train_int_sub5 %>% select(-y), center = TRUE, scale = TRUE)

sds <- int_sub5_pca$sdev
pct_var <- (sds ^ 2) / sum(sds ^ 2) * 100
pct_var

which(pct_var >= 0.20)

int_sub5_pc <- as.data.frame(int_sub5_pca$x[, which(pct_var >= 0.05)])
train <- cbind(int_sub5_pc, train2 %>% select(y, all_of(vimp_sub1)))



set.seed(123)
train_id <- sample(nrow(train), nrow(train) * 0.8)
training <- train[train_id, ]
validation <- train[-train_id, ]

set.seed(123)
int_pc_rf2 <- randomForest::randomForest(
  x = training %>% select(-y),
  y = log1p(training$y),
  importance = TRUE
)

sum((log1p(validation$y) - predict(int_pc_rf2, validation)) ^ 2) * 5

# saveRDS(int_pc_rf2, "I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/int_pc_rf2.rda")
int_pc_rf2 <- readRDS("I:/My Drive/Uni/CLAMSES/Anno 2/DATA_SCIENCE/DATA_MINING/DM-project/DM_project/current_version/models/int_pc_rf2.rda")









