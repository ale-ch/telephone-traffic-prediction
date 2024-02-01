#### BOOTSTRAP MISCLASSIFIED DATA ####
train <- tr %>% 
  select(all_of(vimp_sub1), y) %>% 
  mutate(y01 = ifelse(y > 0, 1, 0)) %>% 
  select(-y)

valid <- val %>% 
  select(all_of(vimp_sub1), y) %>% 
  mutate(y01 = ifelse(y > 0, 1, 0)) %>% 
  select(-y)


rownames(miscl) <- 1:nrow(miscl)

miscl_id <- as.integer(rownames(miscl))

set.seed(123)
miscl_boot_id <- sample(miscl_id, 1000, TRUE)

miscl_boot <- miscl[miscl_boot_id, ] %>% select(-y, -gbm_train_fit)

train <- rbind(train, miscl_boot)

set.seed(123)
gbm_boot <- gbm(
  y01 ~ .,
  data = train,
  shrinkage = 0.01,
  bag.fraction = 0.5,
  interaction.depth = 1,
  n.minobsinnode = 10,
  train.fraction = 1,
  distribution = "bernoulli",
  verbose = TRUE,
  n.trees = 1000
)


gbm_valid_fit <- ifelse(predict(gbm_boot, valid) > 0, 1, 0)
gbm_train_fit <- ifelse(predict(gbm_boot, train) > 0, 1, 0)

train_y01 <- train$y01
table(train_y01, gbm_train_fit)
(table(train_y01, gbm_train_fit)[1, 2] + 
    table(train_y01, gbm_train_fit)[2, 1]) / nrow(train)

valid_y01 <- valid$y01
table(valid_y01, gbm_valid_fit)
(table(valid_y01, gbm_valid_fit)[1, 2] + 
    table(valid_y01, gbm_valid_fit)[2, 1]) / nrow(valid)



