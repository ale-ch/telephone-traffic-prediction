thresh <- 0.90
hi_corr_vars <- corr_df %>% 
  mutate(value = abs(value)) %>% 
  filter(var1 != "y", var2 != "y", value >= thresh) %>% 
  select(var2) %>% 
  unlist() %>% 
  unname() %>% 
  unique()

not_in_train2 <- which(!hi_corr_vars %in% names(train2))
if(length(not_in_train2) > 0) {
  hi_corr_vars <- hi_corr_vars[-not_in_train2]
}

tr <- train2 %>% select(-all_of(hi_corr_vars))

set.seed(123)
train_id <- sample(nrow(tr), nrow(tr) * 0.8)
train <- tr[train_id, ]
valid <- tr[-train_id, ]


#### VIMP ####
set.seed(123)
fit <- randomForest::randomForest(
  x = train %>% select(-y),
  y = log1p(train$y),
  importance = TRUE
)

sum((log1p(train$y) - predict(fit, train)) ^ 2) * 1.25
sum((log1p(valid$y) - predict(fit, valid)) ^ 2) * 5


fit_df <- data.frame(
  var = names(sort(fit$importance[, "IncNodePurity"], decreasing = TRUE)),
  importance = unname(sort(fit$importance[, "IncNodePurity"], decreasing = TRUE))
) %>% 
  mutate(norm_importance = importance / max(importance))

fit_df %>% 
  ggplot() +
  geom_col(aes(reorder(var, importance), importance)) +
  coord_flip()




