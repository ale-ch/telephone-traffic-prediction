library(ranger)

tr <- train2 %>% select(all_of(X_RFE), y)
set.seed(123)
train_id <- sample(nrow(tr), nrow(tr) * 0.8)
tr <- tr[train_id, ]



pred_train <- predict(fit3, tr)$predictions

df <- data.frame(
  log1p_y = log1p(tr$y), 
  log1p_pred_train = pred_train, 
  sqerr = (log1p(tr$y) - pred_train) ^ 2) %>% 
  mutate(
    y01 = ifelse(log1p_y > 0, 1, 0),
    pred_01 = ifelse(round(expm1(log1p_pred_train)) > 0, 1, 0)
  )

table(actual = df$y01, pred = df$pred_01)




data.frame(
  var = names(fit3$variable.importance),
  importance = unname(fit3$variable.importance)
) %>% 
  ggplot() +
  geom_col(aes(reorder(var, importance), importance, fill = importance)) +
  coord_flip() +
  xlab("var") +
  ggtitle("rf_RFE_tuned")




