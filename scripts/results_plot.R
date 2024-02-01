RFE_cor_filter_opt_cv <- read_csv("current_version/models/RFE_cor_filter_opt_cv.csv")
RFE_opt_cv <- read_csv("current_version/models/RFE_opt_cv.csv")
rf_all_cv <- read_csv("current_version/models/rf_all_cv.csv")
rf_RFE_hyper_grid <- read_csv("current_version/models/rf_RFE_hyper_grid.csv")




df1 <- rbind(
  
rf_RFE_hyper_grid %>% 
  group_by(model) %>% 
  summarize(
    mean_cv_val_loss = mean(valid_loss),
    mean_cv_tr_loss = mean(train_loss)
  ) %>% 
  select(mean_cv_tr_loss, mean_cv_val_loss) %>% 
  filter(mean_cv_val_loss == min(mean_cv_val_loss)) %>% 
  mutate(model = "rf_RFE_tune",
         X_sub = "RFE"),

rf_all_cv %>% 
  summarize(
    mean_cv_val_loss = mean(valid_loss),
    mean_cv_tr_loss = mean(train_loss)
  ) %>% 
  mutate(model = "rf_all",
         X_sub = "all"),

RFE_opt_cv %>% 
  group_by(X_sub) %>% 
  summarize(
    mean_cv_val_loss = mean(valid_loss),
    mean_cv_tr_loss = mean(train_loss)
  ) %>% 
  mutate(model = "rf_RFE_opt") %>% 
  filter(mean_cv_val_loss == min(mean_cv_val_loss)),

RFE_cor_filter_opt_cv %>% 
  group_by(X_sub) %>% 
  summarize(
    mean_cv_val_loss = mean(valid_loss),
    mean_cv_tr_loss = mean(train_loss)
  ) %>% 
  mutate(model = "rf_RFE_cor_filter_opt") %>% 
  filter(mean_cv_val_loss == min(mean_cv_val_loss))
) %>% select(-X_sub)


mods <- readRDS("current_version/models/models.rda")

preds_train <- lapply(mods, function(model) predict(model, tr)$predictions)
preds_valid <- lapply(mods, function(model) predict(model, val)$predictions)

losses_train <- sapply(preds_train, function(pred) mean((log1p(tr$y) - pred) ^ 2))
losses_valid <- sapply(preds_valid, function(pred) mean((log1p(val$y) - pred) ^ 2))

df2 <- data.frame(
  model = c("rf_all", "rf_RFE_opt", "rf_RFE_tune", "rf_RFE_cor_filter_opt"),
  train_loss = losses_train,
  valid_loss = losses_valid
)


df <- full_join(df1, df2, by = "model") %>% 
  select(model, all_of(setdiff(names(.), "model")))


df %>% 
  pivot_longer(
    cols = 2:5, 
    values_to = "value",
    names_to = "loss"
  ) %>% 
  ggplot(aes(x = model, y = value, fill = loss, label = value)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  ylab("loss") +
  theme(
    axis.text.x = element_text(face = "bold", size = 8)
  )

  
  
  
  