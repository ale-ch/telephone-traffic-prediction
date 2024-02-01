library(ranger)
# source("current_version/utilities/balance_cv_folds.R")

tr <- train2 %>% select(all_of(vimp_sub1), y)
set.seed(123)
train_id <- sample(nrow(tr), nrow(tr) * 0.8)
train <- tr[train_id, ]
valid <- tr[-train_id, ]

p <- seq(0, 0.7, by = 0.05)

train_list <- list()
for(i in 1:length(p)) {
  prob <- p[i]
  
  train_tmp <- train %>% mutate(id = as.integer(rownames(.)))
  tmp <- train %>% mutate(
    y = ifelse(y > 0, 1, 0),
    id = as.integer(rownames(.)))

  tmp <- ovun.sample(y ~ ., tmp, p = prob, seed = 123)$data
  
  train_tmp <- inner_join(tmp, train_tmp, by = "id")
  
  train_tmp <- train_tmp[, !duplicated(as.list(train_tmp))] 
  train_tmp <- train_tmp %>% select(-y.x, -id)
  names(train_tmp) <- str_remove(names(train_tmp), "\\.x|\\.y")
  
  train_list[[i]] <- train_tmp
}


hyper_grid <- expand.grid(
  p = p,
  train_loss = NA,
  valid_loss = NA
)

for(i in 1:length(train_list)) {
  train <- train_list[[i]]
  
  fit <- ranger(
    formula         = log1p(y) ~ ., 
    data            = train, 
  )
  
  pred_train <- predict(fit, train)$predictions
  pred_valid <- predict(fit, valid)$predictions
  
  hyper_grid$train_loss[i] <- sum((log1p(train$y) - pred_train) ^ 2) * 1.25
  hyper_grid$valid_loss[i] <- sum((log1p(valid$y) - pred_valid) ^ 2) * 5
}

best_p <- (hyper_grid %>% 
    arrange(valid_loss) %>% 
    select(p) %>% 
    head(1))$p

hyper_grid %>%
  pivot_longer(
    cols = 2:3,
    names_to = "loss",
    values_to = "value"
  ) %>% 
  ggplot() +
  geom_line(aes(p, value, color = loss)) +
  geom_vline(xintercept = best_p,
             linetype = "dotted") +
  scale_x_continuous(breaks = p) +
  ylab("loss") +
  theme(
    axis.text.x = element_text(angle = 45),
    legend.title = element_blank()
  )

  

  
  