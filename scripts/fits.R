set.seed(123)

X_RFE <- readRDS("current_version/models/RFE_best.rda")
X_RFE_cor <- readRDS("current_version/models/RFE_cor_filter_best.rda")

tr_id <- sample(nrow(train2), nrow(train2) * 0.80)
tr <- train2[tr_id, ]
val <- train2[-tr_id, ]


fit1 <- ranger(
  formula         = log1p(y) ~ ., 
  data            = tr,
  importance      = "impurity"
)

fit2 <- ranger(
  formula         = log1p(y) ~ ., 
  data            = tr[, c(X_RFE, "y")],
  importance      = "impurity"
)

fit3 <- ranger(
  formula         = log1p(y) ~ ., 
  data            = tr[, c(X_RFE, "y")],
  importance      = "impurity",
  mtry            = 7,
  num.trees       = 500
)

fit4 <- ranger(
  formula         = log1p(y) ~ ., 
  data            = tr[, c(X_RFE_cor, "y")],
  importance      = "impurity"
)


mods <- list(fit1, fit2, fit3, fit4)
names(mods) <- c("rf_all", "rf_RFE_opt", "rf_RFE_tune", "rf_RFE_cor_filter_opt")

# saveRDS(mods, "current_version/models/models.rda")

mods <- readRDS("current_version/models/models.rda")

preds_train <- lapply(mods, function(model) predict(model, tr)$predictions)
preds_valid <- lapply(mods, function(model) predict(model, val)$predictions)

losses_train <- sapply(preds_train, function(pred) mean((log1p(tr$y) - pred) ^ 2))
losses_valid <- sapply(preds_valid, function(pred) mean((log1p(val$y) - pred) ^ 2))

df <- data.frame(
  model = c("rf_all", "rf_RFE_opt", "rf_RFE_tune", "rf_RFE_cor_filter_opt"),
  train_loss = losses_train,
  valid_loss = losses_valid
)















