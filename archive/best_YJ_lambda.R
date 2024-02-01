library(purrr)

lambda <- seq(-5, 5, 0.01)
cols <- 97:100
p_value <- matrix(nrow = length(lambda), ncol = length(cols))
ks_statistic <- matrix(nrow = length(lambda), ncol = length(cols))

for(i in cols) {
  for(j in 1:length(lambda)) {
    trans_y <- yeo.johnson(train2[, i], lambda = lambda[j])
    trans_scaled_y <- scale(trans_y)
    trans_scaled_y_pos <- trans_scaled_y[which(trans_scaled_y != min(trans_scaled_y))]
    
    ks_test <- ks.test(trans_scaled_y_pos, "pnorm")
    
    # p_value[j, i - cols[1] - 1] <- ks_test$p.value
    ks_statistic[j, i - cols[1] - 1] <- ks_test$statistic
  }
}

ks_statistic <- cbind(lambda, ks_statistic)

best_lambda <- lambda[apply(ks_statistic[, 2:ncol(ks_statistic)], 2, which.min)]
best_lambda


best_lambdas <- c(
  0.65, 0.56, 0.56, 0.78, 0.59, 0.35, 0.64, 0.57, 1.30, 2.54,
  0.59, 0.57, 0.48, 0.90, 0.60, 0.25, 0.59, 0.52, 1.24, 2.44,
  0.48, 0.84, 0.64, 0.33, 0.55, 0.78, 1.10, 2.50, 0.55, 0.55,
  0.47, 0.94, 0.69, 0.41, 0.57, 0.48, 1.06, 2.28, 0.56, 0.56,
  0.45, 0.95, 0.69, 0.46, 0.51, 0.46, 1.05, 2.63, 0.53, 0.53,
  0.43, 1.05, 0.71, 0.45, 0.47, 0.43, 1.06, 2.28, 0.50, 0.50,
  0.41, 1.05, 0.73, 0.53, 0.42, 0.41, 0.96, 1.98, 0.49, 0.49,
  0.39, 1.12, 0.75, 0.62, 0.41, 0.43, 0.91, 2.53, 0.44, 0.44,
  0.29, 1.21, 0.75, 0.65, 0.32, 0.34, 0.85, 2.16, 0.37, 0.37)

best_lambdas <- as.data.frame(matrix(best_lambdas, ncol = 10, nrow = 9))

names(best_lambdas) <- str_replace_all(names(train2[, 10:19]), "1", "n")

#### `bestNormalize` package #####

library(bestNormalize)

y_yj <- yeojohnson(train2[, 100])
hist(y_yj$x)
hist(y_yj$x.t)

y_bal_yj <- yeojohnson(train_balanced[, 100])
hist(y_bal_yj$x)
hist(y_bal_yj$x.t)

y_bal_std_yj <- yeojohnson(scale(train_balanced[, 100]))
hist(y_bal_std_yj$x)
hist(y_bal_std_yj$x.t)

col10_yj <- yeojohnson(train2[, 10])
hist(col10_yj$x)
hist(col10_yj$x.t)

col10_bal_yj <- yeojohnson(train2[, 10])
hist(col10_bal_yj$x)
hist(col10_bal_yj$x.t)











