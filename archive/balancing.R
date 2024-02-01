library(ROSE)

set.seed(123)

mltools::skewness(as.data.table(train2))

df <- train2 %>% 
  mutate(
    y01 = ifelse(y > 0, 1, 0)
  )

train_balanced <- ROSE::ovun.sample(y01 ~ ., df, p = 0.1)$data

train_balanced %>% 
  filter(y == 0) %>% 
  count() / nrow(train_balanced)

hist(train_balanced$y)
boxplot(train_balanced$y)

trans_y <- yeo.johnson(train_balanced$y, 0.02)
trans_std_y <- scale(trans_y)

ks.test(trans_std_y, "pnorm")

hist(trans_std_y, probability = TRUE)
curve(dnorm(x), add = TRUE)


col10_trans <- yeo.johnson(train_balanced$q01_out_ch_peak, lambda = 0.65)
col10_trans_std <- scale(col10_trans)

col10_trs <- yeo.johnson(train2$q01_out_ch_peak, lambda = 0.65)
col10_trs_std <- scale(col10_trs)

hist(col10_trans_std)
hist(col10_trs_std)
hist(train_balanced[, 10])
hist(train2[, 10])



train_balanced_pc <- stats::prcomp(train_balanced[, 10:99])

cumsum((train_balanced_pc$sdev ^ 2) / sum(train_balanced_pc$sdev ^ 2) * 100)


