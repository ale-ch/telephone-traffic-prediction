library(recipes)
library(caret)
source("current_version/utilities/balance_cv_folds.R")
source("current_version/utilities/data_cleaning.R")
rm(test, test2, test2_dummy)
theme_set(theme_minimal())

set.seed(123)
folds <- createFolds(train$y, k = 10, list = TRUE, returnTrain = TRUE)

dummies <- dummyVars(~ ., train2, sep = "_", fullRank = TRUE)
train_full_rank <- as.data.frame(predict(dummies, newdata = train2))
train_full_rank[, c(1:9, 11:23)] <- 
  map(train_full_rank[, c(1:9, 11:23)], as.factor)

#### PCR - CV ####
train_control <- trainControl(
  method = "cv", 
  number = 10,
  index = folds
)

set.seed(123)
pcr_cv <- train(
  y ~ ., 
  data = train_full_rank, 
  method = "pcr", 
  trControl = train_control,
  preProcess = c("center", "scale"),
  tuneLength = 90
)

pcr_cv_res <- pcr_cv$results

# write_csv(pcr_cv_res, "current_version/models/tuning_results/pcr_cv.csv")

pcr_cv_res <- read_csv("current_version/models/tuning_results/pcr_cv.csv")

pcr_cv_res %>% 
  ggplot() +
  geom_line(aes(ncomp, RMSE))


#### PCR with Balancing - CV ####
df <- train_full_rank %>% 
  mutate(
    y01 = ifelse(y > 0, 1, 0),
    id = as.numeric(rownames(.))
  ) %>% 
  select(id, y01)

p <- seq(0.05, 0.3, 0.01)
bal_folds <- lapply(p, function(prop) balance_cv_folds(y01 ~ id, df, folds, prop))
train_control <- lapply(bal_folds, 
                        function(bal_fold) {
                          train_control <- trainControl(
                            method = "cv", 
                            number = 10,
                            index = bal_fold$train_folds_bal,
                            indexOut = bal_fold$valid_folds
                          )
                        })

mod_cv <- list()
for(i in 1:length(p)) {
  set.seed(123)
  mod_cv[[i]] <- train(
    y ~ ., 
    data = train_full_rank, 
    method = "pcr", 
    trControl = train_control[[i]],
    preProcess = c("center", "scale"),
    tuneLength = 50
  )
  
}


pcr_bal_cv_res <- cbind(p = p, 
             map_df(mod_cv, 
                    function(mod) mod$results[as.integer(mod$bestTune), ]))

best_p <- (pcr_bal_cv_res %>% 
  filter(RMSE == min(RMSE)))$p

pcr_bal_cv_best <- mod_cv[[which(p == best_p)]]$results %>% mutate(p = best_p)

pcr_bal_cv_res %>% 
  ggplot() +
  geom_line(aes(p, RMSE))

pcr_bal_cv_best %>% 
  ggplot() +
  geom_line(aes(ncomp, RMSE))


# write_csv(pcr_bal_cv_res, 
#           "current_version/models/tuning_results/pcr_bal_cv/pcr_bal_cv_res.csv")
# 
# write_csv(pcr_bal_cv_best, 
#           "current_version/models/tuning_results/pcr_bal_cv/pcr_bal_cv_best.csv")
# 


pcr_bal_cv_res <- read_csv("current_version/models/tuning_results/pcr_bal_cv/pcr_bal_cv_res.csv")
pcr_bal_cv_best <- read_csv("current_version/models/tuning_results/pcr_bal_cv/pcr_bal_cv_best.csv")





