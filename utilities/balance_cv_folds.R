library(ROSE)
library(dplyr)

balance_cv_folds <- function(formula = formula(), data, folds, p, seed = 123) {
  train_folds <- list()
  valid_folds <- list()
  train_folds_bal <- list()
  for(i in 1:length(folds)) {
    fold <- folds[[i]]
    
    valid_folds[[i]] <- data %>% 
      filter(!(id %in% fold)) %>% 
      select(id) %>% 
      unlist() %>% 
      unname() %>% 
      as.integer()
    
    set.seed(seed)
    dat <<- data %>% filter(id %in% fold)
    train_folds_bal[[i]] <- as.integer(ovun.sample(formula, 
                                                   dat, 
                                                   p = p)$data$id)
  }
  
  list(train_folds_bal = train_folds_bal,
       valid_folds = valid_folds)
}

# balance_cv_folds(y01 ~ id, df, folds, 0.2)
