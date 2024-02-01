library(stringr)
library(VGAM)
library(purrr)
library(mltools)
library(data.table)
library(readr)
library(tidyverse)

PATH <- "https://raw.githubusercontent.com/aldosolari/DM/master/docs/DATA/"
train <- read_delim(paste0(PATH, "train.csv"), delim = " ")
test <- read_delim(paste0(PATH, "test.csv"), delim = " ")

train_tmp <- train
test_tmp <- test

names(train_tmp) <- str_replace_all(names(train), "\\.", "_")
names(test_tmp) <- str_replace_all(names(test), "\\.", "_")

#### Data Cleaning ####

train2 <- train_tmp %>% 
  mutate(
    tariff_plan = as.factor(tariff_plan),
    payment_method = as.factor(payment_method),
    gender = as.factor(gender),
    activ_area = as.factor(activ_area),
    activ_chan = as.factor(activ_chan)
  )

test2 <- test_tmp %>% 
  mutate(
    tariff_plan = as.factor(tariff_plan),
    payment_method = as.factor(payment_method),
    gender = as.factor(gender),
    activ_area = as.factor(activ_area),
    activ_chan = as.factor(activ_chan)
  )

train2$vas1 <- ifelse(train2$vas1 == "Y", 1, 0)
train2$vas2 <- ifelse(train2$vas2 == "Y", 1, 0)

test2$vas1 <- ifelse(test2$vas1 == "Y", 1, 0)
test2$vas2 <- ifelse(test2$vas2 == "Y", 1, 0)

train2 <- as.data.frame(train2)
test2 <- as.data.frame(train2)

train2_dummy <- as.data.frame(one_hot(as.data.table(train2), dropCols = TRUE))
train2_dummy[, c(1:12, 14:28)] <- map_df(train2_dummy[, c(1:12, 14:28)], as.factor)
names(train2_dummy) <- str_replace_all(names(train2_dummy), "\\.| ", "_")


test2_dummy <- as.data.frame(one_hot(as.data.table(test2), dropCols = TRUE)) 
test2_dummy[, c(1:12, 14:28)] <- map_df(test2_dummy[, c(1:12, 14:28)], as.factor)
names(test2_dummy) <- str_replace_all(names(test2_dummy), "\\.| ", "_")


rm(train_tmp, test_tmp, PATH)
