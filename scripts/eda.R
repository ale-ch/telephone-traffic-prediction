library(readr)
library(corrplot)
library(tidyverse)
library(ranger)
library(heatmaply)
source("https://raw.githubusercontent.com/ale-ch/custom_functions_R/main/custom_functions_R/corr_rank.R")
source("current_version/utilities/vimp_plot.R")
source("current_version/utilities/data_cleaning.R")
rm(test, test2, test2_dummy)
theme_set(theme_minimal())

train2_num <- apply(train2_dummy, 2, as.numeric)



#### Data Properties ####

##### Dataset structure #####
# dataset structure
str(train2)

##### Missing Values #####
# no missing values
anyNA(train2)

##### All-zero rows #####
# Osservazioni le cui variabili continue assumono tutte valore 0
is_zero_row <- apply(train2[, 10:100], 1, function(rows) all(rows == 0))
which(is_zero_row)
sum(is_zero_row) # 1417 osservazioni all-zero

##### Zero cols #####
# Proporzione di 0 nelle colonne
zero_prop <- apply(train2[, 10:100], 2, function(col) sum(col == 0) / nrow(train2))

zero_prop <- as.data.frame(zero_prop)
zero_prop <- zero_prop %>% 
  mutate(var = rownames(.))
rownames(zero_prop) <- NULL

zero_vars <- zero_prop %>% 
  filter(zero_prop >= 0.8) %>% 
  select(var) %>% unlist()

#### Response ####
summary(train2$y)
sd(train2$y)

train2 %>% 
  ggplot() + 
  geom_histogram(aes(y)) # extremely skewed distribution 

train2 %>% 
  ggplot() + 
  geom_boxplot(aes(y)) +
  coord_flip() # there are few outliers

train2 %>% 
  ggplot() + 
  geom_histogram(aes(log1p(y))) # there are few outliers



# Osservazioni pari a 0
train2 %>% 
  filter(y == 0) %>% View()

train2 %>% 
  filter(y > 0) %>% View()


#### Predictors ####

##### status #####
summary(train2$status)
sd(train2$status)

p0 <- train2 %>% 
  ggplot() + 
  geom_histogram(aes(status))

##### tariff plan #####
summary(train2$status)
sd(train2$status)

p1 <- train2 %>% 
  ggplot() + 
  geom_bar(aes(tariff_plan))

##### payment method #####
p2 <- train2 %>% 
  ggplot() + 
  geom_bar(aes(payment_method))

##### gender #####
p3 <- train2 %>% 
  ggplot() + 
  geom_bar(aes(gender))

##### activ area #####
p4 <- train2 %>% 
  ggplot() + 
  geom_bar(aes(activ_area))

##### activ channel #####
p5 <- train2 %>% 
  ggplot() + 
  geom_bar(aes(activ_chan))

##### vas1 #####
p6 <- train2 %>% 
  ggplot() + 
  geom_histogram(aes(vas1))

##### vas2 #####
p7 <- train2 %>% 
  ggplot() + 
  geom_histogram(aes(vas2))

gridExtra::grid.arrange(nrow = 3, ncol = 3, p0, p1, p2, p3, p4, p5, p6, p7)

##### Continuous predictors ####

for(i in 10:19) {
  name <- names(train[, i])
  hist(train2[, i], 
       main = paste0("Histogram of ", name),
       xlab = name)
}


plots <- list()
for(i in 10:19) {
  col <- names(train2 %>% select(i))
  plots[[i]] <- train2 %>% 
    select(all_of(col)) %>% 
    ggplot() +
    geom_histogram(aes_string(col))
}

plots[1:9] <-  NULL

gridExtra::grid.arrange(plots, nrow = 2, ncol = 5, grobs = plots)

train2 %>% 
  select(all_of(col)) %>% 
  ggplot() +
  geom_histogram(aes_string(col))

train2 %>% 
  select(q01_out_dur_peak) %>% 
  ggplot() +
  geom_histogram(aes(q01_out_dur_peak))

# Deviazione standard  
sd_x <- apply(train2[, 10:100], 2, 
              function(x) { 
                sd(x)
              })

hist(sd_x) 

# range
range_x <- apply(train2[, 10:100], 2, function(col) range(col))
range_x <- as.data.frame(t(range_x))

train2[which(train2$q03_in_dur_tot < 0), ]
train2[which(train2$q09_out_dur_peak < 0), ]

hist(range_x$V1)
hist(range_x$V2)

boxplot(train2[10:19])
boxplot(train2[20:29])

boxplot(scale(train2[10:19]))

# skewed distributions of the continuous predictors --> yeo-johnson 
# The continuous predictors are all on different scales --> standardize
# See transform_scale.R in the "resources" folder for details

#### Correlations/Relations ####

# Correlation matrix (All variables)
heatmaply_cor(
  cor(train2_num),
  symm = TRUE,
  cexRow = .0001, 
  cexCol = .0001, 
  branches_lwd = .1,
  showticklabels = c(FALSE, FALSE)
)

# Correlation matrix (numeric predictors)
cor_map <- heatmaply_cor(
  cor(train2 %>% select(10:99)),
  symm = TRUE,
  cexRow = .0001, 
  cexCol = .0001, 
  branches_lwd = .1,
  showticklabels = c(FALSE, FALSE)
)


# Scatterplots y ~ x
par(mfrow = c(3, 3))
for(i in 1:9) {
  plot(train2[, i], train2[, "y"])
}

par(mfrow = c(4, 3))
for(i in 10:19) {
  plot(train2[, i], train2[, "y"])
}

##### between all variables ######
corr_df <- corr_rank(train2_num)
corr_df %>% View()

##### between predictors and response #####
corr_df %>% 
  filter(var1 == "y") %>% 
  View()

##### between predictors #####
corr_df %>% 
  filter(var1 != "y", var2 != "y") %>% 
  mutate(value = abs(value)) %>% 
  View()

# filter out correlations lower than 10%
cor_xy <- corr_df %>% 
  filter(var1 == "y") %>% 
  mutate(value = abs(value)) %>% 
  filter(value > 0.10) %>% 
  arrange() 

my_table <- gridExtra::tableGrob(cor_xy[1:10, ])
gridExtra::grid.arrange(my_table)


#### Clustering ####

df <- as.data.frame(train2_num[-c(which(train2$q03_in_dur_tot < 0, ), 
                                  which(train2$q09_out_dur_peak < 0, )), ]) 
pr <- prcomp(df %>% select(-y), scale = TRUE)
pr <- as.data.frame(pr$x)
 
pr %>% 
  ggplot() + 
  geom_point(aes(PC2, PC1), color = ifelse(df$y > 0, 1, 2))

pr %>% 
  ggplot() + 
  geom_point(aes(PC2, PC1), 
             color = ifelse(df$q09_out_dur_offpeak == 0, 1, 2))


df2 <- cbind(pr, y = train2$y[-c(which(train2$q03_in_dur_tot < 0, ), 
                                which(train2$q09_out_dur_peak < 0, ))],
             q09_out_dur_offpeak = df$q09_out_dur_offpeak) 

df2$q09_01 <- as.factor(ifelse(df2$q09_out_dur_offpeak == 0, 0, 1))

df2 %>% 
  ggplot() + 
  geom_point(aes(PC1, y, color = q09_01)) +
  theme(legend.position = "none")

df2 %>% 
  ggplot() + 
  geom_point(aes(PC2, PC1, color = q09_01)) + 
  theme(legend.title = element_blank()) + 
  scale_color_discrete(labels = c("q09_out_dur_offpeak = 0", 
                                  "q09_out_dur_offpeak > 0"))


train2 %>% 
  mutate(q09_01 = as.factor(ifelse(q09_out_dur_offpeak == 0, 0, 1))) %>% 
  ggplot() + 
  geom_point(aes(q09_out_dur_peak, y, color = q09_01)) + 
    theme(legend.title = element_blank()) + 
    scale_color_discrete(labels = c("q09_out_dur_offpeak = 0", 
                                    "q09_out_dur_offpeak > 0"))

ggplot() +
  geom_point(aes(pr$PC1, log1p(df$y), 
                 color = ifelse(df2$q09_out_dur_offpeak == 0, 1, 2)))




#### Time Series ####

y_ts <- train2[, which(str_detect(names(train2), "out_dur_offpeak"))] +
train2[, which(str_detect(names(train2), "out_dur_peak"))]

y_ts <- cbind(y_ts, train2$y)

t <- 1:length(y_ts)
names(y_ts) <- paste0("y", t)

names(y_ts) <- t
y_ts$id <- 1:nrow(y_ts)
y_ts <- y_ts %>% 
  pivot_longer(
    cols = 1:10, 
    names_to = "t",
    values_to = "y"
  )


y_ts %>% 
  ggplot(aes(t, y, group = id, color = id)) +
  geom_line()


y_ts %>% 
  ggplot(aes(t, log1p(y), group = id, color = id)) +
  geom_line()

