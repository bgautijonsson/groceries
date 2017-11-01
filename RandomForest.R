# Wrangling
library(dplyr)
library(tidyr)
library(data.table)
library(tibble)
library(lubridate)
library(reshape2)


# Machine Learning
library(caret)
library(forecast)
library(foreach)
library(forecastHybrid)
source('cost.R')

# Parallel Processing
library(parallel)
library(doParallel)

# Seed
set.seed(1337)

# Reading the data
training <- fread('train_set_2017.csv')
dev <- fread('dev_set.csv')

training$date <- as.Date(training$date, format = "%Y-%m-%d")
dev$date <- as.Date(dev$date, format = "%Y-%m-%d")

# Transform to log1p

training$unit_sales <- as.numeric(training$unit_sales)
training[training$unit_sales < 0, "unit_sales"] <- 0
training$unit_sales <- log1p(training$unit_sales)

# Variables as factors

training$store_nbr <- as.character(training$store_nbr)
training$item_nbr <- as.character(training$item_nbr)

# Preprocess

training_small <- training %>%
    mutate(week = ceiling(day(date)/4), weekday = weekdays(date)) %>%
    mutate(perishable = as.factor(perishable)) %>%
    group_by(week, weekday, store_nbr, perishable) %>%
    summarise(unit_sales = mean(unit_sales))


training_small[training_small$store_nbr == 32,]$store_nbr = '26'
training_small$weekday <- as.factor(training_small$weekday)

dev_pred <- dev %>%
    mutate(week = ceiling(day(date)/4), weekday = weekdays(date)) %>%
    mutate(perishable = if_else(perishable == 1, 0, 1)) %>%
    select(week, weekday, store_nbr, perishable)

dev_pred$weekday <- as.factor(dev_pred$weekday)

# Training
X_train <- training_small[,-5]
y_train <- training_small$unit_sales

trControl <- trainControl(method = 'cv', number = 5, 
                          verboseIter = TRUE, allowParallel = TRUE)

cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)
fit <- train(x = X_train, y = y_train, method = 'rf', 
             trControl = trControl)
stopCluster(cluster)
registerDoSEQ()


pred <- predict(fit, dev_pred)

pred <- expm1(pred)

source('cost.R')

cost(pred = pred, test = dev)

plotnum <- sample(x = 1:1000000, size = 500)
plot(dev$unit_sales[plotnum], pred[plotnum])


