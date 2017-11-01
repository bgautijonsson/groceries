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
library(xgboost)

# Parallel Processing
library(parallel)
library(doParallel)

# Seed
set.seed(1337)

# Reading the data

training <- fread('train_set_2017.csv')
dev <- fread('dev_set.csv')
dev[dev$unit_sales<0,]$unit_sales <- 0

training$date <- as.Date(training$date, format = "%Y-%m-%d")
dev$date <- as.Date(dev$date, format = "%Y-%m-%d")

# Transform to log1p

training$unit_sales <- as.numeric(training$unit_sales)
training[training$unit_sales < 0, "unit_sales"] <- 0
training$unit_sales <- log1p(training$unit_sales)

# Preprocess

training <- training %>%
    mutate(month = month(date), week = ceiling(day(date)/4), weekday = weekdays(date)) %>%
    group_by(week, weekday, store_nbr, item_nbr) %>%
    summarise(unit_sales = mean(unit_sales))

dev <- dev %>%
    mutate(week = ceiling(day(date)/4), weekday = weekdays(date)) %>%
    select(week, weekday, store_nbr,item_nbr, unit_sales)

# One Hot Encoding

train_label <- training$unit_sales
dev_label <- dev$unit_sales
onehot <- dummyVars(formula =  unit_sales ~ ., data = training, sep = ".")

training <- predict(object = onehot, newdata = training)
dev <- predict(object = onehot, newdata = dev)

training <- xgb.DMatrix(data = training, label = train_label)
dev <- xgb.DMatrix(data = dev, label = log1p(dev_label))
watchlist <- list(train=training, test=dev)

# Training

xgfit <- xgb.train(data = training, verbose = 1, nrounds = 100,
                  watchlist = watchlist, nthread = 2, save_period = 50, 
                  save_name = 'xgboost.model', xgb_model = 'xgboost.model')




# Predict and Evaluate (Þetta gerist líka inní algoriþmanum)
pred <- predict(xgfit, dev)
pred <- expm1(pred)

cost(pred = pred, test = dev)



# Model
