# Wrangling
library(dplyr)
library(tidyr)
library(data.table)
library(tibble)
library(lubridate)
library(reshape2)


# Machine Learning
library(caret)
source('cost.R')
library(keras)






# Reading the data
items <- fread('./data/items.csv')
stores <- fread('./data/stores.csv')
training <- fread('train_set_2017.csv')
dev <- fread('dev_set.csv')


training$date <- as.Date(training$date, format = "%Y-%m-%d")
dev$date <- as.Date(dev$date, format = "%Y-%m-%d")

# Transform to log1p

training$unit_sales <- as.numeric(training$unit_sales)
training[training$unit_sales < 0, "unit_sales"] <- 0
training$unit_sales <- log1p(training$unit_sales)

dev[dev$unit_sales<0,]$unit_sales <- 0
dev$unit_sales <- log1p(dev$unit_sales)



# Preprocess

training <- training %>%
    mutate(store_nbr = as.factor(store_nbr)) %>%
    mutate(week = ceiling(day(date)/4), weekday = weekdays(date)) %>%
    inner_join(items) %>%
    group_by(store_nbr, family, week, weekday) %>%
    summarise(unit_sales = mean(unit_sales))

dev <- dev %>%
    mutate(store_nbr = as.factor(store_nbr)) %>%
    mutate(month = month(date), week = ceiling(day(date)/4), weekday = weekdays(date)) %>%
    inner_join(items, by = "item_nbr") %>%
    select(week, weekday, store_nbr,  family, unit_sales)

# One Hot Encoding

train_label <- training$unit_sales
dev_label <- dev$unit_sales

onehot <- dummyVars(formula =  unit_sales ~ ., data = training, sep = ".")

training <- predict(object = onehot, newdata = training)

onehotdev <- dummyVars(formula =  unit_sales ~ ., data = dev, sep = ".")
dev <- predict(object = onehot, newdata = dev)

# Model


model <- keras_model_sequential() 

model %>%
    layer_dense(units = 150, activation = 'relu', input_shape = c(95)) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 90, activation = 'relu') %>%
    layer_dropout(rate = 0.4) %>%
    layer_dense(units = 30, activation = 'relu') %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 1)

model %>% compile(
    loss = 'mean_squared_error',
    optimizer = 'adam',
    metrics = 'mse'
)

history <- model %>% fit(
    training, train_label,
    epochs = 30, batch_size = 128,
    validation_split = 0.1
)    

plot(history)

model %>% evaluate(dev, dev_label)
