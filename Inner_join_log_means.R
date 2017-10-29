# Wrangling
library(dplyr)
library(tidyr)
library(data.table)
library(tibble)
library(lubridate)


# Read data and tidy

train_names <- fread(input = "./data/train.csv", nrows = 1)
training <- fread(input = "./data/train.csv", data.table = TRUE,
                  skip = 100000000)

training <- as_data_frame(training)

test <- fread(input = "./data/test.csv")
test <- as_data_frame(test)
names(training) <- names(train_names)

training$date <- as.Date(training$date, format = "%Y-%m-%d")
training$weekday <- weekdays(training$date)
test$date <- as.Date(test$date,format = '%Y-%m-%d')
test$weekday <- weekdays(test$date)
training <- training[training$date > '2017-03-01',]
training <- training[,c('weekday', 'store_nbr', 'item_nbr','unit_sales')]

# Transform to log1p

training$unit_sales <- as.numeric(training$unit_sales)
training[training$unit_sales < 0, "unit_sales"] <- 0
training$unit_sales <- log1p(training$unit_sales)

# SQL Join operations

# Notum sem mest flokkað miðgildi til að byrj ameð
submission <- training %>%
    group_by(weekday, item_nbr, store_nbr) %>%
    summarise('unit_sales' = mean(unit_sales)) %>%
    right_join(test) %>%
    ungroup() %>%
    mutate(unit_sales = expm1(unit_sales)) %>%
    select(c('id', 'unit_sales'))

# Miðgildi út frá aðeins færri flokkum
submission2 <- training %>%
    group_by(item_nbr, weekday) %>%
    summarise('unit_sales' = mean(unit_sales)) %>%
    right_join(test) %>%
    ungroup() %>%
    mutate(unit_sales = expm1(unit_sales)) %>%
    select(c('id', 'unit_sales'))

submission3 <- training %>%
    group_by(item_nbr) %>%
    summarise('unit_sales' = mean(unit_sales)) %>%
    right_join(test) %>%
    ungroup %>%
    mutate(unit_sales = expm1(unit_sales)) %>%
    select(c('id', 'unit_sales'))

# Impute'a minna flokkað miðgildi inn í NA gildi
submission[is.na(submission$unit_sales),] <- submission[is.na(submission$unit_sales),] %>%
    inner_join(test) %>%
    select(id) %>%
    inner_join(submission2)

items <- fread('./data/items.csv')

submission[is.na(submission$unit_sales),] <- submission[is.na(submission$unit_sales),] %>%
    inner_join(test) %>%
    select(id) %>%
    inner_join(submission3)

submission[is.na(submission$unit_sales),]
