library(data.table)
library(lubridate)
library(dplyr)
library(tidyr)
library(tibble)

#Read Data
train_names <- fread(input = "./data/train.csv", data.table = TRUE, nrows = 1)
training <- fread(input = "./data/train.csv", data.table = TRUE,
                  skip = 100000000)
items <- fread(input = "./data/items.csv")
# As Tibble
training <- as_data_frame(training)
items <- as_data_frame(items)
# Col Names
names(training) <- names(train_names)
# Dates and subset
training$date <- as.Date(training$date, format = "%Y-%m-%d")

# Relevant minimal training set
train_set_2017 <- training[training$date <= "2017-07-30",]
train_set_2017 <- train_set_2017[train_set_2017$date > "2017-02-01",]
train_set_2017 <- train_set_2017 %>%
    inner_join(items) %>%
    select(c('id', 'date', 'store_nbr', 'item_nbr', 'unit_sales', 'perishable'))
write.csv(x = train_set_2017, file = 'train_set_2017.csv', row.names = FALSE)

# Dev Set
dev_set <- training[training$date > "2017-07-30",]

dev_set <- dev_set %>%
    inner_join(items) %>%
    select(c('id', 'date', 'store_nbr', 'item_nbr', 'unit_sales', 'perishable')) %>%
    mutate(perishable = if_else(condition = perishable == 1, true = 1.25, false = 1))

write.csv(x = dev_set, file = 'dev_set.csv', row.names = FALSE)
