# Wrangling
library(dplyr)
library(tidyr)
library(data.table)
library(tibble)
library(lubridate)

# Visuals
library(ggplot2)
library(GGally)
library(ggthemes)


# Reading the data
## Unzippaði með mac forriti sem heitir Keka
## Ekki keyra þennan kóða nema þú viljir allt gagnasafnið
#train <- fread(input = "./data/train.csv")
#train <- as_data_frame(train)

# Sample af stóra gagnasafninu. Commenta kóðan útt til að eyða því ekki óvart.
# Ætti að endurtaka samplið með set.seed() á eitthvað og gera write.csv
# smalltrain <- sample_frac(tbl = train, size = 0.01, replace = F)

items <- as_data_frame(fread("./data/items.csv"))
oil <- as_data_frame(fread("./data/oil.csv"))
stores <- as_data_frame(fread("./data/stores.csv"))
transactions <- as_data_frame(fread("./data/transactions.csv"))
holidays <- as_data_frame(fread("./data/holidays_events.csv"))

# Inner join til að gera eina töflu
combined <- smalltrain %>%
    inner_join(items) %>%
    inner_join(stores) %>%
    inner_join(transactions) %>%
    inner_join(oil)
combined <- combined %>%  
    mutate(date = as.Date(combined$date, format = "%Y-%m-%d"))

combined
str(combined)
summary(combined)

# City boxplot
ggplot(combined, aes(city, transactions)) + geom_boxplot() + theme_tufte() + coord_flip()
# Transactions boxplot
ggplot(combined, aes(family, transactions)) + geom_boxplot() + theme_tufte() + coord_flip()
