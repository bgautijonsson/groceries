# Thetta er port af forecasting.r thar sem
# skipt er ut doMC, sem er unix only parallel processing pakki, fyrir doParallel

# Wrangling
library(dplyr)
library(tidyr)
library(data.table)
library(tibble)
library(lubridate)
library(reshape2)

# Visuals
library(ggplot2)
library(GGally)
library(ggthemes)

# Machine Learning
library(caret)
library(forecast)
library(foreach)

# Parallel Processing
library(parallel)
library(doParallel)
library(doMC)

# Seed
set.seed(1337)

# Reading the data
## Unzippaði með mac forriti sem heitir Keka
## Ekki keyra þennan kóða nema þú viljir allt gagnasafnið

# set working directory i folder med data til ad naestu linur virki
# t.d. setwd('C:/Users/Larus/Documents/GitHub/groceries')

train_names <- fread(input = "./data/train.csv", nrows = 1)
training <- fread(input = "./data/train.csv", data.table = TRUE,
                  skip = 100000000)

# Taka allt dataset i training:
# training <- fread(input = "./data/train.csv", data.table = TRUE)

training <- as_data_frame(training)

test <- fread(input = "./data/test.csv")
test <- as_data_frame(test)
names(training) <- names(train_names)

training$date <- as.Date(training$date, format = "%Y-%m-%d")
test$date <- as.Date(test$date,format = '%Y-%m-%d')

training <- training[training$date > '2017-04-01',]
training$store_item_nbr <- paste(training$store_nbr, training$item_nbr, sep = "_")
training <- training[,c('date', 'store_item_nbr', 'unit_sales')]
test$store_item_nbr <- paste(test$store_nbr, test$item_nbr, sep = "_")

# Transform to log1p

training$unit_sales <- as.numeric(training$unit_sales)
training[training$unit_sales < 0, "unit_sales"] <- 0
training$unit_sales <- log1p(training$unit_sales)

# dcast the data from long to wide format for time series forecasting

training <- dcast(training, store_item_nbr ~ date, value.var = "unit_sales", fill = 0)
train_ts <- ts(training, frequency = 7)

fcst_intv = 16  # 16 days of forecast interval (Aug 16 ~ 31) per the submission requirement
fcst_matrix <- matrix(NA,nrow=nrow(train_ts),ncol=fcst_intv)


# register cores for parallel processing in ETS forecasting
registerDoMC(detectCores()-1)
fcst_matrix <- foreach(i=1:nrow(train_ts),.combine=rbind, .packages=c("forecast")) %dopar% { 
  fcst_matrix <- forecast(ets(train_ts[i,]),h=fcst_intv)$mean
}

# post-processing the forecast table
fcst_matrix[fcst_matrix < 0] <- 0
colnames(fcst_matrix) <- as.character(seq(from = as.Date("2017-08-16"), 
                                          to = as.Date("2017-08-31"), 
                                          by = 'day'))
fcst_df <- as.data.frame(cbind(training[, 1], fcst_matrix)) 
colnames(fcst_df)[1] <- "store_item_nbr"

# melt the forecast data frame from wide to long format for final submission
fcst_df_long <- melt(fcst_df, id = 'store_item_nbr', 
                     variable.name = "fcst_date", 
                     value.name = 'unit_sales')
fcst_df_long$store_item_nbr <- as.character(fcst_df_long$store_item_nbr)
fcst_df_long$fcst_date <- as.Date(parse_date_time(fcst_df_long$fcst_date,'%y-%m-%d'))
fcst_df_long$unit_sales <- as.numeric(fcst_df_long$unit_sales)

# transform back to exp1p
fcst_df_long$unit_sales <- expm1(fcst_df_long$unit_sales)


# generate the final submission file
submission <- left_join(test, fcst_df_long, 
                        c("store_item_nbr" = "store_item_nbr", 'date' = 'fcst_date'))
submission$unit_sales[is.na(submission$unit_sales)] <- 0
submission <- select(submission, c('id', 'unit_sales'))
write.csv(submission, "submission_forecastingR.csv", row.names = FALSE)





