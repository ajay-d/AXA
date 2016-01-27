rm(list=ls(all=TRUE))
options(scipen = 10)

library(dplyr)
library(data.table)
library(ggplot2)
library(glmnet)
library(h2o)

setwd('/users/adeonari/downloads/AXA')

load('drivers8.RData')

drivers <- list.files("./drivers/")
#drivers <- drivers[1:2]
#drivers <- c(1,2,1701,3000)
n.drivers <- length(drivers)

set.seed(33333)

predict.summary <- NULL

#http://0xdata.com/docs/master/model/deep-learning/
localH2O <- h2o.init(ip = 'localhost', nthreads=-1, max_mem_size = '96g')
h2o.clusterInfo(localH2O)

for(d in drivers) {
  print(d)
  print(match(d, drivers))
  
  curDriver <- driver.summary[driver==d] %>%
    mutate(target = 1)
  
  nonDrivers <- driver.summary[driver!=d] %>%
    #filter(driver != d) %>%
    sample_n(1000) %>%
    mutate(target = 0)
  
  model.data <- rbindlist(list(curDriver, nonDrivers), use.names=TRUE) %>%
    select(-driver, -trip) 

  model.data <- sapply(model.data, as.numeric) %>%
    as.data.frame

  dat_h2o <- as.h2o(localH2O, model.data, key = 'dat')
  
  y <- match('target', names(model.data))
  
  model <- 
    h2o.deeplearning(x = 1:(y-1),  # column numbers for predictors
                     y = y,   # column number for label
                     data = dat_h2o, # data in H2O format
                     activation = "TanhWithDropout", # Tanh, Rectifier, TanhWithDropout, RectifierWithDropout
                     input_dropout_ratio = 0.25, # % of inputs dropout
                     hidden_dropout_ratios = c(0.25,0.25,0.25), # % for nodes dropout
                     balance_classes = FALSE, 
                     hidden = c(50,50,50), # three layers of 50 nodes
                     epochs = 100) # max. no. of epochs
  
  h2o_p <- h2o.predict(model, as.h2o(localH2O, curDriver))
  h2o_p <- as.data.frame(h2o_p)
  
  p <- cbind(curDriver %>% select(driver, trip), h2o_p) %>%
    mutate(driver_trip = paste0(driver,'_', trip)) %>%
    select(driver_trip, X1) %>%
    rename(prob = X1)
  
  predict.summary <- rbind(predict.summary, p)
}

h2o.shutdown(localH2O, prompt = FALSE)

ggplot(h2o_p) +
  geom_histogram(aes(X1), binwidth=.01)

ggplot(predict.summary) +
  geom_histogram(aes(prob), binwidth=.01)

file <- paste0("axa-deep8-", "act=TanhWithDropout", ".csv.gz")

write.csv(predict.summary, gzfile(file), row.names=FALSE)
