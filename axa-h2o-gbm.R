options(scipen = 10)

library(dplyr)
library(data.table)
library(ggplot2)
library(glmnet)
library(h2o)

setwd('/users/adeonari/downloads/AXA')

load('drivers2.RData')

drivers <- list.files("./drivers/")
#drivers <- drivers[1:2]
#drivers <- c(1,2,1701,3000)
n.drivers <- length(drivers)

set.seed(2015)

predict.summary <- NULL
model.key <- NULL

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
  
  dat_h2o <- as.h2o(localH2O, model.data)
  
  #match('target', names(model.data))
  
  #create a grid of GBM models
  model <- 
    h2o.gbm(x = 1:143,
            y = 'target',
            distribution = 'bernoulli',
            data = dat_h2o,
            #n.trees = c(10000,20000),
            n.trees = c(5000,10000), 
            #interaction.depth = c(1,2,3,4,5),
            interaction.depth = c(2,3,4),
            n.minobsinnode = 5,
            shrinkage = c(0.01, 0.001),
            importance = FALSE, 
            balance.classes = FALSE)
  
  n.models <- length(model@model)
  model.summary <- NULL
  for(m in 1:n.models) {

    h2o_p <- h2o.predict(model@model[[m]], as.h2o(localH2O, curDriver))
    h2o_p <- as.data.frame(h2o_p) %>%
      select(X1) %>%
      #rename(paste0('prob_', m) = 'X1')
      setNames(paste0('prob_', m))
    
    if(!is.null(model.summary))
      model.summary <- cbind(model.summary, h2o_p)
    else
      model.summary <- h2o_p
  }
  
  p <- cbind(curDriver %>% select(driver, trip), model.summary) %>%
    mutate(driver_trip = paste0(driver,'_', trip)) %>%
    select(driver_trip, starts_with('prob'))
  
  #keep model params as a key file
  mk <- as.data.frame(do.call(rbind, model@sumtable)) %>%
    mutate(model.num = 1:n()) %>%
    mutate(driver = d)
  
  model.key <- rbind(model.key, mk)
  predict.summary <- rbind(predict.summary, p)
}

h2o.shutdown(localH2O, prompt = FALSE)

save(predict.summary, model.key, file='h2o-gbms.RData')

#file <- paste0("axa-h20-gbm-", ".csv.gz")
#write.csv(predict.summary, gzfile(file), row.names=FALSE)
