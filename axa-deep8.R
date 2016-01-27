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
#drivers <- drivers[1:5]
#drivers <- c(1,2,1701,3000)
n.drivers <- length(drivers)

set.seed(22222)

predict.summary <- NULL

#http://0xdata.com/docs/master/model/deep-learning/
localH2O <- h2o.init(ip = 'localhost', nthreads=6, max_mem_size = '64g')
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
  
  grid_search <- 
  h2o.deeplearning(x = 1:(y-1),  # column numbers for predictors
                   y = y,   # column number for label
                   data = dat_h2o, # data in H2O format
  			           classification = TRUE,
                   activation = c("TanhWithDropout", 'RectifierWithDropout'), # Tanh, Rectifier, TanhWithDropout, RectifierWithDropout
                   input_dropout_ratio = 0.25, # % of inputs dropout
                   hidden_dropout_ratios = c(0.25,0.25,0.25,0.25,0.25), # % for nodes dropout
				           train_samples_per_iteration = -2, #-2: autotuning
                   #balance_classes = TRUE,
                   #class_sampling_factors = c(.1,.9), #0, 1
				           #max_after_balance_size = .9,
				           fast_mode = TRUE,
				           l1=c(0,1e-5),
				           shuffle_training_data = TRUE,
        					 #rate = .005,
        					 adaptive_rate = T,
        						rho = 0.99,
        						epsilon = 1e-8,
                   hidden = c(50,50,50,50,50), # 5 layers of 50 nodes
                   epochs = 100) # max. no. of epochs
  
  h2o_p <- list()

  for(model.i in 1:length(grid_search@model)) {

    h2o_p[[model.i]] <- h2o.predict(grid_search@model[[model.i]], as.h2o(localH2O, curDriver))
    h2o_p[[model.i]] <- as.data.frame(h2o_p[[model.i]])
  }
  
  p <- do.call(cbind, h2o_p)
  p <- p[grep('X1',names(p), ignore.case = TRUE)]
  
  #setNames(h2o_p[[model.i]], c('predict', 'X0', paste0('prob', model.i)))
  
  p <- cbind(curDriver %>% select(driver, trip), p) %>%
    mutate(driver_trip = paste0(driver,'_', trip)) %>%
    select(driver_trip, starts_with('X'))
  
  predict.summary <- rbind(predict.summary, p)
  
}

h2o.shutdown(localH2O, prompt = FALSE)

file <- paste0("axa-deep-", "act=TanhWithDropout(5,.25)", "adaptive_rate(.99,1e-8)", ".csv.gz")

write.csv(predict.summary, gzfile(file), row.names=FALSE)
