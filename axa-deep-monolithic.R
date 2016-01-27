rm(list=ls(all=TRUE))
options(scipen = 10)

library(dplyr)
library(data.table)
library(ggplot2)
library(parallel)
library(h2o)

setwd('/users/adeonari/downloads/AXA')

load('drivers8.RData')

drivers <- list.files("./drivers/")
drivers <- drivers[1:24]
#drivers <- c(1,2,1701,3000)
n.drivers <- length(drivers)

set.seed(12345)

n <- detectCores()

cl <- makePSOCKcluster(n)

#divvy up drivers by number of cores
d.per <- round(n.drivers/n)
d.list <- split(drivers, ceiling(seq_along(drivers)/d.per))

clusterExport(cl, c('d.list','drivers','driver.summary'))
clusterEvalQ(cl,library(dplyr))
clusterEvalQ(cl,library(data.table))

x <- function(x){
  drivers <- d.list[[x]]
  model.data.cl <- NULL
  
  for(d in drivers) {

  print(d)
  print(match(d, drivers))
  
  curDriver <- driver.summary[driver==d] %>%
    mutate(target = 1)
  
  nonDrivers <- driver.summary[driver!=d] %>%
    sample_n(1000) %>%
    mutate(target = 0)
  
  model.data <- rbindlist(list(curDriver, nonDrivers), use.names=TRUE) %>%
    select(-driver, -trip) 

  model.data.cl <- rbind(model.data, model.data.cl)

  }
  return(model.data.cl)
}

model.data.cl <- clusterApply(cl, 1:n, x)
stopCluster(cl)

model.data.summary <- rbindlist(model.data.cl, use.names=FALSE)

#http://0xdata.com/docs/master/model/deep-learning/
#http://h2o.gitbooks.io/h2o-deep-learning/content/deep_learning_overview.html
#http://learn.h2o.ai/content/hands-on_training/deep_learning.html
localH2O <- h2o.init(ip = 'localhost', nthreads=12, max_mem_size = '96g')
h2o.clusterInfo(localH2O)

dat_h2o <- as.h2o(localH2O, model.data.summary, key = 'dat')

y <- match('target', names(model.data.summary))
  
model <- 
  h2o.deeplearning(x = 1:(y-1),  # column numbers for predictors
                   y = y,   # column number for label
                   data = dat_h2o, # data in H2O format
				           classification = TRUE,
                   activation = "TanhWithDropout", # Tanh, Rectifier, TanhWithDropout, RectifierWithDropout
                   input_dropout_ratio = 0.25, # % of inputs dropout
                   hidden_dropout_ratios = c(0.25,0.25,0.25,0.25,0.25), # % for nodes dropout
				           train_samples_per_iteration = -2, #-2: autotuning
                   balance_classes = TRUE,
                   class_sampling_factors = c(.1,.9), #0, 1
				           #max_after_balance_size = .9,
				           fast_mode = TRUE,
				           #l1=c(0,1e-5),
				           shuffle_training_data = TRUE,
        					 #rate = .005,
        					 adaptive_rate = T,
        						rho = 0.99,
        						epsilon = 1e-8,
                   hidden = c(50,50,50,50,50), # 5 layers of 50 nodes
                   epochs = 100) # max. no. of epochs

h2o_p <- h2o.predict(model, as.h2o(localH2O, driver.summary))
h2o_p <- as.data.frame(h2o_p)

h2o.shutdown(localH2O, prompt = FALSE)

predict.summary <- cbind(driver.summary %>% select(driver, trip), h2o_p) %>%
  mutate(driver_trip = paste0(driver,'_', trip)) %>%
  select(driver_trip, X1) %>%
  rename(prob = X1)

ggplot(predict.summary) +
  geom_histogram(aes(X1), binwidth=.01)

file <- paste0("axa-deep-", "act=TanhWithDropout(5,.25)", "adaptive_rate(.99,1e-8)", ".csv.gz")

write.csv(predict.summary, gzfile(file), row.names=FALSE)
