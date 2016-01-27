rm(list=ls(all=TRUE))
options(scipen = 10)

library(dplyr)
library(ggplot2)
library(data.table)
library(parallel)
library(gbm)
library(e1071)

detectCores()

setwd('/users/adeonari/downloads/AXA')
#setwd("//W7GS-5121022/Science")

load('drivers7.RData')

drivers <- sort(unique(driver.summary$driver))
#drivers <- list.files("./drivers/")
#drivers <- drivers[1:12]
#drivers <- c(1,2,1701,3000)
n.drivers <- length(drivers)

n <- detectCores()
#n <- 12

if (file.exists('cluster1.txt'))
  file.remove('cluster1.txt')
cl <- makePSOCKcluster(n, outfile='cluster1.txt')

#divvy up drivers by number of cores
d.per <- round(n.drivers/n)
d.list <- split(drivers, ceiling(seq_along(drivers)/d.per))

clusterEvalQ(cl,library(dplyr))
clusterEvalQ(cl,library(data.table))
clusterEvalQ(cl,library(e1071))
clusterExport(cl, c('d.list','drivers','driver.summary'))
clusterSetRNGStream(cl, 2222)

x <- function(x){
  drivers <- d.list[[x]]
  predict.cl <- NULL
  
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
    
    
    #start <- Sys.time()
#      tune.s <- tune.svm(as.factor(target) ~ ., data=model.data, gamma=10^(-4:-1), cost=10^(1:3),
#                         tunecontrol = tune.control(sampling = "fix")
#                         #tunecontrol = tune.control(sampling = "bootstrap", nboot=5, boot.size=.9)
#                         #tunecontrol = tune.control(sampling = "cross", cross=5)
#                         )
#     #print(Sys.time()-start)
#     print(tune.s$best.parameters)

    s <- svm(as.factor(target) ~ ., data=model.data, gamma=.001, cost=10, probability=TRUE, epsilon=.01,
             class.weights=c('1'=1, '0'=.25)
             #class.weights=c('1'=1, '0'=.1)
             )
    
#     s <- svm(as.factor(target) ~ ., data=model.data, gamma=tune.s$best.parameters$gamma, cost=tune.s$best.parameters$cost,
#              probability=TRUE, epsilon=.01, class.weights=c('1'=1, '0'=.25))
    
    #summary(tune.s)
    #summary(s)
    
    p <- predict(s, curDriver, probability=TRUE)
    p<-attr(p, 'probabilities')[,1]
    
    p <- cbind(curDriver %>% select(driver, trip), p) %>%
      mutate(driver_trip = paste0(driver,'_', trip)) %>%
      select(driver_trip, p) %>%
      rename(prob = p)
    
    predict.cl <- rbind(predict.cl, p)
  }
  return(predict.cl)
}

start <- Sys.time()

predict.cl <- clusterApply(cl, 1:n, x)

stopCluster(cl)

message('time to run:')
print(Sys.time()-start)

predict.summary <- rbindlist(predict.cl, use.names=FALSE)

summary(predict.summary$prob)
ggplot(predict.summary) +
  geom_histogram(aes(prob), binwidth=.01)

#file <- paste0("axa-svm7-gamma=10(-4,-1)-cost=5(1,4)-epsilon=01-weights(1,25)-seed=1111", ".csv.gz")
file <- paste0("axa-svm7-gamma=001-cost=10-epsilon=01-weights(1,25)-seed=2222", ".csv.gz")

write.csv(predict.summary,gzfile(file),row.names=FALSE)
