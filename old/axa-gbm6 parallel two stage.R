rm(list=ls(all=TRUE))
options(scipen = 10)

library(dplyr)
library(ggplot2)
library(data.table)
library(parallel)
library(gbm)

detectCores()

setwd('/users/adeonari/downloads/AXA')
#setwd("//W7GS-5121022/Science")

load('drivers6.RData')

drivers <- sort(unique(driver.summary$driver))
drivers <- list.files("./drivers/")
#drivers <- drivers[1:24]
#drivers <- c(1,2,1701,3000)
n.drivers <- length(drivers)

#n <- detectCores()
n <- 12

if (file.exists('cluster.txt'))
  file.remove('cluster.txt')
cl <- makePSOCKcluster(n, outfile='cluster.txt')

#divvy up drivers by number of cores
d.per <- round(n.drivers/n)
d.list <- split(drivers, ceiling(seq_along(drivers)/d.per))

clusterEvalQ(cl,library(dplyr))
clusterEvalQ(cl,library(data.table))
clusterEvalQ(cl,library(gbm))
clusterExport(cl, c('d.list','drivers','driver.summary'))
clusterSetRNGStream(cl, 1111)

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
    
    g1 <- gbm(target ~ ., data=model.data, dist='bernoulli', n.trees=5000,
             interaction.depth=1, n.minobsinnode=5, shrinkage=.001, train.fraction=.90,
             #keep.data=TRUE, verbose=TRUE)
             keep.data=FALSE, verbose=FALSE)
    #summary(g1)
    best.iter <- gbm.perf(g1, method='test')
    print(best.iter)
    #summary(g1, n.trees=best.iter)
    
    #First stage prediction
    p1 <- predict(g1, curDriver, 
                 n.trees=best.iter, type="response")
    #p1.cut <- median(p1)
    p1.cut <- quantile(p1)[[2]]
    
    #Should be about 100 trips
    p1 <- cbind(curDriver, p1) %>%
      filter(p1 > p1.cut) %>%
      select(-p1)
    
    p1.non <- predict(g1, driver.summary[driver!=d], 
                 n.trees=best.iter, type="response")
    
    p1.non.cut <- median(p1.non)
    #p1.non.cut <- quantile(p1.non)[[3]]
    
    p1.non <- cbind(driver.summary[driver!=d], p1.non) %>%
      filter(p1.non > p1.non.cut) %>%
      sample_n(500) %>%
      mutate(target = 0) %>%
      select(-p1.non)
    
    model.data <- rbindlist(list(p1, p1.non), use.names=TRUE) %>%
      select(-driver, -trip)
    
    g2 <- gbm(target ~ ., data=model.data, dist='bernoulli', n.trees=10000,
             interaction.depth=2, n.minobsinnode=5, shrinkage=.001, train.fraction=.75,
             #keep.data=TRUE, verbose=TRUE)
             keep.data=FALSE, verbose=FALSE)

    best.iter <- gbm.perf(g2, method='test')
    print(best.iter)

    #second stage prediction
    p2 <- predict(g2, curDriver, 
                 n.trees=best.iter, type="response")
  
    p2 <- cbind(curDriver %>% select(driver, trip), p2) %>%
      mutate(driver_trip = paste0(driver,'_', trip)) %>%
      select(driver_trip, p2) %>%
      rename(prob = p2)
    
    predict.cl <- rbind(predict.cl, p2)
  }
  return(predict.cl)
}

start <- Sys.time()

predict.cl <- clusterApply(cl, 1:n, x)

stopCluster(cl)

message('time to run:')
print(Sys.time()-start)

predict.summary <- rbindlist(predict.cl, use.names=FALSE)

ggplot(predict.summary) +
  geom_histogram(aes(prob), binwidth=.01)

file <- paste0("axa-gbm6-2stage-cut2=5-50-5k-10k-depth=1-2-train=90-75-seed=1111", ".csv.gz")

write.csv(predict.summary,gzfile(file),row.names=FALSE)
