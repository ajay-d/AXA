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
#drivers <- list.files("./drivers/")
#drivers <- drivers[1:12]
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
clusterSetRNGStream(cl, 6666)

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
    
    g <- gbm(target ~ ., data=model.data, dist='bernoulli', n.trees=10000,
             interaction.depth=2, n.minobsinnode=5, shrinkage=.001, train.fraction=.90,
             #keep.data=TRUE, verbose=TRUE)
             keep.data=FALSE, verbose=FALSE)
    #summary(g)
    best.iter <- gbm.perf(g, method='test')
    print(best.iter)
    #summary(g, n.trees=best.iter)
    
    p <- predict(g, curDriver, 
                 n.trees=best.iter, type="response")
  
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

ggplot(predict.summary) +
  geom_histogram(aes(prob), binwidth=.01)

file <- paste0("axa-gbm6-10k-depth=2-train=.90-shrink=.001-seed=6666", ".csv.gz")

write.csv(predict.summary,gzfile(file),row.names=FALSE)
