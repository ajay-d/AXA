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

load('drivers8.RData')

drivers <- sort(unique(driver.summary$driver))
drivers <- list.files("./drivers/")
#drivers <- drivers[1:24]
#drivers <- c(1,2,1701,3000)
n.drivers <- length(drivers)

#n <- detectCores()
n <- 6

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
clusterSetRNGStream(cl, 88888)

x <- function(x){
  drivers <- d.list[[x]]
  predict.cl <- NULL
  
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
    
    gbms <- list()
    
    gbms[[1]] <- gbm(target ~ ., data=model.data, dist='bernoulli', n.trees=10000,
                     interaction.depth=1, n.minobsinnode=5, shrinkage=.001,
                     keep.data=FALSE, verbose=FALSE)

    nonDrivers <- driver.summary[driver!=d] %>%
      sample_n(1000) %>%
      mutate(target = 0)
    
    model.data <- rbindlist(list(curDriver, nonDrivers), use.names=TRUE) %>%
      select(-driver, -trip)
    
    gbms[[2]] <- gbm(target ~ ., data=model.data, dist='bernoulli', n.trees=10000,
                     interaction.depth=1, n.minobsinnode=5, shrinkage=.001,
                     keep.data=FALSE, verbose=FALSE)
    
    best.iter <- 10000
    
    #First stage prediction
    ##Blend two seperate GBMs
    
    p <- list()
    
    p[[1]] <- predict(gbms[[1]], curDriver, 
                      n.trees=best.iter, type="response")
    p[[2]] <- predict(gbms[[2]], curDriver, 
                      n.trees=best.iter, type="response")
    
    #p1.cut <- median(p1)
    #p1.cut <- quantile(p1)[[2]]
    p1.cut <- quantile(p[[1]], seq(0.05, 1, by = 0.05))[['10%']]
    
    #ggplot(as.data.frame(p[[2]])) +
    #  geom_histogram(aes(p[[2]]), binwidth=.01)
    
    p.comb <- do.call(cbind, lapply(p, as.data.frame)) %>%
      setNames(c('p1', 'p2')) %>%
      rowwise() %>%
      mutate(p=(p1+p2)/2)
    
    p.cut <- quantile(p.comb$p, seq(0.05, 1, by = 0.05))[['20%']]
    
    curDriver.trimmed <- cbind(curDriver, p.comb) %>%
      filter(p > p.cut) %>%
      select(-p1, -p2, -p)
    
    p.non <- list()
    
    p.non[[1]] <- predict(gbms[[1]], driver.summary[driver!=d], 
                           n.trees=best.iter, type="response")
    p.non[[2]] <- predict(gbms[[2]], driver.summary[driver!=d], 
                           n.trees=best.iter, type="response")
    
    p.comb.non <- do.call(cbind, lapply(p.non, as.data.frame)) %>%
      setNames(c('p1', 'p2')) %>%
      rowwise() %>%
      mutate(p=(p1+p2)/2)
    
    #p1.non.cut <- median(p1.non)
    #p1.non.cut <- quantile(p1.non)[[3]]
    
    p.comb.non.cut <- median(p.comb.non$p)
    
    non.sample <- cbind(driver.summary[driver!=d], p.comb.non) %>%
      ##Try with and without commenting out the filter
      filter(p > p.comb.non.cut) %>%
      sample_n(nrow(curDriver.trimmed)) %>%
      #sample_n(1000) %>%
      mutate(target = 0) %>%
      select(-p1, -p2, -p)
    
    model.data <- rbindlist(list(curDriver.trimmed, non.sample), use.names=TRUE) %>%
      select(-driver, -trip)
    
    gbms[[3]] <- gbm(target ~ ., data=model.data, dist='bernoulli', n.trees=10000,
                     #interaction.depth=1, n.minobsinnode=5, shrinkage=.001, train.fraction=.75,
                     interaction.depth=3, n.minobsinnode=5, shrinkage=.0005,
                     #keep.data=TRUE, verbose=TRUE)
                     keep.data=FALSE, verbose=FALSE)

    #best.iter <- gbm.perf(g2, method='test')
    #print(best.iter)

    #second stage prediction
    p2 <- predict(gbms[[3]], curDriver, 
                 n.trees=best.iter, type="response")
  
    p2 <- cbind(curDriver %>% select(driver, trip), p2) %>%
      mutate(driver_trip = paste0(driver,'_', trip)) %>%
      select(driver_trip, p2) %>%
      rename(prob = p2)
    
#     ggplot(p2) +
#       geom_histogram(aes(prob), binwidth=.01)
    
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

file <- paste0("axa-gbm8-2stage-cut2=20-50-10k-10k-depth=1-3-train=100-100-seed=88888", ".csv.gz")

write.csv(predict.summary,gzfile(file),row.names=FALSE)
