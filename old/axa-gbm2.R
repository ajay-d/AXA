options(scipen = 10)

library(dplyr)
library(data.table)
library(ggplot2)
library(glmnet)
library(gbm)

set.seed(122014)

setwd('/users/adeonari/downloads/AXA')

load('drivers2.RData')

drivers <- list.files("./drivers/")
#drivers <- drivers[1:10]
#drivers <- c(1,2,1701,3000)
n.drivers <- length(drivers)

predict.summary <- NULL

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
    select(-driver, -trip) %>%
    select(duration, max.dist, end.dist, target, contains('smooth'))
    
  model.data <- sapply(model.data, as.numeric) %>%
    as.data.frame
  
  g <- gbm(target ~ ., data=model.data, dist='bernoulli', n.trees=10000,
           interaction.depth=2, n.minobsinnode=5, shrinkage=.001, train.fraction=.75,
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
  
  predict.summary <- rbind(predict.summary, p)
}

ggplot(predict.summary) +
  geom_histogram(aes(prob), binwidth=.01)

file <- paste0("axa-gbm2-depth=2", ".csv.gz")

write.csv(predict.summary,gzfile(file),row.names=FALSE)

