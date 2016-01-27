options(scipen = 10)

library(dplyr)
library(data.table)
library(glmnet)
library(ggplot2)
#library(speedglm)
#library(biglm)
#library(doMC)
library(doParallel)

#registerDoMC(cores=4)
registerDoParallel(cores=6)

set.seed(2014)

setwd('/users/adeonari/downloads/AXA')

load('drivers.RData')

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
    select(-driver, -trip)
  
  #glm seems really slow
  #g <- glm(target ~ ., data=model.data, family = binomial("logit"))
  #p <- predict(g, curDriver, type = "response")
  
  target <- as.factor(model.data$target)
  x <- data.matrix(model.data %>% select(-target))
  #g <- glmnet(x, target, family = 'binomial')
  
  #alpha=1 #lasso
  #alpha=0 #ridge
  
  alpha <- 1
  
  #g.cv <- cv.glmnet(x, target, family = "binomial", type.measure = "auc", nfolds = 5, alpha = 1, parallel=TRUE) 
  g.cv <- cv.glmnet(x, target, family = "binomial", type.measure = "auc", nfolds = 5, alpha = alpha, parallel=TRUE,
                    type.logistic="modified.Newton")
  
  #g.cv$lambda.min
  p <- predict(g.cv, newx = data.matrix(curDriver %>% select(-driver, -trip, -target)), 
               s = "lambda.min", type="response")
  dimnames(p)[[2]] <- 'prob'
  
  p <- cbind(curDriver %>% select(driver, trip), p) %>%
    mutate(driver_trip = paste0(driver,'_', trip)) %>%
    select(driver_trip, prob)
  
  predict.summary <- rbind(predict.summary, p)
}

#hist(predict.summary$prob)

#plot(g)
#plot(g.cv)

ggplot(predict.summary) +
  geom_histogram(aes(prob), binwidth=.01)

file <- paste0("axa-logistic-", "alpha=", alpha, ".csv.gz")

write.csv(predict.summary,gzfile(file),row.names=FALSE)
