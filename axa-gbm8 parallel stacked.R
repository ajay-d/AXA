rm(list=ls(all=TRUE))
options(scipen = 10)

library(dplyr)
library(ggplot2)
library(data.table)
library(parallel)
library(gbm)
library(glmnet)
library(e1071)
library(randomForest)

detectCores()

setwd('/users/adeonari/downloads/AXA')
#setwd("//W7GS-5121022/Science")

load('drivers8.RData')

#RF doesn't like weird variable names
#pct <- names(driver.summary)[grep('%',names(driver.summary), ignore.case = TRUE)]
#names(driver.summary)[grep('%',names(driver.summary), ignore.case = TRUE)] <- gsub('%', '', pct, fixed=TRUE)

#space <- names(driver.summary)[grep(' ',names(driver.summary), ignore.case = TRUE)]
#names(driver.summary)[grep(' ',names(driver.summary), ignore.case = TRUE)] <- gsub(' ', '.', space, fixed=TRUE)

setNames(driver.summary, gsub('%', '', names(driver.summary), fixed=TRUE))
setNames(driver.summary, gsub(' ', '', names(driver.summary), fixed=TRUE))

#driver.summary <- sapply(driver.summary, as.numeric) %>% as.data.frame

drivers <- sort(unique(driver.summary$driver))
drivers <- list.files("./drivers/")
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
clusterEvalQ(cl,library(glmnet))
clusterEvalQ(cl,library(e1071))
clusterEvalQ(cl,library(randomForest))
clusterExport(cl, c('d.list','drivers','driver.summary'))
clusterSetRNGStream(cl, 77777)

x <- function(x){
  drivers <- d.list[[x]]
  predict.cl <- NULL
  
  for(d in drivers) {
    print(paste0('driver:', d))
    print(match(d, drivers))
    
    curDriver <- driver.summary[driver==d] %>%
      mutate(target = 1)
    
	  #train glms on set 1
    nonDrivers1 <- driver.summary[driver!=d] %>%
      #filter(driver != d) %>%
      sample_n(1000) %>%
      mutate(target = 0)
    
  	#train rf on set 2
  	nonDrivers2 <- driver.summary[driver!=d] %>%
        anti_join(nonDrivers1, by=c('driver', 'trip')) %>%
        sample_n(1000) %>%
        mutate(target = 0)
	
  	#train gbm on set 3
  	nonDrivers3 <- driver.summary[driver!=d] %>%
        anti_join(nonDrivers1, by=c('driver', 'trip')) %>%
  	    anti_join(nonDrivers2, by=c('driver', 'trip')) %>%
        sample_n(1000) %>%
        mutate(target = 0)
	
    model.data1 <- rbindlist(list(curDriver, nonDrivers1), use.names=TRUE)
	  model.data2 <- rbindlist(list(curDriver, nonDrivers2), use.names=TRUE)
  	model.data3 <- rbindlist(list(curDriver, nonDrivers3), use.names=TRUE)    
	
    #model.data <- sapply(model.data, as.numeric) %>% as.data.frame
    
    target <- as.factor(model.data1$target)
    train1 <- data.matrix(model.data1 %>% select(-driver, -trip, -target))
    train2 <- data.matrix(model.data2 %>% select(-driver, -trip, -target))
  	train3 <- data.matrix(model.data3 %>% select(-driver, -trip, -target))
	
    glm.cv <- cv.glmnet(train1, target, family = "binomial", type.measure = "auc", nfolds = 5, alpha = 1, parallel=FALSE,
                        type.logistic="modified.Newton")
    
  	#predict on set 3 for gbm input
    p1 <- predict(glm.cv, newx = train3, s = "lambda.min", type="response")
    dimnames(p1)[[2]] <- 'glm1'
    
    #Try training on 1 and 2
    glm.cv <- cv.glmnet(train1, target, family = "binomial", type.measure = "auc", nfolds = 5, alpha = .5, parallel=FALSE,
                        type.logistic="modified.Newton")
    
	  #predict on set 3 for gbm input
    p2 <- predict(glm.cv, newx = train3, s = "lambda.min", type="response")
    dimnames(p2)[[2]] <- 'glm2'
    
    glm.cv <- cv.glmnet(train1, target, family = "binomial", type.measure = "auc", nfolds = 5, alpha = 0, parallel=FALSE,
                        type.logistic="modified.Newton")
    
	  #predict on set 3 for gbm input
    p3 <- predict(glm.cv, newx = train3, s = "lambda.min", type="response")
    dimnames(p3)[[2]] <- 'glm3'
    
#   	rf <- randomForest(as.factor(target) ~ ., data=model.data2 %>% select(-driver, -trip, -target), type='classification', 
#                        n.trees=5000, importance=TRUE)
#   	#print(rf)
#     #importance(rf)
#     
#   	#predict on set 3 for gbm input
#   	p.rf <- predict(rf, model.data3 %>% select(-driver, -trip, -target), type="prob")
#     rf1 <- p.rf[,'1']
	
	
#     #gamma:.001, cost:100, weight:.1
#     s <- svm(as.factor(target) ~ ., data=model.data %>% select(-driver, -trip), gamma=.001, cost=100, probability=TRUE, epsilon=.01,
#              class.weights=c('1'=1, '0'=.1))
#     
#     svm1 <- predict(s, model.data, probability=TRUE)
#     svm1<-attr(svm1, 'probabilities')[,1]
# 
#     #gamma:.001, cost:1000, weight:.25
#     s <- svm(as.factor(target) ~ ., data=model.data %>% select(-driver, -trip), gamma=.001, cost=1000, probability=TRUE, epsilon=.01,
#              class.weights=c('1'=1, '0'=.25))
#     
#     svm2 <- predict(s, model.data, probability=TRUE)
#     svm2<-attr(svm2, 'probabilities')[,1]
#     
#     tune.s <- tune.svm(as.factor(target) ~ ., data=model.data %>% select(-driver, -trip), gamma=10^(-4:-1), cost=10^(1:3),
#                        tunecontrol = tune.control(sampling = "fix")
#                        )
#     #tuned weight:.1
#     s <- svm(as.factor(target) ~ ., data=model.data %>% select(-driver, -trip), gamma=tune.s$best.parameters$gamma, cost=tune.s$best.parameters$cost,
#              probability=TRUE, epsilon=.01, class.weights=c('1'=1, '0'=.1))
#     
#     svm3 <- predict(s, model.data, probability=TRUE)
#     svm3<-attr(svm3, 'probabilities')[,1]
#     
#     #tuned weight:.25
#     s <- svm(as.factor(target) ~ ., data=model.data %>% select(-driver, -trip), gamma=tune.s$best.parameters$gamma, cost=tune.s$best.parameters$cost,
#              probability=TRUE, epsilon=.01, class.weights=c('1'=1, '0'=.25))
#     
#     svm4 <- predict(s, model.data, probability=TRUE)
#     svm4<-attr(svm4, 'probabilities')[,1]
    
    #model.data <- cbind(model.data, p1,p2,p3,svm1,svm2,svm3,svm4)
    #model.data3 <- cbind(model.data3, p1,p2,p3, rf1)
    model.data3 <- cbind(model.data3, p1,p2,p3)    

    g <- gbm(target ~ ., data=model.data3 %>% select(-driver, -trip), dist='bernoulli', n.trees=10000,
             interaction.depth=1, n.minobsinnode=5, shrinkage=.001, train.fraction=.75,
             keep.data=FALSE, verbose=FALSE)
    
    #summary(g) %>% head
    best.iter <- gbm.perf(g, method='test')
    print(paste0('best.iter:', best.iter))
    #summary(g, n.trees=best.iter)
    
    new.cols <- model.data3 %>%
      filter(target==1) %>%
      #select(starts_with('glm'), starts_with('svm'))
      select(starts_with('glm'), starts_with('svm'), starts_with('rf'), driver, trip)
      
    curDriver <- curDriver %>%
      #bind_cols(new.cols)
      inner_join(new.cols, by=c('driver', 'trip'))
      
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

file <- paste0("axa-gbm8-10k-depth=1-train=.75-shrink=.001-seed=77777-stacked=3.1", ".csv.gz")

write.csv(predict.summary,gzfile(file),row.names=FALSE)
