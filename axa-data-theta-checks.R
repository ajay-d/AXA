rm(list=ls(all=TRUE))
options(scipen = 10)

library(dplyr)
library(ggplot2)
library(data.table)
library(parallel)

sessionInfo()
detectCores()

setwd('/users/adeonari/downloads/AXA')

drivers <- list.files("./drivers/")
drivers <- drivers[1:24]
#drivers <- c(1,2,1701,3000)
n.drivers <- length(drivers)

driver.summary <- NULL
n <- detectCores()
if (file.exists('cluster.txt'))
  file.remove('cluster.txt')
cl <- makePSOCKcluster(n, outfile='cluster.txt')

#divvy up drivers by number of cores
d.per <- round(n.drivers/n)
d.list <- split(drivers, ceiling(seq_along(drivers)/d.per))

clusterExport(cl, c('d.list' ,'driver.summary'))
clusterEvalQ(cl,library(dplyr))
clusterEvalQ(cl,library(data.table))

x <- function(x){
  drivers <- d.list[[x]]
  for(driver in drivers) {
    print(driver)
    print(match(driver, drivers))
    dirPath <- paste0("./drivers/", driver, '/')
    
    currentDriver <- NULL
    
    for(i in 1:200) {
      
      currentData <- NULL
      
      trip <- read.csv(paste0(dirPath, i, ".csv"))
            
      trip.turns <- trip %>%
        #take median location at every 30 seconds to smooth out turns
        mutate(r = row_number()) %>%
        mutate(grp = ceiling(r/30)) %>%
        group_by(grp) %>%
        summarise(x = median(x),
                  y = median(y)) %>%
        mutate(x.diff = x-lag(x),
               y.diff = y-lag(y)) %>%
        mutate(x.turn = ifelse(sign(x.diff) != sign(lag(x.diff)) & sign(x.diff) != 0,
                               1,
                               0),
               y.turn = ifelse(sign(y.diff) != sign(lag(y.diff)) & sign(y.diff) != 0,
                               1,
                               0),
               u.turn = ifelse(x.turn+y.turn > 1,
                               1,
                               0)
              ) %>%
        mutate(dist.smooth = sqrt(x^2 + y^2),
               #theta = atan(y/x),
               theta = atan2(y,x),
               theta.deg = theta*180/pi,
               theta.deg = ifelse(theta.deg < 0,
                                  theta.deg+360,
                                  theta.deg),
               quadrant = (theta.deg/90) %% 4 + 1)
      
      
      currentData <- c(driver=driver,trip=i, duration=nrow(trip), 
                 min.theta=min(trip.turns$theta.deg),
                 max.theta=max(trip.turns$theta.deg),
                 min.x=min(trip.turns$x),
                 max.x=max(trip.turns$x),
                 min.y=min(trip.turns$y),
                 max.y=max(trip.turns$y),
                 min.quad=min(trip.turns$quadrant),
                 max.quad=max(trip.turns$quadrant)
                 )
      
      #currentData <- data.frame(lapply(currentData, type.convert), stringsAsFactors=FALSE)
      currentData <- as.data.table(t(currentData))
      #driver.summary <- rbind(driver.summary, currentData)
      l <- list(currentDriver, currentData)
      #is use.names=FALSE faster?
      currentDriver <- rbindlist(l, use.names=TRUE)
      
    }
    
    l <- list(driver.summary, currentDriver)
    driver.summary <- rbindlist(l, use.names=FALSE)
    
  }
  return(driver.summary)
}

start <- Sys.time()

driver.cl <- clusterApply(cl, 1:n, x)

stopCluster(cl)

message('time to run:')
print(Sys.time()-start)

driver.summary <- rbindlist(driver.cl, use.names=FALSE)
table(driver.summary$min.quad)
