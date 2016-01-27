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
drivers <- drivers[1:36]
#drivers <- c(1,2,1701,3000)
n.drivers <- length(drivers)

driver.summary <- NULL

n <- detectCores()
n <- 6

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
      
      trip <- trip %>%
        mutate(x.lag2 = lag(x, 2),
         y.lag2 = lag(y, 2),
         d1 = sqrt((x-lag(x))^2 + (y-lag(y))^2),
         d2 = sqrt((x-lag(x, 2))^2 + (y-lag(y, 2))^2),
         theta1 = atan2(y-lag(y),x-lag(x)),
         theta2 = atan2(y-lag(y,2),x-lag(x,2)),
         theta.diff = theta1-theta2,
         theta.deg = theta.diff*180/pi
         )
      
      trip.turns <- trip %>%
        select(x,y) %>%
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
#         mutate(dist.smooth = sqrt(x^2 + y^2),
#                #theta = atan(y/x),
#                theta = atan2(y,x),
#                theta.deg = theta*180/pi,
#                theta.deg = ifelse(theta.deg < 0,
#                                   theta.deg+360,
#                                   theta.deg),
#                quadrant = (theta.deg/90) %% 4 + 1,
#                quadrant = floor(quadrant))
        mutate(dist.smooth = sqrt(x^2 + y^2),
               theta1 = atan2(y-lag(y),x-lag(x)),
               theta2 = atan2(y-lag(y,2),x-lag(x,2)),
               theta.diff.sample = theta1-theta2
               )
      
#       quad <- trip.turns %>%
#         group_by(quadrant) %>%
#         tally()
#       quad <- data.frame('quadrant'=1:4) %>%
#         left_join(quad, by='quadrant') %>%
#         mutate(n = ifelse(is.na(n),
#                           0,
#                           n))
      
      dist <- sqrt((trip$x)^2 + (trip$y)^2)
      theta <- atan2(trip$y,trip$x)
      theta.alt <- trip$theta.diff
      
      speed <- sqrt(diff(trip$x)^2 + diff(trip$y)^2)
      #1m/s = 2.2369mph
      speed <- 2.2369 * speed
      speed.q <- quantile(speed, seq(0.05, 1, by = 0.05))
      names(speed.q) <- paste("speed", names(speed.q))
      
      acceleration <- diff(speed)
      acceleration.q <- quantile(acceleration, seq(0.05, 1, by = 0.05))
      names(acceleration.q) <- paste("accel", names(acceleration.q))

      jerk <- diff(acceleration)
      jerk.q <- quantile(jerk, seq(0.05, 1, by = 0.05))
      names(jerk.q) <- paste("jerk", names(jerk.q))

      theta.diff <- diff(theta)
      theta.diff.q <- quantile(theta.diff, seq(0.05, 1, by = 0.05), na.rm=TRUE)
      names(theta.diff.q) <- paste("theta diff", names(theta.diff.q))
    
      theta.sample.diff <- trip.turns$theta.diff.sample
      theta.sample.diff.q <- quantile(theta.sample.diff, seq(0.05, 1, by = 0.05), na.rm=TRUE)
      names(theta.sample.diff.q) <- paste("theta diff sample", names(theta.sample.diff.q))    

      theta.alt.q <- quantile(theta.alt, seq(0.05, 1, by = 0.05), na.rm=TRUE)
      names(theta.alt.q) <- paste("theta alt", names(theta.alt.q))    
  
      theta.diff.alt <- diff(theta.alt.q)
      theta.diff.alt.q <- quantile(theta.diff.alt, seq(0.05, 1, by = 0.05), na.rm=TRUE)
      names(theta.diff.alt.q) <- paste("theta diff alt", names(theta.diff.alt.q))

      #smoothing
      speed <- as.data.frame(speed) %>%
        mutate(x = 1:n())
      
      speed.smooth <- loess(speed$speed~speed$x, span = .05)
      #speed.smooth.q <- quantile(speed.smooth$fitted, c(seq(0, .04, by = 0.01), seq(0.05, .95, by = 0.05), seq(.96, 1, by = 0.01)))
      speed.smooth.q <- quantile(speed.smooth$fitted, seq(0, 1, by = 0.025))
      names(speed.smooth.q) <- paste("speed smooth", names(speed.smooth.q))      

      acceleration <- as.data.frame(acceleration) %>%
        mutate(x = 1:n())
      
      acceleration.smooth <- loess(acceleration$acceleration~acceleration$x, span = .05)
      #acceleration.smooth.q <- quantile(acceleration.smooth$fitted, c(seq(0, .04, by = 0.01), seq(0.05, .95, by = 0.05), seq(.96, 1, by = 0.01)))
      acceleration.smooth.q <- quantile(acceleration.smooth$fitted, seq(0, 1, by = 0.025))
      names(acceleration.smooth.q) <- paste("accel smooth", names(acceleration.smooth.q))
      
      jerk <- as.data.frame(jerk) %>%
        mutate(x = 1:n())
      
      jerk.smooth <- loess(jerk$jerk~jerk$x, span = .05)
      #jerk.smooth.q <- quantile(jerk.smooth$fitted, c(seq(0, .04, by = 0.01), seq(0.05, .95, by = 0.05), seq(.96, 1, by = 0.01)))
      jerk.smooth.q <- quantile(jerk.smooth$fitted, seq(0, 1, by = 0.025))
      names(jerk.smooth.q) <- paste("jerk smooth", names(jerk.smooth.q))

      theta.diff <- as.data.frame(theta.diff) %>%
        mutate(x = 1:n())
      
      theta.diff.smooth <- loess(theta.diff$theta.diff~theta.diff$x, span = .05)
      #theta.diff.smooth.q <- quantile(theta.diff.smooth$fitted, c(seq(0, .04, by = 0.01), seq(0.05, .95, by = 0.05), seq(.96, 1, by = 0.01)))
      theta.diff.smooth.q <- quantile(theta.diff.smooth$fitted, seq(0, 1, by = 0.025))
      names(theta.diff.smooth.q) <- paste("theta diff smooth", names(theta.diff.smooth.q)) 

      theta.alt <- as.data.frame(theta.alt) %>%
        mutate(x = 1:n())
      
      theta.alt.smooth <- loess(theta.alt$theta.alt~theta.alt$x, span = .05)
      #theta.alt.smooth.q <- quantile(theta.alt.smooth$fitted, c(seq(0, .04, by = 0.01), seq(0.05, .95, by = 0.05), seq(.96, 1, by = 0.01)))
      theta.alt.smooth.q <- quantile(theta.alt.smooth$fitted, seq(0, 1, by = 0.025))
      names(theta.alt.smooth.q) <- paste("theta alt smooth", names(theta.alt.smooth.q)) 

      #Angular change x speed
      angular.speed <- theta.diff.smooth$fitted * speed.smooth$fitted
      #angular.speed.q <- quantile(angular.speed, c(seq(0, .04, by = 0.01), seq(0.05, .95, by = 0.05), seq(.96, 1, by = 0.01)))
      angular.speed.q <- quantile(angular.speed, seq(0, 1, by = 0.025))
      names(angular.speed.q) <- paste("angular speed", names(angular.speed.q))

      angular.speed.alt <- theta.alt.smooth$fitted * speed.smooth$fitted
      #angular.speed.alt.q <- quantile(angular.speed.alt, c(seq(0, .04, by = 0.01), seq(0.05, .95, by = 0.05), seq(.96, 1, by = 0.01)))
      angular.speed.alt.q <- quantile(angular.speed.alt, seq(0, 1, by = 0.025))
      names(angular.speed.alt.q) <- paste("angular speed alt", names(angular.speed.alt.q))
      
      #Angular change x acceleration
      angular.accel <- theta.diff.smooth$fitted * acceleration.smooth$fitted
      #angular.accel.q <- quantile(angular.accel, c(seq(0, .04, by = 0.01), seq(0.05, .95, by = 0.05), seq(.96, 1, by = 0.01)))
      angular.accel.q <- quantile(angular.accel, seq(0, 1, by = 0.025))
      names(angular.accel.q) <- paste("angular accel", names(angular.accel.q))

      angular.accel.alt <- theta.alt.smooth$fitted * acceleration.smooth$fitted
      #angular.accel.alt.q <- quantile(angular.accel.alt, c(seq(0, .04, by = 0.01), seq(0.05, .95, by = 0.05), seq(.96, 1, by = 0.01)))
      angular.accel.alt.q <- quantile(angular.accel.alt, seq(0, 1, by = 0.025))
      names(angular.accel.alt.q) <- paste("angular accel alt", names(angular.accel.alt.q))
      
      #difference on smoothed speed
      acceleration.alt <- diff(speed.smooth$fitted)
      #acceleration.alt.q <- quantile(acceleration.alt, c(seq(0, .04, by = 0.01), seq(0.05, .95, by = 0.05), seq(.96, 1, by = 0.01)))
      acceleration.alt.q <- quantile(acceleration.alt, seq(0, 1, by = 0.025))
      names(acceleration.alt.q) <- paste("accel smooth alt", names(acceleration.alt.q))
      
      #difference on smoothed acceleration
      jerk.alt <- diff(acceleration.smooth$fitted)
      jerk.alt.q <- quantile(jerk.alt, seq(0.05, 1, by = 0.05))
      names(jerk.alt.q) <- paste("jerk smooth alt", names(jerk.alt.q))
      
      #take quantiles on smoothed dist away
      dist.q <- quantile(trip.turns$dist.smooth, seq(0.05, 1, by = 0.05))
      names(dist.q) <- paste("dist", names(dist.q))
      
      #take quantiles on sampled theta in radians
      #theta.q <- quantile(trip.turns$theta, seq(0.05, 1, by = 0.05), na.rm=TRUE)
      #names(theta.q) <- paste("theta", names(theta.q))
      
      currentData <- c(driver=driver,trip=i, duration=nrow(trip), 
                 max.dist = max(dist),
                 max.dist.smooth = max(trip.turns$dist.smooth),
                 end.dist = dist[nrow(trip)],
                 
                 theta.diff.q, theta.sample.diff.q, theta.diff.smooth.q, 
                 theta.alt.q, theta.diff.alt.q, theta.alt.smooth.q, 
                 angular.speed.q, angular.speed.alt.q, angular.accel.q, angular.accel.alt.q, 
                 speed.q, speed.smooth.q, 
                 acceleration.q, acceleration.smooth.q, acceleration.alt.q, 
                 jerk.q, jerk.smooth.q, jerk.alt.q)
      
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
driver.summary <- as.data.table(mapply(function(x) as.numeric(x), driver.summary))

setkey(driver.summary,driver, trip)

length(sort(unique(driver.summary$driver)))

save(driver.summary, file='drivers8.RData')
