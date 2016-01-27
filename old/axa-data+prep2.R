rm(list=ls(all=TRUE))
options(scipen = 10)

library(dplyr)
library(ggplot2)
library(data.table)

setwd('/users/adeonari/downloads/AXA')

drivers <- list.files("./drivers/")
#drivers <- drivers[1:20]
#drivers <- c(1,2,1701,3000)
n.drivers <- length(drivers)


driver.summary <- NULL

for(driver in drivers) {
  print(driver)
  print(match(driver, drivers))
  dirPath <- paste0("./drivers/", driver, '/')
  
  currentDriver <- NULL
  
  for(i in 1:200) {
    
    currentData <- NULL
    
    trip <- read.csv(paste0(dirPath, i, ".csv"))
    dist <- sqrt((trip$x)^2 + (trip$y)^2)
    
    speed <- sqrt(diff(trip$x)^2 + diff(trip$y)^2)
    #1m/s = 2.2369mph
    speed <- 2.2369 * speed
    speed.q <- quantile(speed, seq(0.05, 1, by = 0.05))
    names(speed.q) <- paste("speed", names(speed.q))
    
    acceleration <- diff(speed)
    acceleration.q <- quantile(acceleration, seq(0.05, 1, by = 0.05))
    names(acceleration.q) <- paste("accel", names(acceleration.q))

    acceleration <- as.data.frame(acceleration) %>%
      mutate(x = 1:n())
    
    acceleration.smooth <- loess(acceleration$acceleration~acceleration$x, span = .05)
    acceleration.smooth.q <- quantile(acceleration.smooth$fitted, seq(0.05, 1, by = 0.05))
    names(acceleration.smooth.q) <- paste("accel smooth", names(acceleration.smooth.q))
    
    speed <- as.data.frame(speed) %>%
      mutate(x = 1:n())
    
    speed.smooth <- loess(speed$speed~speed$x, span = .05)
    speed.smooth.q <- quantile(speed.smooth$fitted, seq(0.05, 1, by = 0.05))
    names(speed.smooth.q) <- paste("speed smooth", names(speed.smooth.q))

    #difference on smoothed speed
    acceleration.alt <- diff(speed.smooth$fitted)
    acceleration.alt.q <- quantile(acceleration.alt, seq(0.05, 1, by = 0.05))
    names(acceleration.alt.q) <- paste("accel smooth alt", names(acceleration.alt.q))
    
    #difference on smoothed acceleration
    jerk <- diff(acceleration.smooth$fitted)
    jerk.q <- quantile(jerk, seq(0.05, 1, by = 0.05))
    names(jerk.q) <- paste("jerk smooth", names(jerk.q))
    
    currentData <- c(driver=driver,trip=i, duration=nrow(trip), 
                     max.dist = max(dist),
                     end.dist = dist[nrow(trip)],
                     speed.q, speed.smooth.q, 
                     acceleration.q, acceleration.smooth.q, acceleration.alt.q, jerk.q)

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

setkey(driver.summary,driver, trip)

#driver.summary <- as.data.frame(driver.summary, row.names=FALSE)

save(driver.summary, file='drivers2.RData')



