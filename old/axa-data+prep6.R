rm(list=ls(all=TRUE))
options(scipen = 10)

library(dplyr)
library(ggplot2)
library(data.table)

setwd('/users/adeonari/downloads/AXA')

drivers <- list.files("./drivers/")
drivers <- drivers[1:2]
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
    
    trip.turns <- trip %>%
      mutate(r = row_number()) %>%
      mutate(grp = ceiling(r/20)) %>%
      group_by(grp) %>%
      summarise(x = mean(x),
                y = mean(y)) %>%
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
               quadrant = (theta.deg/90) %% 4 + 1,
               quadrant = floor(quadrant),
             theta.diff = theta-lag(theta))
      
      quad <- trip.turns %>%
        group_by(quadrant) %>%
        tally()
      quad <- data.frame('quadrant'=1:4) %>%
        left_join(quad, by='quadrant') %>%
        mutate(n = ifelse(is.na(n),
                          0,
                          n))
    
      dist <- sqrt((trip$x)^2 + (trip$y)^2)
      theta <- atan2(trip$y,trip$x)
    
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
    
      theta.sample.diff <- diff(trip.turns$theta)
      theta.sample.diff.q <- quantile(theta.sample.diff, seq(0.05, 1, by = 0.05))
      names(theta.sample.diff.q) <- paste("theta sample", names(theta.sample.diff.q))
    
      #smoothing
      speed <- as.data.frame(speed) %>%
        mutate(x = 1:n())
      
      speed.smooth <- loess(speed$speed~speed$x, span = .05)
      speed.smooth.q <- quantile(speed.smooth$fitted, seq(0.05, 1, by = 0.05))
      names(speed.smooth.q) <- paste("speed smooth", names(speed.smooth.q))      

      acceleration <- as.data.frame(acceleration) %>%
        mutate(x = 1:n())
      
      acceleration.smooth <- loess(acceleration$acceleration~acceleration$x, span = .05)
      acceleration.smooth.q <- quantile(acceleration.smooth$fitted, seq(0.05, 1, by = 0.05))
      names(acceleration.smooth.q) <- paste("accel smooth", names(acceleration.smooth.q))
      
      jerk <- as.data.frame(jerk) %>%
        mutate(x = 1:n())
      
      jerk.smooth <- loess(jerk$jerk~jerk$x, span = .05)
      jerk.smooth.q <- quantile(jerk.smooth$fitted, seq(0.05, 1, by = 0.05))
      names(jerk.smooth.q) <- paste("jerk smooth", names(jerk.smooth.q))
      
      theta.diff <- as.data.frame(theta.diff) %>%
        mutate(x = 1:n())
      theta.diff.smooth <- loess(theta.diff$theta.diff~theta.diff$x, span = .05)
      theta.diff.smooth.q <- quantile(theta.diff.smooth$fitted, seq(0.05, 1, by = 0.05))
      names(theta.diff.smooth.q) <- paste("theta diff smooth", names(theta.diff.smooth.q))  
        
      #Angular change x speed
      angular.speed <- theta.diff.smooth$fitted * speed.smooth$fitted
      angular.accel <- theta.diff.smooth$fitted * acceleration.smooth$fitted
      
      #difference on smoothed speed
      acceleration.alt <- diff(speed.smooth$fitted)
      acceleration.alt.q <- quantile(acceleration.alt, seq(0.05, 1, by = 0.05))
      names(acceleration.alt.q) <- paste("accel smooth alt", names(acceleration.alt.q))
      
      #difference on smoothed acceleration
      jerk.alt <- diff(acceleration.smooth$fitted)
      jerk.alt.q <- quantile(jerk.alt, seq(0.05, 1, by = 0.05))
      names(jerk.alt.q) <- paste("jerk smooth alt", names(jerk.alt.q))
      
      #take quantiles on smoothed dist away
      dist.q <- quantile(trip.turns$dist.smooth, seq(0.05, 1, by = 0.05))
      names(dist.q) <- paste("dist", names(dist.q))
    
      currentData <- c(driver=driver,trip=i, duration=nrow(trip), 
                       max.dist = max(dist),
                       max.dist.smooth = max(trip.turns$dist.smooth),
                       end.dist = dist[nrow(trip)],
                       end.dist.smooth = trip.turns[nrow(trip.turns),'dist.smooth'][[1]],
                       #x.turn = sum(trip.turns$x.turn, na.rm=T),
                       #y.turn = sum(trip.turns$y.turn, na.rm=T),
                       u.turn = sum(trip.turns$u.turn, na.rm=T),
                       #quad1 = quad[[1,'n']] / nrow(trip.turns),
                       #quad2 = quad[[2,'n']] / nrow(trip.turns),
                       #quad3 = quad[[3,'n']] / nrow(trip.turns),
                       #quad4 = quad[[4,'n']] / nrow(trip.turns),
                       dist.q, theta.diff.q, theta.sample.diff.q, theta.diff.smooth.q,
                       speed.q, speed.smooth.q, 
                       acceleration.q, acceleration.smooth.q, acceleration.alt.q, 
                       jerk.q, jerk.alt.q)

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

trip1 <- read.csv("./drivers/1/1.csv")



ggplot(data=trip1, aes(x=x, y=y)) + 
  geom_path()

trip.turns <- trip1 %>%
      mutate(r = row_number()) %>%
      #sample every 20th row to calculate number of turns
      filter(r %% 20 == 0) %>%
      #sample 25% of the rows to calculate number of turns
      #sample_frac(.25) %>%
      #arrange(r) %>%
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
      mutate(theta = atan(y/x) * 180/pi)

sum(trip.turns$x.turn, na.rm=T)
sum(trip.turns$y.turn, na.rm=T)
sum(trip.turns$u.turn, na.rm=T)


trip.turns <- trip1 %>%
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
      mutate(theta = atan(y/x) * 180/pi,
             quadrant = (theta/90) %% 4 + 1)

summary(trip.turns$theta)
table(trip.turns$quadrant)

ggplot(data=trip.turns, aes(x=x, y=y)) + 
  geom_path()

sum(trip.turns$x.turn, na.rm=T)
sum(trip.turns$y.turn, na.rm=T)
sum(trip.turns$u.turn, na.rm=T)

t1 <- filter(driver.summary, u.turn==9)


