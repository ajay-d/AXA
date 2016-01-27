rm(list=ls(all=TRUE))
options(scipen = 10)

library(dplyr)
library(ggplot2)
library(data.table)
library(gbm)

setwd('/users/adeonari/downloads/AXA')

copy.table <- function(obj, size = 16384) {
  clip <- paste('clipboard-', size, sep = '')
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = FALSE, sep = '\t')
  close(f)  
}

drivers <- list.files("./drivers/")
drivers <- drivers[1:2]
#drivers <- c(1,2,1701,3000)
n.drivers <- length(drivers)

trip <- read.csv("./drivers/1/200.csv")

speed <- sqrt(diff(trip$x)^2 + diff(trip$y)^2)
#1m/s = 2.2369mph
speed <- 2.2369 * speed

ggplot(data=trip1, aes(x=x, y=y)) + 
  geom_path()

gg.data <- 
  as.data.frame(speed) %>%
  mutate(x = 1:n())

trip.turns <- trip %>%
      mutate(row = row_number()) %>%
      #mutate(grp = ceiling(r/30)) %>%
      #group_by(grp) %>%
      #summarise(x = median(x),
      #          y = median(y)) %>%
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
      mutate(r = sqrt(x^2 + y^2),
            theta = atan(y/x)
            #theta = atan2(y,x)
            )

sum(trip.turns$x.turn, na.rm=T)
sum(trip.turns$y.turn, na.rm=T)
sum(trip.turns$u.turn, na.rm=T)

l.1 <- loess(gg.data$speed ~ gg.data$x)
l.2 <- loess(gg.data$speed ~ gg.data$x, span = .1)
l <- loess(gg.data$speed ~ gg.data$x, span = .05)

fit <- cbind(gg.data$x,
             l.1$fitted,
             l.2$fitted,
             l$fitted)
fit <- as.data.frame(fit)

fit <- cbind(gg.data$x,
             l$fitted)
fit <- as.data.frame(fit)

ggplot(gg.data, aes(x, speed)) +
  geom_point() +
  stat_smooth(method = 'loess') +
  geom_line(aes(fit$V1, fit$V2), color = 'red') +
  xlab("Time from Start of Trip (Seconds)") +
  ylab("Speed (Miles/Hour)")
  #ggtitle('Raw Speed \nDriver 1 / Trip 200')

library(grid)
ggplot(data=trip.turns, aes(x=x, y=y)) + 
  geom_path()

ggplot(data=trip.turns %>% filter(x > 450, x < 500), aes(x=x, y=y)) + 
  geom_path() + geom_point(size = 2) +
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=20)) +
  #xlab("X (Meters from (0,0)") +
  #ylab("Y (Meters from (0,0)")
  ggtitle('X,Y are Meters from (0,0) (Trip start location)')


ggplot(data=trip.turns %>% filter(x > 450, x < 500), aes(x=x, y=y)) + 
  geom_point(size = 2) + 
  #geom_text(aes(label=row), vjust=1, size = 3) +
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=20)) +
  #xlab("X (Meters from (0,0)") +
  #ylab("Y (Meters from (0,0)")
  ggtitle('X,Y are Meters from (0,0) (Trip start location)') + 
  geom_path(data=trip.turns %>% filter(row > 52, row < 85)) +
  geom_path(data=trip.turns %>% filter(row > 679, row < 708))

  
ggplot(data=trip.turns %>% filter(x > 0, x < 1000), aes(x=x, y=y)) + 
  geom_path()

ggplot(data=trip.turns, aes(x=r, y=theta)) + 
  geom_path() + coord_polar()


trip1 <- read.csv("./drivers/1/49.csv")
trip2 <- read.csv("./drivers/1/141.csv")

flip1 <- rbind(trip1 %>% mutate(trip='49'),
               trip2 %>% mutate(trip='141'))
  
ggplot(data=flip1, aes(x=x, y=y)) +
  geom_path(aes(color=trip))

trip1 <- read.csv("./drivers/1/113.csv")
trip2 <- read.csv("./drivers/1/190.csv")

flip1 <- rbind(trip1 %>% mutate(trip='113'),
               trip2 %>% mutate(trip='190'))
  
ggplot(data=flip1, aes(x=x, y=y)) +
  geom_path(aes(color=trip))

load('drivers6.RData')
d <- 10

curDriver <- driver.summary[driver==d] %>%
    mutate(target = 1)
  
nonDrivers <- driver.summary[driver!=d] %>%
  #filter(driver != d) %>%
  sample_n(1000) %>%
  mutate(target = 0)
  
model.data <- rbindlist(list(curDriver, nonDrivers), use.names=TRUE) %>%
  select(-driver, -trip)
    
  
g <- gbm(target ~ ., data=model.data, dist='bernoulli', n.trees=10000,
         interaction.depth=5, n.minobsinnode=5, shrinkage=.001, train.fraction=.75,
         #keep.data=TRUE, verbose=TRUE)
         keep.data=FALSE, verbose=FALSE)

summary(g)
best.iter <- gbm.perf(g, method='test')
print(best.iter)

pred <- summary(g) %>%
  filter(rel.inf > .75) %>%
  mutate(jerk = grepl('jerk', var),
         theta = grepl('theta', var),
         speed = grepl('speed', var),
         accel = grepl('accel', var),
         angular = grepl('angular', var))

sum(pred$jerk)
sum(pred$theta)
sum(pred$speed)
sum(pred$accel)
sum(pred$angular)

p <- predict(g, curDriver, 
                 n.trees=best.iter, type="response")
ggplot(as.data.frame(p)) +
  geom_histogram(aes(p), binwidth=.01) +
  xlab("Probability Trip was driven by Driver 10") +
  ylab("Frequency")

p2 <- predict(g, model.data, 
                 n.trees=best.iter, type="response")
p2 <- as.data.frame(cbind(p2, model.data$target))
names(p2) <- c('prob', 'target')

ggplot(p2) +
  geom_histogram(aes(prob), binwidth=.01) +
  facet_grid(~target) +
  xlab("Probability Trip was driven by Driver 10") +
  ylab("Frequency")

min(driver.summary$duration)
max(driver.summary$duration)


#plot all trips by one driver

driver=1
dirPath <- paste0("./drivers/", driver, '/')
    
currentDriver <- NULL

for(i in 1:200) {
  trip <- read.csv(paste0(dirPath, i, ".csv")) %>%
    mutate(trip = i)
  
  currentDriver <- rbind(currentDriver, trip, use.names=TRUE)
}

ggplot(currentDriver, aes(x=x, y=y)) +
  geom_path(aes(color=trip), alpha=.5) +
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=20)) +
  #xlab("X (Meters from (0,0)") +
  #ylab("Y (Meters from (0,0)")
  ggtitle('X,Y are Meters from (0,0) (Trip start location)')


trip <- read.csv("./drivers/1/136.csv")
ggplot(data=trip, aes(x=x, y=y)) + 
  geom_point() + 
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=20)) +
  ggtitle('X,Y are Meters from (0,0) (Trip start location)')
