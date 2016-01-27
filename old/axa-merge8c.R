rm(list=ls(all=TRUE))
options(scipen = 10)

library(dplyr)
library(ggplot2)
library(data.table)
library(parallel)
library(gbm)

detectCores()

setwd('/users/adeonari/downloads/AXA')

stack8.1 <- read.csv('axa-gbm8-2stage-cut2=10-0-10k-25k-depth=1-1-train=100-75-seed=12345.csv.gz') %>% rename(prob.stack8.1=prob)
stack8.2 <- read.csv('axa-gbm8-10k-depth=1-train=.75-shrink=.001-seed=77777-stacked=3.1.csv.gz') %>% rename(prob.stack8.2=prob)

deep <- read.csv('axa-deep-act=TanhWithDropout.csv.gz') %>% rename(prob.deep = prob)

gbm2.1 <- read.csv('axa-gbm2-25k-depth=1-train=.75-shrink=.001-seed=12345.csv.gz') %>% rename(prob.gbm2.1=prob)
gbm2.2 <- read.csv('axa-gbm2-25k-depth=1-train=.75-shrink=.001-seed=122014.csv.gz') %>% rename(prob.gbm2.2=prob)

stack7.1 <- read.csv('axa-gbm7-10k-depth=1-train=.75-shrink=.001-seed=77777-stacked=3.csv.gz') %>% rename(prob.stack7.1=prob) 
stack7.2 <- read.csv('axa-gbm7-2stage-cut2=10-0-10k-25k-depth=1-1-train=100-75-seed=12345.csv.gz') %>% rename(prob.stack7.2=prob)

gbm2.1 <- read.csv('axa-gbm2-25k-depth=1-train=.75-shrink=.001-seed=12345.csv.gz') %>% rename(prob.gbm2.1=prob) 
gbm2.2 <- read.csv('axa-gbm2-25k-depth=1-train=.75-shrink=.001-seed=122014.csv.gz') %>% rename(prob.gbm2.2=prob)

gbm7.1 <- read.csv('axa-gbm7-25k-depth=1-train=.80-shrink=.001-seed=22222.csv.gz') %>% rename(prob.gbm7.1=prob) 
gbm7.2 <- read.csv('axa-gbm7-25k-depth=1-train=.75-shrink=.001-seed=122014.csv.gz') %>% rename(prob.gbm7.2=prob) 

stack8.3 <- read.csv('axa-gbm8-2stage-cut2=20-50-10k-10k-depth=1-3-train=100-100-seed=88888.csv.gz') %>% rename(prob.stack8.3=prob)
stack8.4 <- read.csv('axa-gbm8-2stage-cut2=20-50-10k-10k-depth=1-3-train=100-100-seed=99999.csv.gz') %>% rename(prob.stack8.4=prob)

ggplot(stack8.1) +
  geom_histogram(aes(prob.stack8.1), binwidth=.01)
summary(stack8.1$prob.stack8.1)
ggplot(stack8.2) +
  geom_histogram(aes(prob.stack8.2), binwidth=.01)
summary(stack8.2$prob.stack8.2)
ggplot(stack8.3) +
  geom_histogram(aes(prob.stack8.3), binwidth=.01)
summary(stack8.3$prob.stack8.3)

ggplot(gbm2.1) +
  geom_histogram(aes(prob.gbm2.1), binwidth=.01)
summary(gbm2.1$prob.gbm2.1)

##### .90417 #####
gbm.merge.1 <-  gbm2.1 %>% 
  inner_join(gbm2.2, by='driver_trip') %>% 
  inner_join(stack8.2, by='driver_trip') %>% 
  inner_join(stack8.3, by='driver_trip') %>% 
  mutate(prob = .25*prob.gbm2.1 + .25*prob.gbm2.2 + .25*prob.stack8.2 + .25*prob.stack8.3) %>% 
  select(driver_trip,prob)

ggplot(gbm.merge.1) +
  geom_histogram(aes(prob), binwidth=.01)
summary(gbm.merge.1$prob)

##### .88904 #####
gbm.merge.2 <- stack8.3 %>%
  inner_join(stack8.4, by='driver_trip') %>%
  mutate(prob = .5*prob.stack8.3 + .5*prob.stack8.4) %>%
  select(driver_trip,prob)

##### .90063 #####
gbm.merge.3 <- gbm2.1 %>%
  inner_join(gbm2.2, by='driver_trip') %>% 
  inner_join(stack8.3, by='driver_trip') %>% 
  inner_join(stack8.4, by='driver_trip') %>% 
  mutate(prob = .25*prob.gbm2.1 + .25*prob.gbm2.2 + .25*prob.stack8.3 + .25*prob.stack8.4) %>% 
  select(driver_trip,prob)

ggplot(gbm.merge.1) +
  geom_histogram(aes(prob), binwidth=.01)
summary(gbm.merge.1$prob)

ggplot(gbm.merge.2) +
  geom_histogram(aes(prob), binwidth=.01)
summary(gbm.merge.2$prob)

ggplot(gbm.merge.3) +
  geom_histogram(aes(prob), binwidth=.01)
summary(gbm.merge.3$prob)

file <- paste0("axa-gbm2-8-merge1c", ".csv.gz")
write.csv(gbm.merge.1,gzfile(file),row.names=FALSE)

file <- paste0("axa-gbm8-stack3-4", ".csv.gz")
write.csv(gbm.merge.2,gzfile(file),row.names=FALSE)

file <- paste0("axa-gbm2-8-stack3-4", ".csv.gz")
write.csv(gbm.merge.3,gzfile(file),row.names=FALSE)

#########################################################

merge1 <- read.csv('axa-gbm2-7-8-merge1-deep.csv.gz') %>% rename(m1=prob)
merge2 <- read.csv('axa-gbm2-8-merge1c.csv.gz') %>% rename(m2=prob)

##### 0.90492 #####
double.merge <- merge1 %>%
  inner_join(merge2) %>%
  mutate(prob = .5*m1 + .5*m2) %>% 
  select(driver_trip,prob)

ggplot(double.merge) +
  geom_histogram(aes(prob), binwidth=.01)
summary(double.merge$prob)

file <- paste0("axa-gbm2-8-double-merge", ".csv.gz")
write.csv(double.merge,gzfile(file),row.names=FALSE)

########################################################

gbm.merge.1 <-  stack7.1 %>% 
  inner_join(stack7.2, by='driver_trip') %>% 
  inner_join(gbm2.1, by='driver_trip') %>% 
  inner_join(gbm2.2, by='driver_trip') %>% 
  inner_join(stack8.1, by='driver_trip') %>% 
  inner_join(stack8.2, by='driver_trip') %>% 
  inner_join(stack8.3, by='driver_trip') %>% 
  inner_join(stack8.4, by='driver_trip') %>% 
  mutate(prob = .1*prob.stack7.1 + .1*prob.stack7.2 + .2*prob.gbm2.1 + .2*prob.gbm2.2 + .2*prob.stack8.3 + .2*prob.stack8.4) %>% 
  select(driver_trip,prob)

##### 0.90276 #####
ggplot(gbm.merge.1) +
  geom_histogram(aes(prob), binwidth=.01)
summary(double.merge$prob)

file <- paste0("axa-gbm2-7-8-stack", ".csv.gz")
write.csv(gbm.merge.1,gzfile(file),row.names=FALSE)

######################################################

merge1 <- read.csv('axa-gbm2-8-double-merge.csv.gz') %>% rename(prob1=prob)
merge2 <- read.csv('axa-gbm2-8-merge1c.csv.gz') %>% rename(prob2=prob)

gbm.double.merge <-  merge1 %>% 
  inner_join(merge2, by='driver_trip') %>% 
  mutate(prob = .5*prob1 + .5*prob2) %>% 
  select(driver_trip,prob)

##### 0.90276 #####
ggplot(gbm.double.merge) +
  geom_histogram(aes(prob), binwidth=.01)
summary(gbm.double.merge$prob)

file <- paste0("axa-gbm-double-merge", ".csv.gz")
write.csv(gbm.double.merge,gzfile(file),row.names=FALSE)

