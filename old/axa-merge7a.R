rm(list=ls(all=TRUE))
options(scipen = 10)

library(dplyr)
library(ggplot2)
library(data.table)
library(parallel)
library(gbm)

detectCores()

setwd('/users/adeonari/downloads/AXA')

gbm1 <- read.csv('axa-gbm7-10k-depth=2-train=.75-shrink=.001-seed=122014.csv.gz') %>%
  rename(prob1=prob)
gbm2 <- read.csv('axa-gbm7-10k-depth=2-train=.80-shrink=.001-seed=12345.csv.gz') %>%
  rename(prob2=prob)
gbm3 <- read.csv('axa-gbm7-25k-depth=1-train=.75-shrink=.001-seed=122014.csv.gz') %>%
  rename(prob3=prob)
gbm4 <- read.csv('axa-gbm7-25k-depth=1-train=.80-shrink=.001-seed=22222.csv.gz') %>%
  rename(prob4=prob)

svm1 <- read.csv('axa-svm7-gamma=10(-4,-1)-cost=5(1,3)-epsilon=01-weights(1,1)-seed=1111.csv.gz') %>%
  rename(prob1=prob)
svm2 <- read.csv('axa-svm7-gamma=10(-4,-1)-cost=5(1,3)-epsilon=01-weights(1,25)-seed=12345.csv.gz') %>%
  rename(prob2=prob)

svm.merge <- svm1 %>%
  inner_join(svm2, by='driver_trip') %>%
  mutate(prob.svm = .5*prob1 + .5*prob2) %>%
  select(driver_trip,prob.svm)

ggplot(svm.merge) +
  geom_histogram(aes(prob.svm), binwidth=.01)

summary(svm.merge$prob.svm)

gbm.merge <- gbm1 %>%
  inner_join(gbm2, by='driver_trip') %>%
  inner_join(gbm3, by='driver_trip') %>%
  inner_join(gbm4, by='driver_trip') %>%
  mutate(prob.gbm = .25*prob1 + .25*prob2 + .25*prob3 + .25*prob4) %>%
  inner_join(svm.merge, by='driver_trip')

gbm.merge1 <- gbm.merge %>%
  rename(prob = prob.gbm) %>%
  select(driver_trip,prob)

file <- paste0("axa-gbm7-merge", ".csv.gz")
write.csv(gbm.merge1,gzfile(file),row.names=FALSE)

svm.merge1 <- svm.merge %>%
  rename(prob = prob.svm) %>%
  select(driver_trip,prob)

file <- paste0("axa-svm7-merge", ".csv.gz")
write.csv(svm.merge1,gzfile(file),row.names=FALSE)

gbm.svm.merge <- gbm.merge %>%
  mutate(prob = .05*prob.svm + .95*prob.gbm) %>%
  select(driver_trip,prob)
file <- paste0("axa-gbm-svm7-merge", ".csv.gz")
write.csv(gbm.svm.merge,gzfile(file),row.names=FALSE)
