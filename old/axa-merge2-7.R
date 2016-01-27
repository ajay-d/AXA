rm(list=ls(all=TRUE))
options(scipen = 10)

library(dplyr)
library(ggplot2)
library(data.table)
library(parallel)
library(gbm)

detectCores()

setwd('/users/adeonari/downloads/AXA')

gbm1 <- read.csv('axa-gbm2-25k-depth=1-train=.75-shrink=.001-seed=12345.csv.gz')
gbm2 <- read.csv('axa-gbm2-25k-depth=1-train=.75-shrink=.001-seed=122014.csv.gz')

gbm.merge <- gbm1 %>%
  inner_join(gbm2, by='driver_trip') %>%
  mutate(prob = .5*prob.x + .5*prob.y) %>%
  select(driver_trip,prob)

ggplot(gbm.merge) +
  geom_histogram(aes(prob), binwidth=.01)

summary(gbm.merge$prob)

file <- paste0("axa-gbm2-merge", ".csv.gz")
write.csv(gbm.merge,gzfile(file),row.names=FALSE)

################################
gbm3 <- read.csv('axa-gbm7-25k-depth=1-train=.80-shrink=.001-seed=22222.csv.gz') %>%
  rename(prob3=prob)
gbm4 <- read.csv('axa-gbm7-25k-depth=1-train=.75-shrink=.001-seed=122014.csv.gz') %>%
  rename(prob4=prob)

ggplot(gbm3) +
  geom_histogram(aes(prob3), binwidth=.01)

summary(gbm3$prob3)
summary(gbm4$prob4)

gbm.merge <- gbm1 %>%
  inner_join(gbm2, by='driver_trip') %>%
  inner_join(gbm3, by='driver_trip') %>%
  inner_join(gbm4, by='driver_trip') %>%
  mutate(prob = .25*prob.x + .25*prob.y + .25*prob3 + .25*prob4) %>%
  select(driver_trip,prob)

ggplot(gbm.merge) +
  geom_histogram(aes(prob), binwidth=.01)

file <- paste0("axa-gbm2-7-25k-merge", ".csv.gz")
write.csv(gbm.merge,gzfile(file),row.names=FALSE)

