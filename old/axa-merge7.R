rm(list=ls(all=TRUE))
options(scipen = 10)

library(dplyr)
library(ggplot2)
library(data.table)
library(parallel)
library(gbm)

detectCores()

setwd('/users/adeonari/downloads/AXA')

gbm1 <- read.csv('axa-gbm7-10k-depth=2-train=.75-shrink=.001-seed=1111.csv.gz')
gbm2 <- read.csv('axa-gbm7-10k-depth=2-train=100-shrink=.001-seed=2222.csv.gz')

gbm.merge <- gbm1 %>%
  inner_join(gbm2, by='driver_trip') %>%
  mutate(prob = .5*prob.x + .5*prob.y) %>%
  select(driver_trip,prob)

ggplot(gbm.merge) +
  geom_histogram(aes(prob), binwidth=.01)

summary(gbm.merge$prob)

file <- paste0("axa-gbm7-merge", ".csv.gz")
write.csv(gbm.merge,gzfile(file),row.names=FALSE)

################################
gbm.rand1 <- read.csv('axa-random7-10k-seed=1111.csv.gz') %>%
  rename(prob.rand1=prob)
ggplot(gbm.rand1) +
  geom_histogram(aes(prob.rand1), binwidth=.01)
summary(gbm.rand1$prob.rand1)

################################
gbm3 <- read.csv('axa-gbm6-2stage-cut=50-50-50-75-5k-10k-depth=1-2-train=100-100-seed=2222.csv.gz') %>%
  rename(prob3=prob)
gbm4 <- read.csv('axa-gbm6-2stage-50-75-5k-10k-depth=1-2-train=100-100-seed=2222.csv.gz') %>%
  rename(prob4=prob)

ggplot(gbm3) +
  geom_histogram(aes(prob3), binwidth=.01)

summary(gbm3$prob3)
summary(gbm4$prob4)

gbm.merge <- gbm1 %>%
  inner_join(gbm2, by='driver_trip') %>%
  inner_join(gbm3, by='driver_trip') %>%
  mutate(prob = .45*prob.x + .45*prob.y + .1*prob3) %>%
  select(driver_trip,prob)

ggplot(gbm.merge) +
  geom_histogram(aes(prob), binwidth=.01)

file <- paste0("axa-gbm7-merge", ".csv.gz")
write.csv(gbm.merge,gzfile(file),row.names=FALSE)


####################################
gbm1 <- read.csv('axa-gbm5-merge.csv.gz') 
gbm2 <- read.csv('axa-gbm2-25k-merge.csv.gz')
gbm3 <- read.csv('axa-gbm6-2stage-cut=50-50-50-75-5k-10k-depth=1-2-train=100-100-seed=2222.csv.gz') %>%
  rename(prob3=prob)

gbm.merge <- gbm1 %>%
  inner_join(gbm2, by='driver_trip') %>%
  inner_join(gbm3, by='driver_trip') %>%
  mutate(prob = .475*prob.x + .475*prob.y + .05*prob3) %>%
  select(driver_trip,prob)

ggplot(gbm.merge) +
  geom_histogram(aes(prob), binwidth=.01)

file <- paste0("axa-gbm2-5-6a-merge", ".csv.gz")
write.csv(gbm.merge,gzfile(file),row.names=FALSE)
