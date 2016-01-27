rm(list=ls(all=TRUE))
options(scipen = 10)

library(dplyr)
library(ggplot2)
library(data.table)
library(parallel)
library(gbm)

detectCores()

setwd('/users/adeonari/downloads/AXA')


gbm1 <- read.csv('axa-gbm6-25k-depth=2-train=100-shrink=.001-seed=2222.csv.gz')
gbm2 <- read.csv('axa-gbm6-25k-depth=2-train=.90-shrink=.001-seed=1111.csv.gz')

#gbm1 <- read.csv('axa-gbm5-10k-depth=1-train=.75-shrink=.001-seed=1111.csv.gz')
gbm3 <- read.csv('axa-gbm5-10k-depth=2-train=.75-shrink=.001-seed=2222.csv.gz')
gbm4 <- read.csv('axa-gbm5-10k-depth=3-train=.75-shrink=.001-seed=3333.csv.gz')

#gbm4 <- read.csv('axa-gbm4-10k-depth=2-train=.75-shrink=.001-seed=1111.csv.gz')
#gbm5 <- read.csv('axa-gbm4-10k-depth=2-train=.75-shrink=.001-seed=2222.csv.gz')

gbm.merge <- gbm1 %>%
  left_join(gbm2, by='driver_trip') %>%
  mutate(prob = .5*prob.x + .5*prob.y) %>%
  select(driver_trip,prob)

gbm1 <- rename(gbm1, prob1=prob)
gbm2 <- rename(gbm2, prob2=prob)
gbm3 <- rename(gbm3, prob3=prob)
gbm4 <- rename(gbm4, prob4=prob)

gbm.merge <- gbm1 %>%
  left_join(gbm2, by='driver_trip') %>%
  left_join(gbm3, by='driver_trip') %>%
  left_join(gbm4, by='driver_trip') %>%
  mutate(prob = .25*prob1 + .25*prob2 + .25*prob3 + .25*prob4)

gbm.merge <- select(gbm.merge, driver_trip,prob)

ggplot(gbm.merge) +
  geom_histogram(aes(prob), binwidth=.01)

file <- paste0("axa-gbm5-6-merge", ".csv.gz")

write.csv(gbm.merge,gzfile(file),row.names=FALSE)
#############################################################################################
gbm2 <- rename(gbm2, prob1=prob)
gbm3 <- rename(gbm3, prob2=prob)
gbm4 <- rename(gbm4, prob3=prob)
gbm5 <- rename(gbm5, prob4=prob)

gbm.merge <- gbm2 %>%
  left_join(gbm3, by='driver_trip') %>%
  left_join(gbm4, by='driver_trip') %>%
  left_join(gbm5, by='driver_trip') %>%
  mutate(prob = .25*prob1 + .25*prob2 + .25*prob3 + .25*prob4)

gbm.merge <- select(gbm.merge, driver_trip,prob)

ggplot(gbm.merge) +
  geom_histogram(aes(prob), binwidth=.01)

file <- paste0("axa-gbm5-merge", ".csv.gz")

write.csv(gbm.merge,gzfile(file),row.names=FALSE)
