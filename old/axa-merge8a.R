rm(list=ls(all=TRUE))
options(scipen = 10)

library(dplyr)
library(ggplot2)
library(data.table)
library(parallel)
library(gbm)

detectCores()

setwd('/users/adeonari/downloads/AXA')

stack8.1 <- read.csv('axa-gbm8-2stage-cut2=10-0-10k-25k-depth=1-1-train=100-75-seed=12345.csv.gz') %>%
  rename(prob1=prob)
stack8.2 <- read.csv('axa-gbm8-10k-depth=1-train=.75-shrink=.001-seed=77777-stacked=3.1.csv.gz') %>%
  rename(prob2=prob)

gbm8.1 <- read.csv('axa-gbm8-25k-depth=1-train=.75-shrink=.001-seed=122014.csv.gz') %>%
  rename(prob3=prob)

gbm2.1 <- read.csv('axa-gbm2-25k-depth=1-train=.75-shrink=.001-seed=12345.csv.gz') %>%
  rename(prob3=prob)
gbm2.2 <- read.csv('axa-gbm2-25k-depth=1-train=.75-shrink=.001-seed=122014.csv.gz') %>%
  rename(prob4=prob)

ggplot(stack8.1) +
  geom_histogram(aes(prob1), binwidth=.01)
summary(stack8.1$prob1)

ggplot(stack8.2) +
  geom_histogram(aes(prob2), binwidth=.01)
summary(stack8.2$prob2)

gbm.merge.1 <- stack8.1 %>%
  inner_join(stack8.2, by='driver_trip') %>%
  mutate(prob = .5*prob1 + .5*prob2) %>%
  select(driver_trip,prob)

gbm.merge.2 <- stack8.1 %>%
  inner_join(stack8.2, by='driver_trip') %>%
  inner_join(gbm2.1, by='driver_trip') %>%
  inner_join(gbm2.2, by='driver_trip') %>%
  #inner_join(gbm7.1, by='driver_trip') %>%
  #inner_join(gbm7.2, by='driver_trip') %>%
  #mutate(prob.merge = .25*prob3 + .25*prob4 + .25*prob5 + .25*prob6) %>%
  mutate(prob = .25*prob1 + .25*prob2 + .25*prob3 + .25*prob4) %>%
  select(driver_trip,prob)

ggplot(gbm.merge.1) +
  geom_histogram(aes(prob), binwidth=.01)
summary(gbm.merge.1$prob)

ggplot(gbm.merge.2) +
  geom_histogram(aes(prob), binwidth=.01)
summary(gbm.merge.2$prob)

file <- paste0("axa-gbm8-2stage-merge1", ".csv.gz")
write.csv(gbm.merge.1,gzfile(file),row.names=FALSE)

file <- paste0("axa-gbm8-2stage-merge2", ".csv.gz")
write.csv(gbm.merge.2,gzfile(file),row.names=FALSE)
############################################################################


