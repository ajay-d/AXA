rm(list=ls(all=TRUE))
options(scipen = 10)

library(dplyr)
library(ggplot2)
library(data.table)
library(parallel)
library(gbm)

detectCores()

setwd('/users/adeonari/downloads/AXA')


gbm1 <- read.csv('axa-gbm6-10k-depth=2-train=.75-shrink=.001-seed=4444.csv.gz')
gbm2 <- read.csv('axa-gbm6-10k-depth=2-train=.90-shrink=.001-seed=6666.csv.gz')
gbm3 <- read.csv('axa-gbm6-10k-depth=2-train=100-shrink=.001-seed=5555.csv.gz')


gbm4 <- read.csv('axa-gbm6-10k-depth=1-train=.75-shrink=.001-seed=3333.csv.gz')
gbm5 <- read.csv('axa-gbm6-10k-depth=1-train=100-shrink=.001-seed=1111.csv.gz')


gbm6 <- read.csv('axa-gbm6-25k-depth=2-train=.90-shrink=.001-seed=1111.csv.gz')
gbm7 <- read.csv('axa-gbm6-25k-depth=2-train=100-shrink=.001-seed=2222.csv.gz')

model <- 6

ggplot(get(paste0('gbm', model))) +
  geom_histogram(aes(prob), binwidth=.01)
summary(get(paste0('gbm', model))$prob)


gbm1 <- rename(gbm1, prob1=prob)
gbm2 <- rename(gbm2, prob2=prob)
gbm3 <- rename(gbm3, prob3=prob)
gbm4 <- rename(gbm4, prob4=prob)
gbm5 <- rename(gbm5, prob5=prob)
gbm6 <- rename(gbm6, prob6=prob)
gbm7 <- rename(gbm7, prob7=prob)

gbm.merge <- gbm1 %>%
  left_join(gbm2, by='driver_trip') %>%
  left_join(gbm3, by='driver_trip') %>%
  left_join(gbm4, by='driver_trip') %>%
  left_join(gbm5, by='driver_trip') %>%
  left_join(gbm6, by='driver_trip') %>%
  left_join(gbm7, by='driver_trip') %>%
  mutate(prob = .2*prob1 + .2*prob2 + .2*prob3 + 
           .1*prob4 + .1*prob5 +
           .1*prob6 + .1*prob7)

gbm.merge <- select(gbm.merge, driver_trip,prob)

ggplot(gbm.merge) +
  geom_histogram(aes(prob), binwidth=.01)

summary(gbm.merge$prob)

file <- paste0("axa-gbm6c7-merge", ".csv.gz")

write.csv(gbm.merge,gzfile(file),row.names=FALSE)
#############################################################################################
gbm1 <- read.csv('axa-gbm2-25k-depth=1.csv.gz') %>%
  rename(prob1=prob)
gbm2 <- read.csv('axa-gbm2-10k-depth=2.csv.gz') %>%
  rename(prob2=prob)
gbm3 <- read.csv('axa-gbm4-10k-depth=2-train=.75-shrink=.001-seed=1111.csv.gz') %>%
  rename(prob3=prob)
gbm4 <- read.csv('axa-gbm4-10k-depth=2-train=.75-shrink=.001-seed=2222.csv.gz') %>%
  rename(prob4=prob)

gbm5 <- read.csv('axa-gbm5-10k-depth=2-train=.75-shrink=.001-seed=2222.csv.gz') %>%
  rename(prob5=prob)
gbm6 <- read.csv('axa-gbm5-10k-depth=3-train=.75-shrink=.001-seed=3333.csv.gz') %>%
  rename(prob6=prob)


deep <- read.csv('axa-deep-act=TanhWithDropout.csv.gz') %>%
  rename(deep.prob = prob)

gbm.merge <- gbm1 %>%
  inner_join(gbm2, by='driver_trip') %>%
  inner_join(gbm3, by='driver_trip') %>%
  inner_join(gbm4, by='driver_trip') %>%
  mutate(gbm.prob = .25*prob1 + .25*prob2 + .25*prob3 + .25*prob4) %>% 
  select(driver_trip,gbm.prob)


ggplot(gbm.merge) +
  geom_histogram(aes(gbm.prob), binwidth=.01)

summary(gbm.merge$gbm.prob)

gbm.merge <- gbm.merge %>%
  inner_join(gbm5, by='driver_trip') %>%
  inner_join(gbm6, by='driver_trip') %>%
  mutate(prob = .5*gbm.prob + .25*prob5 + .25*prob6) %>%
  select(driver_trip,prob)

ggplot(gbm.merge) +
  geom_histogram(aes(prob), binwidth=.01)

summary(gbm.merge$prob)

file <- paste0("axa-gbm2-4-5-merge", ".csv.gz")
write.csv(gbm.merge,gzfile(file),row.names=FALSE)
