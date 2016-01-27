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

gbm.merge.1 <-  stack7.1 %>% 
  inner_join(stack7.2, by='driver_trip') %>% 
  inner_join(gbm2.1, by='driver_trip') %>% 
  inner_join(gbm2.2, by='driver_trip') %>% 
  inner_join(gbm7.1, by='driver_trip') %>% 
  inner_join(gbm7.2, by='driver_trip') %>% 
  mutate(prob = .2*prob.stack7.1 + .2*prob.stack7.2 + .2*prob.gbm2.1 + .2*prob.gbm2.2 + .1*prob.gbm7.1 + .1*prob.gbm7.2) %>% 
  select(driver_trip,prob)

gbm.merge.2 <- stack8.1 %>%
  inner_join(stack8.2, by='driver_trip') %>%
  inner_join(stack7.1, by='driver_trip') %>%
  inner_join(stack7.2, by='driver_trip') %>% 
  inner_join(gbm2.1, by='driver_trip') %>% 
  inner_join(gbm2.2, by='driver_trip') %>% 
  inner_join(gbm7.1, by='driver_trip') %>% 
  inner_join(gbm7.2, by='driver_trip') %>%
  mutate(prob = .2*prob.stack8.1 + .2*prob.stack8.2 + .2*prob.gbm2.1 + .2*prob.gbm2.2 + .1*prob.gbm7.1 + .1*prob.gbm7.2) %>%
  select(driver_trip,prob)

ggplot(gbm.merge.1) +
  geom_histogram(aes(prob), binwidth=.01)
summary(gbm.merge.1$prob)

ggplot(gbm.merge.2) +
  geom_histogram(aes(prob), binwidth=.01)
summary(gbm.merge.2$prob)

file <- paste0("axa-gbm2-7-8-merge1", ".csv.gz")
write.csv(gbm.merge.1,gzfile(file),row.names=FALSE)

file <- paste0("axa-gbm2-7-8-merge2", ".csv.gz")
write.csv(gbm.merge.2,gzfile(file),row.names=FALSE)
############################################################################

gbm.merge.1.deep <-  stack7.1 %>% 
  inner_join(stack7.2, by='driver_trip') %>% 
  inner_join(gbm2.1, by='driver_trip') %>% 
  inner_join(gbm2.2, by='driver_trip') %>% 
  inner_join(gbm7.1, by='driver_trip') %>% 
  inner_join(gbm7.2, by='driver_trip') %>% 
  inner_join(deep, by='driver_trip') %>% 
  mutate(gbm.prob = .2*prob.stack7.1 + .2*prob.stack7.2 + .2*prob.gbm2.1 + .2*prob.gbm2.2 + .1*prob.gbm7.1 + .1*prob.gbm7.2) %>%
  mutate(prob = .05*prob.deep + .95*gbm.prob) %>%
  select(driver_trip,prob)

file <- paste0("axa-gbm2-7-8-merge1-deep", ".csv.gz")
write.csv(gbm.merge.1.deep,gzfile(file),row.names=FALSE)

