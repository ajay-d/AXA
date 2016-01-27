rm(list=ls(all=TRUE))
options(scipen = 10)

library(dplyr)
library(ggplot2)
library(data.table)
library(parallel)
library(gbm)

detectCores()

setwd('/users/adeonari/downloads/AXA')

gbm.merge <- read.csv('axa-gbm2-25k-merge.csv.gz') %>%
  rename(gbm.prob = prob)
deep <- read.csv('axa-deep-act=TanhWithDropout.csv.gz') %>%
  rename(deep.prob = prob)

gbm.merge <- gbm.merge %>%
  left_join(deep, by='driver_trip') %>%
  mutate(prob = .05*deep.prob + .95*gbm.prob) %>%
  select(driver_trip,prob)


ggplot(gbm.merge) +
  geom_histogram(aes(prob), binwidth=.01)

file <- paste0("axa-bestmerge-deep-.05", ".csv.gz")

write.csv(gbm.merge,gzfile(file),row.names=FALSE)
