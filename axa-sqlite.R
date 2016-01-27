rm(list=ls(all=TRUE))
options(scipen = 10)

library(dplyr)
library(data.table)
library(parallel)

sessionInfo()
detectCores()

load('drivers.RData')

drivers <- list.files("./drivers/")
drivers <- drivers[1:24]
#drivers <- c(1,2,1701,3000)
n.drivers <- length(drivers)

#divvy up drivers by number of cores
d.per <- round(n.drivers/n)
d.list <- split(drivers, ceiling(seq_along(drivers)/d.per))

driver_db <- src_sqlite(path = "./sqlfile.db", create = TRUE)
summary_df <- tbl_df(driver.summary)

driver_sqlite <- copy_to(driver_db, summary_df, temporary = FALSE, indexes=list('driver'),
                         type=rep("REAL", dim(summary_df)[[2]]))

driver.summary <- NULL
n <- detectCores()
if (file.exists('cluster.txt'))
  file.remove('cluster.txt')
cl <- makePSOCKcluster(n, outfile='cluster.txt')

clusterExport(cl, c('d.list' ,'driver_sqlite'))
clusterEvalQ(cl,library(dplyr))
clusterEvalQ(cl,library(data.table))

x <- function(x){
  drivers <- d.list[[x]]
  for(d in drivers) {
    print(d)
    print(match(d, drivers))
    curDriver <- driver_sqlite %>%
      filter(driver==d) %>%
      mutate(target = 1)
    
    nonDrivers <- driver_sqlite %>%
      filter(driver != d)
    
    nonDrivers <- as.data.frame(nonDrivers) %>%
      sample_n(1000) %>%
      mutate(target = 0)
    
    model.data <- rbindlist(list(curDriver, nonDrivers), use.names=TRUE) %>%
      select(-driver, -trip)
  }
  
}

start <- Sys.time()

driver.cl <- clusterApply(cl, 1:n, x)

stopCluster(cl)

message('time to run:')
print(Sys.time()-start)

driver.summary <- rbindlist(driver.cl, use.names=FALSE)
