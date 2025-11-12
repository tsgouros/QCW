library(tidyverse)
library(readxl)

## First get a table of account and meters with rate codes.
rateCodes <- read.csv("data/rateData/BIF005 Rate Report.csv",
    col.names=c("account","meter","dateMeterInstalled","dateMeterRemoved","rateCode","meterBoxID")) %>%
    as_tibble() %>%
    mutate(dateMeterInstalled=mdy(dateMeterInstalled),
           dateMeterRemoved=mdy(dateMeterRemoved),
           meterID=paste0(account,"-",meter));

## Read rates from data files. We keep the rates in a single data
## table, adding a "validUntilDate" to each row, according to which
## file it came from. As more rates are approved, they can be added to
## this rbind command.
rates <- rbind(
    read_excel("data/rateData/rate-fee-and-tiers-superseded-2025-08-27.xlsx",
               range="A1:H62") %>%
    mutate(rateSchedule=1,validUntilDate=ymd("2025-08-26")),
    read_excel("data/rateData/rate-fee-and-tiers-updated-2025-09-01.xlsx",
               range="A1:H62") %>%
    mutate(rateSchedule=2,validUntilDate=ymd("2100-01-01")))

applyWaterRate <- function(usage, baseFee, t1, t2, t3, t4) {
    return(baseFee + ifelse(usage > 20000, (usage - 20000)*t4,0) +
        ifelse(usage > 10000, ifelse((usage - 10000) < 10000,
          (usage - 10000) * t3, 10000 * t3), 0) + 
        ifelse(usage > 1000, ifelse((usage - 1000) < 9000,
          (usage - 1000) * t2, 9000 * t2), 0) +
        ifelse(usage > 1000, 1000 * t1, usage * t1))
}   

## Try something like this. Appears to be called a "rolling" join.
##
## rev0625 <- projectConsumption(waterMeans, 2025, 6) %>%
##     mutate(readingDate=ymd("2025-06-27")) %>%
##     left_join(rates, join_by(rateCode,
##                              closest(readingDate <= validUntilDate))) %>%
##     mutate(predBillAmount=applyWaterRate(predUsage,baseFee,t1,t2,t3,t4))
##



