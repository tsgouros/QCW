## A function to accept a month and year and create a data frame of
## read dates.  Using 'ifelse' instead of simple 'if' statements makes
## this harder to read, but allows it to be used in mutate()
## functions.
makeReadDateSimple <- function(dyear, dmonth, cycle) {
    if (cycle == 2) {
        dmonth <- dmonth - 1;
        if (dmonth == 0) {
            dday <- 25;
            dmonth <- 12;
            dyear <- dyear -1;
        }
    } else if (cycle == 1) {
        dday <- 19;
    } else if (cycle == 3) {
        dday <- 5;
    } else if (cycle == 4) {
        dday <- 12;
    } else if (cycle == 5) {
        dday <- 19;
    } else if (cycle == 6) {
        dday <- 19;
    } else {
        dday <- 19;
    }

    return(as_date(sprintf("%d-%0.2d-%0.2d", dyear, dmonth, dday)));
}

    
## This is the same function as above, but using the ifelse command
## instead of if and else, in order to be easily parallelized.
makeReadDate <- function(dyear, dmonth, cycle) {
    return(as_date(sprintf("%d-%0.2d-%0.2d",
                           ifelse(dmonth==1 & cycle==2, dyear - 1, dyear),
                           ifelse(cycle==2,ifelse(dmonth==1, 12, dmonth - 1),
                                  dmonth),
                           ifelse(cycle==1, 19,
                           ifelse(cycle==2, 25,
                           ifelse(cycle==3,  5,
                           ifelse(cycle==4, 12,
                           ifelse(cycle==5, 19,
                           ifelse(cycle==6, 19, 19)))))))));
}
## This is basically the same function as above, but since the target
## format is a floating point number, we skip the as_date(), for speed.
makeReadDateDirect <- function(dyear, dmonth, cycle) {

    return(((ifelse(dmonth==1 & cycle==2, dyear - 1, dyear) - 2019) * 12) +
           (ifelse(cycle==2,ifelse(dmonth==1, 12, dmonth - 1), dmonth) - 6) +
           ((ifelse(cycle==1, 19,
             ifelse(cycle==2, 25,
             ifelse(cycle==3,  5,
             ifelse(cycle==4, 12,
             ifelse(cycle==5, 19,
             ifelse(cycle==6, 19, 19))))))) - 1)/
        (ifelse(ifelse(cycle==2,ifelse(dmonth==1, 12, dmonth - 1), dmonth) %in% c(1,3,5,7,8,10,12), 31,
            ifelse(ifelse(cycle==2,ifelse(dmonth==1, 12, dmonth - 1), dmonth) == 2, 28, 30))));
}

## Project the usage to be billed in a particular month and year.
projectConsumption <- function(wmeans, dyear, dmonth) {

    ## First project the readings using the dates that will be billed
    ## for the given month.
    return(wmeans %>%
           mutate(assumedReadingDate=makeReadDateDirect(dyear, dmonth, cycle),
                  cdate = assumedReadingDate * pi/6,
                  predUsage=amp * (cos(cdate + tst) + 1) + off + (slp * cdate)))
        
}


rev0625 <- projectConsumption(waterMeans, 2025, 6) %>%
    mutate(readingDate=ymd("2025-06-27")) %>%
    left_join(rates, join_by(rateCode,
                             closest(readingDate <= validUntilDate))) %>%
    mutate(predBillAmount=applyWaterRate(predUsage,baseFee,t1,t2,t3,t4))

rev0725 <- projectConsumption(waterMeans, 2025, 7) %>%
    mutate(readingDate=ymd("2025-07-27")) %>%
    left_join(rates, join_by(rateCode,
                             closest(readingDate <= validUntilDate))) %>%
    mutate(predBillAmount=applyWaterRate(predUsage,baseFee,t1,t2,t3,t4))


##waterResid %>% filter(readingDate > mdy("4/30/2025")) %>% mutate(readingDate = readingDate %m+% months(1), billDate = billDate %m+% months(1))
