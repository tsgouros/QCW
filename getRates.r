
## First get a table of account and meters with rate codes.
rateCodes <- read.csv("data/rateData/BIF005 Rate Report.csv") %>%
    mutate(account=Account,
           meter=Meter,
           rateCode=RateCode,
           dateMeterInstalled=mdy(DateMeterInstalled),
           dateMeterRemoved=mdy(DateMeterRemoved)) %>%
    select(-Account,-Meter,-DateMeterInstalled,-DateMeterRemoved,-RateCode) %>%
    as_tibble()

rates <- read_excel("data/rateData/rate-fee-and-tiers.xlsx") %>%
    mutate(baseFee=ifelse(is.na(baseFee),lag(baseFee),baseFee))

applyWaterRate <- function(usage, baseFee, t1, t2, t3, t4) {
    return(baseFee + ifelse(usage > 20000, (usage - 20000)*t4,0) +
        ifelse(usage > 10000, ifelse((usage - 10000) < 10000,
          (usage - 10000) * t3, 10000 * t3), 0) + 
        ifelse(usage > 1000, ifelse((usage - 1000) < 9000,
          (usage - 1000) * t2, 9000 * t2), 0) +
        ifelse(usage > 1000, 1000 * t1, usage * t1))
}   
## Use like this:
##
## rev <- water.resid %>%
##     left_join(rateCodes ,by=c("account","meter"),
##               relationship="many-to-many") %>%
##     left_join(rates,by="rateCode") %>%
##     mutate(billAmount=applyWaterRate(consumption,baseFee,t1,t2,t3,t4),
##            predBillAmount=applyWaterRate(predUsage,baseFee,t1,t2,t3,t4))
##   



