library(lubridate)
library(tidyverse)

readingTable20 <- read.csv("data/2020 BIF016 report.csv",
                         col.names=c("BILLNUMBER", "ACCOUNT", "Service",
                                     "Meter","READTYPE","CURRENTREADING", 
                                     "READSTATUS", "BILLTYPE", "Cycle",
                                     "Book", "READINGDATE", "SEQUENCENUMBER",
                                     "PEAKTIME", "READINGID", "Processed", "Supplier",
                                     "Customer", "PREVIOUSREADING", "Notes", "Consumption",
                                     "MULTIPLIER1", "MULTIPLIER2", "UNITS", "MV90CHANNEL",
                                     "LOSSFACTOR","SERVICEID","PREVIOUSREADINGDATE",
                                     "WORKERCODE", "Interval", "BILLINGINTERVAL", "SERVICEORDER",
                                     "METERTROUBLECODE1", "METERTROUBLECODE2", "Validated", "Days",
                                     "DEMANDFACTOR", "CANCELED", "SERVICEMULTIPLIER1",
                                     "SERVICEMULTIPLIER2", "CONSUMPTIONDISCOUNT", "DISCOUNTCODE",
                                     "Allocation", "Factor", "OVERRIDECONSUMPTION", "USEOVERRIDE",
                                     "USEOVERRIDEREASON", "BILLEDCONSUMPTION", "ISBILLED")) %>%
    as_tibble() %>%
    separate(readingDate, sep=" ", into=c("readingDate","time","M"),
             fill="right") %>%
    select(-time,-M) %>%
    separate(previousReadingDate, sep=" ",
             into=c("previousReadingDate","time","M"), fill="right") %>%
    select(-time,-M) %>%
    mutate(readingDate=mdy(readingDate),
           previousReadingDate=mdy(previousReadingDate),
           readingMonth=month(readingDate),
           previousReadingMonth=month(previousReadingDate));

readingTable21 <- read.csv("data/2021 BIF016 report.csv",
                         col.names=c("BILLNUMBER", "ACCOUNT", "Service",
                                     "Meter","READTYPE","CURRENTREADING", 
                                     "READSTATUS", "BILLTYPE", "Cycle",
                                     "Book", "READINGDATE", "SEQUENCENUMBER",
                                     "PEAKTIME", "READINGID", "Processed", "Supplier",
                                     "Customer", "PREVIOUSREADING", "Notes", "Consumption",
                                     "MULTIPLIER1", "MULTIPLIER2", "UNITS", "MV90CHANNEL",
                                     "LOSSFACTOR","SERVICEID","PREVIOUSREADINGDATE",
                                     "WORKERCODE", "Interval", "BILLINGINTERVAL", "SERVICEORDER",
                                     "METERTROUBLECODE1", "METERTROUBLECODE2", "Validated", "Days",
                                     "DEMANDFACTOR", "CANCELED", "SERVICEMULTIPLIER1",
                                     "SERVICEMULTIPLIER2", "CONSUMPTIONDISCOUNT", "DISCOUNTCODE",
                                     "Allocation", "Factor", "OVERRIDECONSUMPTION", "USEOVERRIDE",
                                     "USEOVERRIDEREASON", "BILLEDCONSUMPTION", "ISBILLED")) %>%
    as_tibble() %>%
    separate(readingDate, sep=" ", into=c("readingDate","time","M"),
             fill="right") %>%
    select(-time,-M) %>%
    separate(previousReadingDate, sep=" ",
             into=c("previousReadingDate","time","M"), fill="right") %>%
    select(-time,-M) %>%
    mutate(readingDate=mdy(readingDate),
           previousReadingDate=mdy(previousReadingDate),
           readingMonth=month(readingDate),
           previousReadingMonth=month(previousReadingDate));

readingTable22 <- read.csv("data/2022 BIF016 report.csv",
                         col.names=c("BILLNUMBER", "ACCOUNT", "Service",
                                     "Meter","READTYPE","CURRENTREADING", 
                                     "READSTATUS", "BILLTYPE", "Cycle",
                                     "Book", "READINGDATE", "SEQUENCENUMBER",
                                     "PEAKTIME", "READINGID", "Processed", "Supplier",
                                     "Customer", "PREVIOUSREADING", "Notes", "Consumption",
                                     "MULTIPLIER1", "MULTIPLIER2", "UNITS", "MV90CHANNEL",
                                     "LOSSFACTOR","SERVICEID","PREVIOUSREADINGDATE",
                                     "WORKERCODE", "Interval", "BILLINGINTERVAL", "SERVICEORDER",
                                     "METERTROUBLECODE1", "METERTROUBLECODE2", "Validated", "Days",
                                     "DEMANDFACTOR", "CANCELED", "SERVICEMULTIPLIER1",
                                     "SERVICEMULTIPLIER2", "CONSUMPTIONDISCOUNT", "DISCOUNTCODE",
                                     "Allocation", "Factor", "OVERRIDECONSUMPTION", "USEOVERRIDE",
                                     "USEOVERRIDEREASON", "BILLEDCONSUMPTION", "ISBILLED")) %>%
    as_tibble() %>%
    separate(readingDate, sep=" ", into=c("readingDate","time","M"),
             fill="right") %>%
    select(-time,-M) %>%
    separate(previousReadingDate, sep=" ",
             into=c("previousReadingDate","time","M"), fill="right") %>%
    select(-time,-M) %>%
    mutate(readingDate=mdy(readingDate),
           previousReadingDate=mdy(previousReadingDate),
           readingMonth=month(readingDate),
           previousReadingMonth=month(previousReadingDate));

readingTable23 <- read.csv("data/2023 BIF016 report.csv",
                         col.names=c("BILLNUMBER", "ACCOUNT", "Service",
                                     "Meter","READTYPE","CURRENTREADING", 
                                     "READSTATUS", "BILLTYPE", "Cycle",
                                     "Book", "READINGDATE", "SEQUENCENUMBER",
                                     "PEAKTIME", "READINGID", "Processed", "Supplier",
                                     "Customer", "PREVIOUSREADING", "Notes", "Consumption",
                                     "MULTIPLIER1", "MULTIPLIER2", "UNITS", "MV90CHANNEL",
                                     "LOSSFACTOR","SERVICEID","PREVIOUSREADINGDATE",
                                     "WORKERCODE", "Interval", "BILLINGINTERVAL", "SERVICEORDER",
                                     "METERTROUBLECODE1", "METERTROUBLECODE2", "Validated", "Days",
                                     "DEMANDFACTOR", "CANCELED", "SERVICEMULTIPLIER1",
                                     "SERVICEMULTIPLIER2", "CONSUMPTIONDISCOUNT", "DISCOUNTCODE",
                                     "Allocation", "Factor", "OVERRIDECONSUMPTION", "USEOVERRIDE",
                                     "USEOVERRIDEREASON", "BILLEDCONSUMPTION", "ISBILLED")) %>%
    as_tibble() %>%
    separate(readingDate, sep=" ", into=c("readingDate","time","M"),
             fill="right") %>%
    select(-time,-M) %>%
    separate(previousReadingDate, sep=" ",
             into=c("previousReadingDate","time","M"), fill="right") %>%
    select(-time,-M) %>%
    mutate(readingDate=mdy(readingDate),
           previousReadingDate=mdy(previousReadingDate),
           readingMonth=month(readingDate),
           previousReadingMonth=month(previousReadingDate));

readingTable24 <- read.csv("data/2024 Jan to Sep BIF016 report.csv",
                         col.names=c("BILLNUMBER", "ACCOUNT", "Service",
                                     "Meter","READTYPE","CURRENTREADING", 
                                     "READSTATUS", "BILLTYPE", "Cycle",
                                     "Book", "READINGDATE", "SEQUENCENUMBER",
                                     "PEAKTIME", "READINGID", "Processed", "Supplier",
                                     "Customer", "PREVIOUSREADING", "Notes", "Consumption",
                                     "MULTIPLIER1", "MULTIPLIER2", "UNITS", "MV90CHANNEL",
                                     "LOSSFACTOR","SERVICEID","PREVIOUSREADINGDATE",
                                     "WORKERCODE", "Interval", "BILLINGINTERVAL", "SERVICEORDER",
                                     "METERTROUBLECODE1", "METERTROUBLECODE2", "Validated", "Days",
                                     "DEMANDFACTOR", "CANCELED", "SERVICEMULTIPLIER1",
                                     "SERVICEMULTIPLIER2", "CONSUMPTIONDISCOUNT", "DISCOUNTCODE",
                                     "Allocation", "Factor", "OVERRIDECONSUMPTION", "USEOVERRIDE",
                                     "USEOVERRIDEREASON", "BILLEDCONSUMPTION", "ISBILLED")) %>%
    as_tibble() %>%
    separate(readingDate, sep=" ", into=c("readingDate","time","M"),
             fill="right") %>%
    select(-time,-M) %>%
    separate(previousReadingDate, sep=" ",
             into=c("previousReadingDate","time","M"), fill="right") %>%
    select(-time,-M) %>%
    mutate(readingDate=mdy(readingDate),
           previousReadingDate=mdy(previousReadingDate),
           readingMonth=month(readingDate),
           previousReadingMonth=month(previousReadingDate));
