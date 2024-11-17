#HW Load Intake
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
    separate(READINGDATE, sep=" ", into=c("READINGDATE","time","M"),
             fill="right") %>%
    select(-time,-M) %>%
    separate(PREVIOUSREADINGDATE, sep=" ",
             into=c("PREVIOUSREADINGDATE","time","M"), fill="right") %>%
    select(-time,-M) %>%
    mutate(READINGDATE=mdy(READINGDATE),
           PREVIOUSREADINGDATE=mdy(PREVIOUSREADINGDATE),
           readingMonth=month(READINGDATE),
           previousReadingMonth=month(PREVIOUSREADINGDATE));

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
    separate(READINGDATE, sep=" ", into=c("READINGDATE","time","M"),
             fill="right") %>%
    select(-time,-M) %>%
    separate(PREVIOUSREADINGDATE, sep=" ",
             into=c("PREVIOUSREADINGDATE","time","M"), fill="right") %>%
    select(-time,-M) %>%
    mutate(READINGDATE=mdy(READINGDATE),
           PREVIOUSREADINGDATE=mdy(PREVIOUSREADINGDATE),
           readingMonth=month(READINGDATE),
           previousReadingMonth=month(PREVIOUSREADINGDATE));

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
    separate(READINGDATE, sep=" ", into=c("READINGDATE","time","M"),
             fill="right") %>%
    select(-time,-M) %>%
    separate(PREVIOUSREADINGDATE, sep=" ",
             into=c("PREVIOUSREADINGDATE","time","M"), fill="right") %>%
    select(-time,-M) %>%
    mutate(READINGDATE=mdy(READINGDATE),
           PREVIOUSREADINGDATE=mdy(PREVIOUSREADINGDATE),
           readingMonth=month(READINGDATE),
           previousReadingMonth=month(PREVIOUSREADINGDATE));

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
    separate(READINGDATE, sep=" ", into=c("READINGDATE","time","M"),
             fill="right") %>%
    select(-time,-M) %>%
    separate(PREVIOUSREADINGDATE, sep=" ",
             into=c("PREVIOUSREADINGDATE","time","M"), fill="right") %>%
    select(-time,-M) %>%
    mutate(READINGDATE=mdy(READINGDATE),
           PREVIOUSREADINGDATE=mdy(PREVIOUSREADINGDATE),
           readingMonth=month(READINGDATE),
           previousReadingMonth=month(PREVIOUSREADINGDATE));

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
    separate(READINGDATE, sep=" ", into=c("READINGDATE","time","M"),
             fill="right") %>%
    select(-time,-M) %>%
    separate(PREVIOUSREADINGDATE, sep=" ",
             into=c("PREVIOUSREADINGDATE","time","M"), fill="right") %>%
    select(-time,-M) %>%
    mutate(READINGDATE=mdy(READINGDATE),
           PREVIOUSREADINGDATE=mdy(PREVIOUSREADINGDATE),
           readingMonth=month(READINGDATE),
           previousReadingMonth=month(PREVIOUSREADINGDATE));

readingTable <- readingTable20 %>%
  rbind(readingTable21 %>%
              select(BILLNUMBER, ACCOUNT, Service,
                     Meter,READTYPE,CURRENTREADING, 
                     READSTATUS, BILLTYPE, Cycle,
                     Book, READINGDATE, SEQUENCENUMBER,
                     PEAKTIME, READINGID, Processed, Supplier,
                     Customer, PREVIOUSREADING, Notes, Consumption,
                     MULTIPLIER1, MULTIPLIER2, UNITS, MV90CHANNEL,
                     LOSSFACTOR,SERVICEID,PREVIOUSREADINGDATE,
                     WORKERCODE, Interval, BILLINGINTERVAL, SERVICEORDER,
                     METERTROUBLECODE1, METERTROUBLECODE2, Validated, Days,
                     DEMANDFACTOR, CANCELED, SERVICEMULTIPLIER1,
                     SERVICEMULTIPLIER2, CONSUMPTIONDISCOUNT, DISCOUNTCODE,
                     Allocation, Factor, OVERRIDECONSUMPTION, USEOVERRIDE,
                     USEOVERRIDEREASON, BILLEDCONSUMPTION, ISBILLED, readingMonth, previousReadingMonth));
readingTable <- readingTable20 %>%
  rbind(readingTable21,readingTable22,readingTable23,readingTable24 %>%
          select(BILLNUMBER, ACCOUNT, Service,
                 Meter,READTYPE,CURRENTREADING, 
                 READSTATUS, BILLTYPE, Cycle,
                 Book, READINGDATE, SEQUENCENUMBER,
                 PEAKTIME, READINGID, Processed, Supplier,
                 Customer, PREVIOUSREADING, Notes, Consumption,
                 MULTIPLIER1, MULTIPLIER2, UNITS, MV90CHANNEL,
                 LOSSFACTOR,SERVICEID,PREVIOUSREADINGDATE,
                 WORKERCODE, Interval, BILLINGINTERVAL, SERVICEORDER,
                 METERTROUBLECODE1, METERTROUBLECODE2, Validated, Days,
                 DEMANDFACTOR, CANCELED, SERVICEMULTIPLIER1,
                 SERVICEMULTIPLIER2, CONSUMPTIONDISCOUNT, DISCOUNTCODE,
                 Allocation, Factor, OVERRIDECONSUMPTION, USEOVERRIDE,
                 USEOVERRIDEREASON, BILLEDCONSUMPTION, ISBILLED, readingMonth, previousReadingMonth));           

cat("done with readingtable\n")

addressTable <- read.csv("data/Updated BIF002 Report.csv",
                      col.names=c("streetNum","streetName","unit",
                                  "account","streetPrefix",
                                  "streetSuffix","serviceAddress",
                                  "address1","address2","Town","State",
                                  "Zip","Lot","Parcel","oldAccount",
                                  "Latitude","Longitude","Zone",
                                  "streetPostDir","taxProfile",
                                  "routeComment","subdivision",
                                  "waterZone","trashZone","extRef",
                                  "collectedBy")) %>% as_tibble();

cat("done with addressTable\n");

customerTable <- read.csv("data/Updated BIF003 Report.csv",
                      col.names=c("customer", "cycle", "book", "account", 
                                  "accountType", "accountStatus", "owner", 
                                  "customerIn", "moveOutDate", 
                                  "lastDepositDate    ", "finalBill    ", 
                                  "penaltyExempt", "noticeExempt", "outInfo", 
                                  "dateOutInfo", "inInfo", "userInInfo", 
                                  "dateInInfo", "moveInInfoFrom", "oldRefNum", 
                                  "customerID", "customerIn2", "paymentTerms", 
                                  "creditLimit", "alwaysNoBill", 
                                  "arTransferCustomerID", "depositXfer", 
                                  "moveInDate", "autoMoveIn", "collectionStatus", 
                                  "vacantAccount", "noReturnEnvelope", "billPrintGroup", 
                                  "masterAccount", "revenueClass", "paymentProfile",
                                  "trashTempDiscont", "sendENotices")) %>%
  as_tibble() %>%
  mutate(moveInDate=mdy(moveInDate), moveInMonth=month(moveInDate),
         moveOutDate=mdy(moveOutDate), moveOutMonth=month(moveOutDate));

cat("done with addressTable\n");

billTable19 <- read.csv("data/2019 BIF951 Report.csv",
                        col.names=c("billNumber","billDate","accountType    ",
                                    "needsPayment","batchID","bif951pk",
                                    "billPrint","billGroup","billStat","billType",
                                    "chgBill","cycle","book","cancel","account",
                                    "customer","customerAcctID","dueDate",
                                    "paymentProfile","processed","serAdd",
                                    "balanceForward","previousBilling",
                                    "sinceLastBill","currentTrans","cancelledBilling",
                                    "amountDue","addressType","address1",
                                    "address2","address3","address4","address5","address6")) %>%
  as_tibble() %>%
  mutate(billDate=dmy(billDate),
         billMonth=month(billDate));

billTable20 <- read.csv("data/2020 BIF951 Report.csv",
                        col.names=c("billNumber","billDate","accountType    ",
                                    "needsPayment","batchID","bif951pk",
                                    "billPrint","billGroup","billStat","billType",
                                    "chgBill","cycle","book","cancel","account",
                                    "customer","customerAcctID","dueDate",
                                    "paymentProfile","processed","serAdd",
                                    "balanceForward","previousBilling",
                                    "sinceLastBill","currentTrans","cancelledBilling",
                                    "amountDue","addressType","address1",
                                    "address2","address3","address4","address5","address6")) %>%
  as_tibble() %>%
  mutate(billDate=dmy(billDate),
         billMonth=month(billDate));

billTable21 <- read.csv("data/2021 BIF951 Report.csv",
                        col.names=c("billNumber","billDate","accountType    ",
                                    "needsPayment","batchID","bif951pk",
                                    "billPrint","billGroup","billStat","billType",
                                    "chgBill","cycle","book","cancel","account",
                                    "customer","customerAcctID","dueDate",
                                    "paymentProfile","processed","serAdd",
                                    "balanceForward","previousBilling",
                                    "sinceLastBill","currentTrans","cancelledBilling",
                                    "amountDue","addressType","address1",
                                    "address2","address3","address4","address5","address6")) %>%
  as_tibble() %>%
  mutate(billDate=dmy(billDate),
         billMonth=month(billDate));

billTable22 <- read.csv("data/2022 BIF951 Report.csv",
                        col.names=c("billNumber","billDate","accountType    ",
                                    "needsPayment","batchID","bif951pk",
                                    "billPrint","billGroup","billStat","billType",
                                    "chgBill","cycle","book","cancel","account",
                                    "customer","customerAcctID","dueDate",
                                    "paymentProfile","processed","serAdd",
                                    "balanceForward","previousBilling",
                                    "sinceLastBill","currentTrans","cancelledBilling",
                                    "amountDue","addressType","address1",
                                    "address2","address3","address4","address5","address6")) %>%
  as_tibble() %>%
  mutate(billDate=dmy(billDate),
         billMonth=month(billDate));

billTable23 <- read.csv("data/2023 BIF951 Report.csv",
                        col.names=c("billNumber","billDate","accountType    ",
                                    "needsPayment","batchID","bif951pk",
                                    "billPrint","billGroup","billStat","billType",
                                    "chgBill","cycle","book","cancel","account",
                                    "customer","customerAcctID","dueDate",
                                    "paymentProfile","processed","serAdd",
                                    "balanceForward","previousBilling",
                                    "sinceLastBill","currentTrans","cancelledBilling",
                                    "amountDue","addressType","address1",
                                    "address2","address3","address4","address5","address6")) %>%
  as_tibble() %>%
  mutate(billDate=dmy(billDate),
         billMonth=month(billDate));

billTable24 <- read.csv("data/2024 Jan to Sep BIF951 report.csv",
                        col.names=c("billNumber","billDate","accountType    ",
                                    "needsPayment","batchID","bif951pk",
                                    "billPrint","billGroup","billStat","billType",
                                    "chgBill","cycle","book","cancel","account",
                                    "customer","customerAcctID","dueDate",
                                    "paymentProfile","processed","serAdd",
                                    "balanceForward","previousBilling",
                                    "sinceLastBill","currentTrans","cancelledBilling",
                                    "amountDue","addressType","address1",
                                    "address2","address3","address4","address5","address6")) %>%
  as_tibble() %>%
  mutate(billDate=dmy(billDate),
         billMonth=month(billDate));

cat("done with billTable\n");
