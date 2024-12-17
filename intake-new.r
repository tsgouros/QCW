# November 21 Updates. Fixed billTable date. Reformat to sentence case.
library(lubridate)
library(tidyverse)


readingTable19 <- read.csv("data/2019 BIF016 report.csv",
                           col.names=c("billNumber", "account", "service",
                                       "meter","readType","currentReading",
                                       "readStatus", "billTyple", "cycle",
                                       "book", "readingDate", "sequenceNumber",
                                       "peakTime", "readingID", "processed", "supplier",
                                       "customer", "previousReading", "notes", "consumption",
                                       "multiplier1", "multiplier2", "units", "mv90Channel",
                                       "lossFactor","serviceID","previousReadingDate",
                                       "workerCode", "interval", "billingInterval", "serviceOrder",
                                       "meterTroubleCode1", "meterTroubleCode2", "validated", "days",
                                       "demandFactor", "canceled", "serviceMultiplier1",
                                       "serviceMultiplier2", "consumptionDiscount", "discountCode",
                                       "allocation", "factor", "overrideconsumption", "useOverride",
                                       "useOverrideReason", "billedConsumption", "isbilled")) %>%
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
         previousReadingMonth=month(previousReadingDate)) %>%
  filter(canceled=="No");

readingTable20 <- read.csv("data/2020 BIF016 report.csv",
                           col.names=c("billNumber", "account", "service",
                                       "meter","readType","currentReading",
                                       "readStatus", "billTyple", "cycle",
                                       "book", "readingDate", "sequenceNumber",
                                       "peakTime", "readingID", "processed", "supplier",
                                       "customer", "previousReading", "notes", "consumption",
                                       "multiplier1", "multiplier2", "units", "mv90Channel",
                                       "lossFactor","serviceID","previousReadingDate",
                                       "workerCode", "interval", "billingInterval", "serviceOrder",
                                       "meterTroubleCode1", "meterTroubleCode2", "validated", "days",
                                       "demandFactor", "canceled", "serviceMultiplier1",
                                       "serviceMultiplier2", "consumptionDiscount", "discountCode",
                                       "allocation", "factor", "overrideconsumption", "useOverride",
                                       "useOverrideReason", "billedConsumption", "isbilled")) %>%
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
         previousReadingMonth=month(previousReadingDate)) %>%
  filter(canceled=="No");

readingTable21 <- read.csv("data/2021 BIF016 report.csv",
                           col.names=c("billNumber", "account", "service",
                                       "meter","readType","currentReading",
                                       "readStatus", "billTyple", "cycle",
                                       "book", "readingDate", "sequenceNumber",
                                       "peakTime", "readingID", "processed", "supplier",
                                       "customer", "previousReading", "notes", "consumption",
                                       "multiplier1", "multiplier2", "units", "mv90Channel",
                                       "lossFactor","serviceID","previousReadingDate",
                                       "workerCode", "interval", "billingInterval", "serviceOrder",
                                       "meterTroubleCode1", "meterTroubleCode2", "validated", "days",
                                       "demandFactor", "canceled", "serviceMultiplier1",
                                       "serviceMultiplier2", "consumptionDiscount", "discountCode",
                                       "allocation", "factor", "overrideconsumption", "useOverride",
                                       "useOverrideReason", "billedConsumption", "isbilled")) %>%
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
         previousReadingMonth=month(previousReadingDate)) %>%
  filter(canceled=="No");

readingTable22 <- read.csv("data/2022 BIF016 report.csv",
                           col.names=c("billNumber", "account", "service",
                                       "meter","readType","currentReading",
                                       "readStatus", "billTyple", "cycle",
                                       "book", "readingDate", "sequenceNumber",
                                       "peakTime", "readingID", "processed", "supplier",
                                       "customer", "previousReading", "notes", "consumption",
                                       "multiplier1", "multiplier2", "units", "mv90Channel",
                                       "lossFactor","serviceID","previousReadingDate",
                                       "workerCode", "interval", "billingInterval", "serviceOrder",
                                       "meterTroubleCode1", "meterTroubleCode2", "validated", "days",
                                       "demandFactor", "canceled", "serviceMultiplier1",
                                       "serviceMultiplier2", "consumptionDiscount", "discountCode",
                                       "allocation", "factor", "overrideconsumption", "useOverride",
                                       "useOverrideReason", "billedConsumption", "isbilled")) %>%
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
         previousReadingMonth=month(previousReadingDate)) %>%
  filter(canceled=="No");

readingTable23 <- read.csv("data/2023 BIF016 report.csv",
                           col.names=c("billNumber", "account", "service",
                                       "meter","readType","currentReading",
                                       "readStatus", "billTyple", "cycle",
                                       "book", "readingDate", "sequenceNumber",
                                       "peakTime", "readingID", "processed", "supplier",
                                       "customer", "previousReading", "notes", "consumption",
                                       "multiplier1", "multiplier2", "units", "mv90Channel",
                                       "lossFactor","serviceID","previousReadingDate",
                                       "workerCode", "interval", "billingInterval", "serviceOrder",
                                       "meterTroubleCode1", "meterTroubleCode2", "validated", "days",
                                       "demandFactor", "canceled", "serviceMultiplier1",
                                       "serviceMultiplier2", "consumptionDiscount", "discountCode",
                                       "allocation", "factor", "overrideconsumption", "useOverride",
                                       "useOverrideReason", "billedConsumption", "isbilled")) %>%
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
         previousReadingMonth=month(previousReadingDate)) %>%
  filter(canceled=="No");

readingTable24 <- read.csv("data/2024 Jan to Sep BIF016 report.csv",
                           col.names=c("billNumber", "account", "service",
                                       "meter","readType","currentReading",
                                       "readStatus", "billTyple", "cycle",
                                       "book", "readingDate", "sequenceNumber",
                                       "peakTime", "readingID", "processed", "supplier",
                                       "customer", "previousReading", "notes", "consumption",
                                       "multiplier1", "multiplier2", "units", "mv90Channel",
                                       "lossFactor","serviceID","previousReadingDate",
                                       "workerCode", "interval", "billingInterval", "serviceOrder",
                                       "meterTroubleCode1", "meterTroubleCode2", "validated", "days",
                                       "demandFactor", "canceled", "serviceMultiplier1",
                                       "serviceMultiplier2", "consumptionDiscount", "discountCode",
                                       "allocation", "factor", "overrideconsumption", "useOverride",
                                       "useOverrideReason", "billedConsumption", "isbilled")) %>%
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
         previousReadingMonth=month(previousReadingDate)) %>%
  filter(canceled=="No");


readingTable <- rbind(readingTable19,
                      readingTable20,
                      readingTable21,
                      readingTable22,
                      readingTable23,
                      readingTable24)

cat("done with readingtable\n")

addressTable <- read.csv("data/Updated BIF002 Report.csv",
                         col.names=c("streetNum","streetName","unit",
                                     "account","streetPrefix",
                                     "streetSuffix","serviceAddress",
                                     "address1","address2","town","state",
                                     "zip","Lot","parcel","oldAccount",
                                     "latitude","longitude","zone",
                                     "streetPostDir","taxProfile",
                                     "routeComment","subdivision",
                                     "waterZone","trashZone","extRef",
                                     "collectedBy")) %>% as_tibble();

cat("done with addressTable\n");

customerTable <- read.csv("data/Updated BIF003 Report.csv",
                          col.names=c("customer", "cycle", "book", "account",
                                      "accountType", "accountStatus", "owner",
                                      "customerIn", "moveOutDate",
                                      "lastDepositDate", "finalBill",
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

cat("done with customerTable\n");

billTable19 <- read.csv("data/2019 BIF951 Report.csv",
                        col.names=c("billNumber","billDate","accountType",
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
  mutate(billDate=mdy(billDate),
         billMonth=month(billDate));

billTable20 <- read.csv("data/2020 BIF951 Report.csv",
                        col.names=c("billNumber","billDate","accountType",
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
  mutate(billDate=mdy(billDate),
         billMonth=month(billDate));

billTable21 <- read.csv("data/2021 BIF951 Report.csv",
                        col.names=c("billNumber","billDate","accountType",
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
  mutate(billDate=mdy(billDate),
         billMonth=month(billDate));

billTable22 <- read.csv("data/2022 BIF951 Report.csv",
                        col.names=c("billNumber","billDate","accountType",
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
  mutate(billDate=mdy(billDate),
         billMonth=month(billDate));

billTable23 <- read.csv("data/2023 BIF951 Report.csv",
                        col.names=c("billNumber","billDate","accountType",
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
  mutate(billDate=mdy(billDate),
         billMonth=month(billDate));

billTable24 <- read.csv("data/2024 Jan to Sep BIF951 report.csv",
                        col.names=c("billNumber","billDate","accountType",
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
  mutate(billDate=mdy(billDate),
         billMonth=month(billDate));

billTable <- rbind(billTable19,
                   billTable20,
                   billTable21,
                   billTable22,
                   billTable23,
                   billTable24)

cat("done with billTable\n");
