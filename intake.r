library(lubridate)
library(tidyverse)


billTable23 <- read.csv("data/2023 Bill Table BIF951.csv",
                      col.names=c("billNumber","billDate","accountType",
                                  "needsPayment","batchID","bif951pk",
                                  "billPrint","billGroup","billStat","billType",
                                  "chgBill","cycle","book","cancel","account",
                                  "customer","customerAcctID","dueDate",
                                  "paymentProfile","processed","serAdd",
                                  "balanceForward","previousBilling",
                                  "sinceLastBill","currentTrans",
                                  "cancelledBilling","amountDue","addressType",
                                  "address1","address2","address3","address4",
                                  "address5","address6")) %>%
    as_tibble() %>%
    mutate(billDate=dmy(billDate),
           billMonth=month(billDate));

## Lots of accountTypes are missing from the January and February 2023
## records, so let's repopulate the accountType field.
billTable23 <- billTable23 %>%
    select(-accountType) %>%
    left_join(billTable23 %>%
              group_by(account) %>%
              dplyr::summarize(accountType=last(accountType)),by="account") %>%
    select(billNumber,billDate,accountType,
           needsPayment,batchID,bif951pk,
           billPrint,billGroup,billStat,billType,
           chgBill,cycle,book,cancel,account,
           customer,customerAcctID,dueDate,
           paymentProfile,processed,serAdd,
           balanceForward,previousBilling,
           sinceLastBill,currentTrans,
           cancelledBilling,amountDue,addressType,
           address1,address2,address3,address4,
           address5,address6,billMonth)

## This seems to be a good format for the bill tables. The specific file
## here is an aggregate of 1/2024 to 6/2024, but so long as the columns
## are consistent, a file with only July in it is easy to append.
billTable24 <- read.csv("data/2024 Bill Table BIF951.csv",
                      col.names=c("billNumber","billDate","accountType",
                                  "needsPayment","batchID","bif951pk",
                                  "billPrint","billGroup","billStat","billType",
                                  "chgBill","cycle","book","cancel","account",
                                  "customer","customerAcctID","dueDate",
                                  "paymentProfile","processed","serAdd",
                                  "balanceForward","previousBilling",
                                  "sinceLastBill","currentTrans",
                                  "cancelledBilling","amountDue",
                                  "aexcept","addressType",
                                  "address1","address2","address3","address4",
                                  "address5","address6")) %>%
    as_tibble() %>%
    mutate(billDate=dmy(billDate),
           billMonth=month(billDate));

billTable <- rbind(billTable23,billTable24 %>% select(-aexcept));

cat("done with billTable\n");

readingTable23 <- read.csv("data/2023 Reading Table BIF016.csv",
                         col.names=c("select","billNumber", "account",
                                     "service","meter", "readType",
                                     "currentReading", "readStatus",
                                     "billType","cycle", "book","readingDate",
                                     "sequenceNumber","peakTime", "readingID",
                                     "processed", "supplier","customer",
                                     "previousReading","notes", "consumption",
                                     "multiplier1", "multiplier2","units",
                                     "mv90channel","lossFactor", "serviceID",
                                     "previousReadingDate", "workerCode",
                                     "interval", "billingInterval",
                                     "serviceOrder", "meterTroubleCode1",
                                     "meterTroubleCode2", "validated", "days",
                                     "demandFactor", "canceled",
                                     "serviceMultiplier1", "serviceMultiplier2",
                                     "consumptionDiscount", "discountCode",
                                     "allocation", "factor",
                                     "overrideConsumption", "useOverride",
                                     "useOverrideReason", "billedConsumption",
                                     "isBilled", "rawUsage")) %>%
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

readingTable24 <- read.csv("data/2024 Reading Table BIF016.csv",
                         col.names=c("select","billNumber", "meter",
                                     "consumption","currentReading", 
                                     "previousReading","account",
                                     "readingDate","readStatus",
                                     "rawUsage","service","readType",     
                                     "billType","cycle", "book",
                                     "sequenceNumber","peakTime", "readingID",
                                     "processed", "supplier","customer",
                                     "notes", 
                                     "multiplier1", "multiplier2","units",
                                     "mv90channel","lossFactor", "serviceID",
                                     "previousReadingDate", "workerCode",
                                     "interval", "billingInterval",
                                     "serviceOrder",
                                     "meterTroubleCode1",
                                     "meterTroubleCode2", "validated", "days",
                                     "demandFactor", "canceled",
                                     "serviceMultiplier1", "serviceMultiplier2",
                                     "consumptionDiscount", "discountCode",
                                     "allocation", "factor",
                                     "overrideConsumption", "useOverride",
                                     "useOverrideReason", "billedConsumption",
                                     "isBilled")) %>%
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

readingTable <- readingTable23 %>%
    rbind(readingTable24 %>%
          select(select, billNumber, account,
                 service, meter, readType,
                 currentReading, readStatus,
                 billType, cycle, book, readingDate,
                 sequenceNumber, peakTime, readingID,
                 processed, supplier, customer,
                 previousReading, notes, consumption,
                 multiplier1, multiplier2, units,
                 mv90channel, lossFactor, serviceID,
                 previousReadingDate, workerCode,
                 interval, billingInterval,
                 serviceOrder, meterTroubleCode1,
                 meterTroubleCode2, validated, days,
                 demandFactor, canceled,
                 serviceMultiplier1, serviceMultiplier2,
                 consumptionDiscount, discountCode,
                 allocation, factor,
                 overrideConsumption, useOverride,
                 useOverrideReason, billedConsumption,
                 isBilled, rawUsage, readingMonth, previousReadingMonth));
cat("done with readingTable\n");

customerTable <- read.csv("data/2024 Customer Account Table BIF003.csv",
                          col.names=c("customer", "cycle", "book", "account",
                                      "accountType", "accountStatus", "owner",
                                      "customerIn", "moveOutDate",
                                      "lastDepositDate", "finalBill",
                                      "penaltyExempt", "noticeExempt",
                                      "outInfo", "dateOutInfo", "inInfo",
                                      "userInInfo", "dateInInfo",
                                      "moveInInfoFrom", "oldRefNum",
                                      "customerID", "customerIn2",
                                      "paymentTerms", "creditLimit",
                                      "alwaysNoBill", "arTransferCustomerID",
                                      "depositXfer", "moveInDate",
                                      "autoMoveIn", "collectionStatus",
                                      "vacantAccount", "noReturnEnvelope",
                                      "billPrintGroup", "masterAccount",
                                      "revenueClass", "paymentProfile",
                                      "trashTempDiscont", "sendENotices")) %>%
    as_tibble() %>%
    mutate(moveInDate=mdy(moveInDate), moveInMonth=month(moveInDate),
           moveOutDate=mdy(moveOutDate), moveOutMonth=month(moveOutDate));

cat("done with customerTable\n");

addressTable <- read.csv("data/2024 Service Address Table BIF002.csv",
                         col.names=c("streetNum","streetName","unit",
                                     "account","streetPrefix",
                                     "streetSuffix","serviceAddress",
                                     "address1","address2","town","state",
                                     "zip","lot","parcel","oldAccount",
                                     "latitude","longitude","zone",
                                     "streetPostDir","taxProfile",
                                     "routeComment","subdivision",
                                     "waterZone","trashZone","extRef",
                                     "collectedBy")) %>% as_tibble();

cat("done with addressTable\n");

if (FALSE) { ## Not currently using these data variables.
b2023.01 <- read.csv("data/January 2023 Bill Transactions BIF955.csv",
                     col.names=c("account","batchID","billNumber",
                                 "transCode","transDate",
                                 "amount","description")) %>%
    as_tibble() %>%
    separate(transDate, sep=" ", into=c("transDate","time","M"),
             fill="right") %>%
    select(-time,-M) %>%
    mutate(transDate=dmy(transDate),
           transMonth=month(transDate));

cat("done with b2023.01\n");

b2023.02 <- read.csv("data/February 2023 Bill Transactions BIF955.csv",
                     col.names=c("account","batchID","billNumber",
                                 "transCode","transDate",
                                 "amount","description")) %>%
    as_tibble() %>%
    separate(transDate, sep=" ", into=c("transDate","time","M"),
             fill="right") %>%
    select(-time,-M) %>%
    mutate(transDate=dmy(transDate),
           transMonth=month(transDate));

cat("done with b2023.02\n");

b2023.03 <- read.csv("data/March 2023 Bill Transactions BIF955.csv",
                     col.names=c("account","batchID","billNumber",
                                 "transCode","transDate",
                                 "amount","description")) %>%
    as_tibble() %>%
    mutate(transDate=dmy(transDate),transMonth=month(transDate));

cat("done with b2023.03\n");

b2023.04 <- read.csv("data/April 2023 Bill Transactions BIF955.csv",
                     col.names=c("account","batchID","billNumber",
                                 "transCode","transDate",
                                 "amount","description")) %>%
    as_tibble() %>%
    mutate(transDate=dmy(transDate),transMonth=month(transDate));

cat("done with b2023.04\n");

b2023.05 <- read.csv("data/May 2023 Bill Transactions BIF955.csv",
                     col.names=c("account","batchID","billNumber",
                                 "transCode","transDate",
                                 "amount","description")) %>%
    as_tibble() %>%
    mutate(transDate=dmy(transDate),transMonth=month(transDate));

cat("done with b2023.05\n");

b2023.06 <- read.csv("data/June 2023 Bill Transactions BIF955.csv",
                     col.names=c("account","batchID","billNumber",
                                 "transCode","transDate",
                                 "amount","description")) %>%
    as_tibble() %>%
    mutate(transDate=dmy(transDate),transMonth=month(transDate));

cat("done with b2023.06\n");

b2023.07 <- read.csv("data/July 2023 Bill Transactions BIF955.csv",
                     col.names=c("account","batchID","billNumber",
                                 "transCode","transDate",
                                 "amount","description")) %>%
    as_tibble() %>%
    mutate(transDate=dmy(transDate),transMonth=month(transDate));

cat("done with b2023.07\n");

b2023.08 <- read.csv("data/August 2023 Bill Transactions BIF955.csv",
                     col.names=c("account","batchID","billNumber",
                                 "transCode","transDate",
                                 "amount","description")) %>%
    as_tibble() %>%
    mutate(transDate=dmy(transDate),transMonth=month(transDate));

cat("done with b2023.08\n");

b2023.09 <- read.csv("data/September 2023 Bill Transactions BIF955.csv",
                     col.names=c("account","batchID","billNumber",
                                 "transCode","transDate",
                                 "amount","description")) %>%
    as_tibble() %>%
    mutate(transDate=dmy(transDate),transMonth=month(transDate));

cat("done with b2023.09\n");

b2023.10 <- read.csv("data/October 2023 Bill Transactions BIF955.csv",
                     col.names=c("account","batchID","billNumber",
                                 "transCode","transDate",
                                 "amount","description")) %>%
    as_tibble() %>%
    mutate(transDate=dmy(transDate),transMonth=month(transDate));

cat("done with b2023.10\n");

b2023.11 <- read.csv("data/November 2023 Bill Transactions BIF955.csv",
                     col.names=c("account","batchID","billNumber",
                                 "transCode","transDate",
                                 "amount","description")) %>%
    as_tibble() %>%
    mutate(transDate=dmy(transDate),transMonth=month(transDate));

cat("done with b2023.11\n");

b2023.12 <- read.csv("data/December 2023 Bill Transactions BIF955.csv",
                     col.names=c("account","batchID","billNumber",
                                 "transCode","transDate",
                                 "amount","description")) %>%
    as_tibble() %>%
    mutate(transDate=dmy(transDate),transMonth=month(transDate));

cat("done with b2023.12\n");


b2023 <- rbind(
    b2023.01 %>%
    filter(transCode %in% c("0002","0003","0042","0043","0044","0047")) %>%
    group_by(account) %>%
    dplyr::summarize(amount=sum(amount),transDate=first(transDate)), 
    b2023.02 %>%
    filter(transCode %in% c("0002","0003","0042","0043","0044","0047")) %>%
    group_by(account) %>%
    dplyr::summarize(amount=sum(amount),transDate=first(transDate)),
    ## The transaction codes changed in March 2023, thank you very much.
    b2023.03 %>%
    filter(transCode %in% c("AD07","AD08","CF01",
                            "LC02","LC03","LC04","LC05","LC06","LC07",
                            "LC14","LC15","LC16",
                            "WA01","WA02","WA03","WA04","WA05","WA06","WA07",
                            "WA15","WA16","WA17","WA19")) %>%
    group_by(account) %>%
    dplyr::summarize(amount=sum(amount),transDate=first(transDate)),
    b2023.04 %>%
    filter(transCode %in% c("AD07","AD08","CF01",
                            "LC02","LC03","LC04","LC05","LC06","LC07",
                            "LC14","LC15","LC16",
                            "WA01","WA02","WA03","WA04","WA05","WA06","WA07",
                            "WA15","WA16","WA17","WA19")) %>%
    group_by(account) %>%
    dplyr::summarize(amount=sum(amount),transDate=first(transDate)),
    b2023.05 %>%
    filter(transCode %in% c("AD07","AD08","CF01",
                            "LC02","LC03","LC04","LC05","LC06","LC07",
                            "LC14","LC15","LC16",
                            "WA01","WA02","WA03","WA04","WA05","WA06","WA07",
                            "WA15","WA16","WA17","WA19")) %>%
    group_by(account) %>%
    dplyr::summarize(amount=sum(amount),transDate=first(transDate)),
    b2023.06 %>%
    filter(transCode %in% c("AD07","AD08","CF01",
                            "LC02","LC03","LC04","LC05","LC06","LC07",
                            "LC14","LC15","LC16",
                            "WA01","WA02","WA03","WA04","WA05","WA06","WA07",
                            "WA15","WA16","WA17","WA19")) %>%
    group_by(account) %>%
    dplyr::summarize(amount=sum(amount),transDate=first(transDate)),
    b2023.07 %>%
    filter(transCode %in% c("AD07","AD08","CF01",
                            "LC02","LC03","LC04","LC05","LC06","LC07",
                            "LC14","LC15","LC16",
                            "WA01","WA02","WA03","WA04","WA05","WA06","WA07",
                            "WA15","WA16","WA17","WA19")) %>%
    group_by(account) %>%
    dplyr::summarize(amount=sum(amount),transDate=first(transDate)),
    b2023.08 %>%
    filter(transCode %in% c("AD07","AD08","CF01",
                            "LC02","LC03","LC04","LC05","LC06","LC07",
                            "LC14","LC15","LC16",
                            "WA01","WA02","WA03","WA04","WA05","WA06","WA07",
                            "WA15","WA16","WA17","WA19")) %>%
    group_by(account) %>%
    dplyr::summarize(amount=sum(amount),transDate=first(transDate)),
    b2023.09 %>%
    filter(transCode %in% c("AD07","AD08","CF01",
                            "LC02","LC03","LC04","LC05","LC06","LC07",
                            "LC14","LC15","LC16",
                            "WA01","WA02","WA03","WA04","WA05","WA06","WA07",
                            "WA15","WA16","WA17","WA19")) %>%
    group_by(account) %>%
    dplyr::summarize(amount=sum(amount),transDate=first(transDate)),
    b2023.10 %>%
    filter(transCode %in% c("AD07","AD08","CF01",
                            "LC02","LC03","LC04","LC05","LC06","LC07",
                            "LC14","LC15","LC16",
                            "WA01","WA02","WA03","WA04","WA05","WA06","WA07",
                            "WA15","WA16","WA17","WA19")) %>%
    group_by(account) %>%
    dplyr::summarize(amount=sum(amount),transDate=first(transDate)),
    b2023.11 %>%
    filter(transCode %in% c("AD07","AD08","CF01",
                            "LC02","LC03","LC04","LC05","LC06","LC07",
                            "LC14","LC15","LC16",
                            "WA01","WA02","WA03","WA04","WA05","WA06","WA07",
                            "WA15","WA16","WA17","WA19")) %>%
    group_by(account) %>%
    dplyr::summarize(amount=sum(amount),transDate=first(transDate)),
    b2023.12 %>%
    filter(transCode %in% c("AD07","AD08","CF01",
                            "LC02","LC03","LC04","LC05","LC06","LC07",
                            "LC14","LC15","LC16",
                            "WA01","WA02","WA03","WA04","WA05","WA06","WA07",
                            "WA15","WA16","WA17","WA19")) %>%
    group_by(account) %>%
    dplyr::summarize(amount=sum(amount),transDate=first(transDate)) );
    
}


