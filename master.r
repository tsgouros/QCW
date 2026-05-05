## A master file, describing the steps of our processing.
##
library(tidyverse)
library(anytime)
library(lubridate)
library(readxl)
library(grid)
library(png)
library(ggplot2)
library(ggrepel)

## These are the time-consuming tasks, with flags to run them or not.

## Getting the more recent billing data.
updateBillingData <- TRUE;
## ... and the county data.
updateCountyData <- TRUE;
## Assemble the big waterTable.
assembleWaterTable <- TRUE;
## Doing the sampling and preliminary processing of the big pile of data.
updateRegression <- TRUE;

## Output graphics
## saveGraphs <- FALSE;

## Not time consuming, but this data is good for debugging.
cleanup <- TRUE;

## To establish consistency in creating the time base from year and
## month data.
convertDateToInteger <- function(year, month) {
    return((year - 2019) * 12 + (month - 6));
}

## Convert a date into a floating-point number of months since 1/1/2019.
convertDate <- function(date) {
    return(((year(date) - 2019) * 12) +
           (month(date) - 6) +
           ((day(date) - 1)/(ifelse(month(date) %in% c(1,3,5,7,8,10,12), 31,
                             ifelse(month(date) == 2, 28, 30)))));
}

## *** GATHER DATA
##
##
## First, incorporate the data from QC water.
if (updateBillingData) {
    cat("updating billing data\n");
    source("intake-new.r");
} else {
    cat("skip updating billing data\n");
}

##
## Read the property data from Maricopa and Pinal counties.
## This creates a big array called 'prop'.
if (updateCountyData) {
    cat("reading county data\n");
    source("read-county-data.r");
} else {
    cat("skip reading county data\n");
}

if (assembleWaterTable) {
    cat("assembling waterTable\n");
    waterTable <- billTable %>%
        ## April 2023 had stolen some bills from March in 2023. Maybe
        ## May, too?
        mutate(billMonth=ifelse(billDate>ymd("2023-03-01")&
                                billDate<ymd("2023-04-12"),
                                3,
                         ifelse(billDate>=ymd("2023-04-12")&
                                billDate<ymd("2023-05-09"),
                                4,
                                billMonth))) %>%
        right_join(readingTable %>%
                   select(billNumber, service, meter, currentReading,
                          previousReading, consumption, rateCode, meterBoxID,
                          readingDate, readingMonth, readStatus,
                          previousReadingDate, previousReadingMonth,
                          readingYear,meterID) %>%
                   ## there are records where the following is not
                   ## true, seem mostly to be from 2/2023.
                   filter(consumption == (currentReading - previousReading)),
                   by="billNumber") %>%
        left_join(customerTable %>% group_by(customer) %>%
                  dplyr::summarize(revenueClass=first(revenueClass)),
                  by="customer") %>%
        left_join(addressTable %>%
                  select(account, subdivision, latitude, longitude,
                         taxProfile, parcel, TAZ_2019, GRD),
                  by="account") %>%
        mutate(waterUseSegment=ifelse(service=="Irrigation",
                                      "Irrigation",
                               ifelse(service=="Landscape",
                               ifelse(revenueClass=="RECOVERED EFFLUENT",
                                      "Recovered Effluent",
                                      "Landscape"),
                               ifelse(revenueClass=="SCHOOL/LARGE TURF 10+ ACRES" |
                                      revenueClass=="SCHOOL",
                                      "School",
                               ifelse(service=="Rental Meters",
                                      "Construction",
                               ifelse(accountType=="Residential/Single Family" | accountType=="Multi-Unit Family",
                                      "Residential",
                               ifelse(accountType=="Town of Queen Creek",
                                      "Government",
                               ifelse(accountType=="Commercial",
                                      "Commercial",
                               ifelse(accountType=="Temp Meter",
                                      "Construction",
                                      "Unknown")))))))))
} else {
    cat("skip assembling waterTable\n");
}


## Combine the big and waterTable usage records. Watch out, this one
## can take a long time, depending on the flags set over there.
if (updateRegression) {
    cat("updating regression parameters\n");
    source("prelim.r", chdir=TRUE);
} else {
    cat("skip updating regression parameters\n");
}

## Bring in precipitation and temperature records
##source("rain.r")





