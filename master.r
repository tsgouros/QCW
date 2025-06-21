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
updateBillingData <- FALSE;
## ... and the county data.
updateCountyData <- FALSE;
## Assemble the big waterTable.
assembleWaterTable <- FALSE;
## Doing the sampling and preliminary processing of the big pile of data.
updateRegression <- TRUE;
## We're going to make some projections of usage, and probably
## revenue, too so we need the location data in an easy-to-use format.
updateLocationData <- TRUE;



## Creating the model with account data and growth projections
## incorporateGrowth <- FALSE;

## Output graphics
## saveGraphs <- FALSE;

## Not time consuming, but this data is good for debugging.
## cleanup <- FALSE;

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
if (updateCountyData) {
    cat("reading county data\n");
    source("read-county-data.r");
} else {
    cat("skip reading county data\n");
}
## This creates a big array called 'prop'.

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
                          previousReading, consumption,
                          readingDate, readingMonth, readStatus,
                          previousReadingDate, previousReadingMonth,
                          readingYear) %>%
                   ## there are records where the following is not
                   ## true, seem mostly to be from 2/2023.
                   filter(consumption == (currentReading - previousReading)),
                   by="billNumber") %>%
        left_join(customerTable %>% group_by(customer) %>%
                  dplyr::summarize(revenueClass=first(revenueClass)),
                  by="customer") %>%
        left_join(addressTable %>%
                  select(account, subdivision, latitude, longitude,
                         taxProfile, parcel),
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

## Add location data to the waterTable and water.means tables. This is
## both the property data from the county assessors and the TAZ
## data. The TAZ data is essential because we need some area within
## which to estimate usage and do our sampling.
if (updateLocationData) {
    cat("update location data\n");
    TAZbyParcel <- read.csv("data/Permits-by-parcel.csv",header=TRUE) %>%
        as_tibble() %>%
        rowwise() %>%
        ## Some large parcels overlap multiple TAZs. We only use the
        ## first, somewhat arbitrarily.
        mutate(TAZ_2019=gsub("/.*$","",TAZ_2019)) 

    locData <- TAZbyParcel %>%
        select(parcel, TAZ_2019) %>%
        filter(parcel != "") %>%
        distinct()

    water.means <- water.means %>% left_join(locData, by="parcel")
    water.resid <- water.resid %>% left_join(locData, by="parcel")
} else {
    cat("skip update location data\n");
}





