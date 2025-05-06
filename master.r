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
recompile <- FALSE;
## Getting the data from the original water billing data...
recompile.pre.2023 <- FALSE;
## ... and the county data.
recompile.county <- FALSE;
## Doing the sampling and preliminary processing of the big pile of data.
prelim <- FALSE;
## Creating the model with account data and growth projections
incorporateGrowth <- FALSE;
## Output graphics
saveGraphs <- FALSE;

## Output graphics
saveGraphs <- FALSE;

## Not time consuming, but this data is good for debugging.
cleanup <- FALSE;

## To establish consistency in creating the time base from year and
## month data.
convertDateToInteger <- function(year, month) {
    return((year - 2017) * 12 + (month - 6));
}

## *** GATHER DATA
##
##
## First, incorporate the data from QC water.
if (recompile) source("intake-new.r");

##
## Read the property data from Maricopa and Pinal counties.
##if (recompile.county) source("~/osa/20/queenscreek/read-county-data.r",
##                             chdir=TRUE);
## This creates a big array called 'prop'.

waterTable <- billTable %>%
    ## April 2023 had stolen some bills from March in 2023. Maybe May, too? 
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
                     previousReadingDate, previousReadingMonth) %>%
              ## there are records where the following is not true, seem mostly
              ## to be from 2/2023.
              filter(consumption == (currentReading - previousReading)),
              by="billNumber") %>%
    left_join(customerTable %>% group_by(customer) %>%
              dplyr::summarize(revenueClass=first(revenueClass)),
              by="customer") %>%
    left_join(addressTable %>%
                select(account, subdivision, latitude, longitude, taxProfile, parcel),
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

## Bring in precipitation and temperature records
##source("rain.r")

## Make a usage record with quarterly averages to match the seasonal
## weather records. These are 3-month seasons, starting with July,
## though perhaps we should test that.

## Combine the big and waterTable usage records. Watch out, this one
## can take a long time, depending on the flags set over there.
##if (prelim) source("~/osa/20/queenscreek/master-taz.r", chdir=TRUE);

