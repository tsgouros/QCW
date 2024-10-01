## See ~/osa/20/queenscreek/assimilate.r
library(anytime)
library(tidyverse)
library(lubridate)

read.and.summarize <- function(pathName) {

    ## Parse the date out of the file name.
    months <- c("Jan","Feb","Mar","Apr","May","Jun",
                "Jul","Aug","Sep","Oct","Nov","Dec");

    ## remove the directory names.
    fileName <- last(strsplit(pathName, "/")[[1]]);
    splitName <- strsplit(fileName, "-")[[1]];

    imnth <- as.numeric(splitName[1]);
    mmnth <- which(splitName[2] == months);
    ## Remove the "a.ttx" from the year.
    myear <- as.numeric(strsplit(splitName[3], "a\\.")[[1]][1]);
    if (is.na(myear))
        myear <- as.numeric(strsplit(splitName[3], "\\.")[[1]][1]);
    cat("Reading and summarizing", mmnth, "/", myear, "\n");

    cnames <-   c("acct",     "parcel",   "locClass", "class", "rate",
                  "usage",    "charge",   "lat",      "lon",
                  "subdiv",   "createDate", "locID");
    cclasses <- c("character","character","character","character","character",
                  "numeric",  "numeric",  "character","character",
                  "character","character", "character");
    test <- read.csv(pathName, sep="\t", skip=8, header=FALSE,
                     col.names=cnames, colClasses=cclasses);

    ## The last line of each ttx file appears to be trash.
    test <- test[1:(length(test$acct)-1),];

    endOfYear <- anytime("6/30/2023");

    test <- test %>%
        mutate(lat=as.numeric(lat)) %>%
        ## The gsub is to take care of an account where the lon is mistyped.
        mutate(lon=as.numeric(gsub("\\.\\.",".",lon))) %>%
        mutate(locID=gsub(",","",locID)) %>%
        mutate(locClass=toupper(locClass)) %>%
        mutate(acctAge=as.numeric(endOfYear - anytime(createDate))) %>%
        group_by(acct, locID, parcel, rate) %>%
        dplyr::summarize(locClass=first(locClass),
                         class=first(class),
                         lat=first(lat),
                         lon=first(lon),
                         subdiv=first(subdiv),
                         mmnth=mmnth,
                         myear=myear,
                         imnth=imnth,
                         acctAge=first(acctAge),
                         usage=sum(usage),
                         charge=sum(charge),
                         .groups="drop");
    
##    cat(">>", fileName, mmnth, myear, "\n");
    return(test);
}

system("date")
big <- read.and.summarize("~/osa/20/queenscreek/data/water-data/171-Jul-2017a.ttx");
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/172-Aug-2017a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/173-Sep-2017a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/174-Oct-2017a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/175-Nov-2017a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/176-Dec-2017a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/177-Jan-2018a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/178-Feb-2018a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/179-Mar-2018a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/180-Apr-2018a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/181-May-2018a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/182-Jun-2018a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/183-Jul-2018a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/184-Aug-2018a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/185-Sep-2018a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/186-Oct-2018a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/187-Nov-2018a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/188-Dec-2018a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/189-Jan-2019a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/190-Feb-2019a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/191-Mar-2019a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/192-Apr-2019a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/193-May-2019a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/194-Jun-2019a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/195-Jul-2019a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/196-Aug-2019a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/197-Sep-2019a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/198-Oct-2019a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/199-Nov-2019a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/200-Dec-2019a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/201-Jan-2020a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/202-Feb-2020a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/203-Mar-2020a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/204-Apr-2020a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/205-May-2020a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/206-Jun-2020a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/207-Jul-2020a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/208-Aug-2020a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/209-Sep-2020a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/210-Oct-2020a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/211-Nov-2020a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/212-Dec-2020a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/213-Jan-2021a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/214-Feb-2021a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/215-Mar-2021a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/216-Apr-2021a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/217-May-2021a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/218-Jun-2021a.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/219-Jul-2021-v2.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/220-Aug-2021-v2.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/221-Sep-2021-v2.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/222-Oct-2021-v2.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/223-Nov-2021-v2.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/224-Dec-2021-v2.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/225-Jan-2022-v2.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/226-Feb-2022-v2.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/227-Mar-2022-v2.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/228-Apr-2022-v2.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/229-May-2022-v2.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/230-Jun-2022-v2.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/231-Jul-2022.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/232-Aug-2022.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/233-Sep-2022.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/234-Oct-2022.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/235-Nov-2022.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/236-Dec-2022.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/237-Jan-2023.ttx"));
big <- rbind(big, read.and.summarize("~/osa/20/queenscreek/data/water-data/238-Feb-2023.ttx"));

big <- as_tibble(big);

system("date")


