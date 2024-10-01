## A library for multi-graph graphs.
library(patchwork)
library(lubridate)

## Grab the "monthly" weather data from
## https://cales.arizona.edu/azmet/22.htm
## The sed business preserves the data lines in the file and the
## headers for each month, while the awk grabs the month from the
## header and adds it to the data. The lubridate parse_date_time
## function reads the month name and month() transforms it to a
## numerical month value.
system("sed -E '/^ +[0-9]+ / p;/MX MN/ p;d' data/rainfall/2217em.txt | awk '/^ [A-z]+/{split($0,a,/ +/);month=a[2]} /^ +[0-9]+ /{print \"2017\", month, $0} ' >trash");
weather.2017 <-
    read.csv("trash",header=FALSE, sep="",
             col.names=c("year","Month","day","tmpMax","tmpMin","tmpAvg",
                         "dewPt","humMax","humMin","humAvg","soilTmp4",
                         "soilTmp20","windAvg","windMax","sol","rain",
                         "etoAZ","etoSTD","heat55","heat50","heat45")) %>%
    rowwise() %>%
    mutate(dateStr=paste0(year,"-",Month,"-",day),
           date=parse_date_time(dateStr, order="ymd"),
           month=month(date)) %>%
    select(date,year,month,day,tmpMax,tmpMin,tmpAvg,dewPt,rain) %>%
    as_tibble();

system("sed -E '/^ +[0-9]+ / p;/MX MN/ p;d' data/rainfall/2218em.txt | awk '/^ [A-z]+/{split($0,a,/ +/);month=a[2]} /^ +[0-9]+ /{print \"2018\", month, $0} ' >trash");
weather.2018 <-
    read.csv("trash",header=FALSE, sep="",
             col.names=c("year","Month","day","tmpMax","tmpMin","tmpAvg",
                         "dewPt","humMax","humMin","humAvg","soilTmp4",
                         "soilTmp20","windAvg","windMax","sol","rain",
                         "etoAZ","etoSTD","heat55","heat50","heat45")) %>%
    rowwise() %>%
    mutate(dateStr=paste0(year,"-",Month,"-",day),
           date=parse_date_time(dateStr, order="ymd"),
           month=month(date)) %>%
    select(date,year,month,day,tmpMax,tmpMin,tmpAvg,dewPt,rain) %>%
    as_tibble();

system("sed -E '/^ +[0-9]+ / p;/MX MN/ p;d' data/rainfall/2219em.txt | awk '/^ [A-z]+/{split($0,a,/ +/);month=a[2]} /^ +[0-9]+ /{print \"2019\", month, $0} ' >trash");
weather.2019 <-
    read.csv("trash",header=FALSE, sep="",
             col.names=c("year","Month","day","tmpMax","tmpMin","tmpAvg",
                         "dewPt","humMax","humMin","humAvg","soilTmp4",
                         "soilTmp20","windAvg","windMax","sol","rain",
                         "etoAZ","etoSTD","heat55","heat50","heat45")) %>%
    rowwise() %>%
    mutate(dateStr=paste0(year,"-",Month,"-",day),
           date=parse_date_time(dateStr, order="ymd"),
           month=month(date)) %>%
    select(date,year,month,day,tmpMax,tmpMin,tmpAvg,dewPt,rain) %>%
    as_tibble();

system("sed -E '/^ +[0-9]+ / p;/MX MN/ p;d' data/rainfall/2220em.txt | awk '/^ [A-z]+/{split($0,a,/ +/);month=a[2]} /^ +[0-9]+ /{print \"2020\", month, $0} ' >trash");
weather.2020 <-
    read.csv("trash",header=FALSE, sep="",
             col.names=c("year","Month","day","tmpMax","tmpMin","tmpAvg",
                         "dewPt","humMax","humMin","humAvg","soilTmp4",
                         "soilTmp20","windAvg","windMax","sol","rain",
                         "etoAZ","etoSTD","heat55","heat50","heat45")) %>%
    rowwise() %>%
    mutate(dateStr=paste0(year,"-",Month,"-",day),
           date=parse_date_time(dateStr, order="ymd"),
           month=month(date)) %>%
    select(date,year,month,day,tmpMax,tmpMin,tmpAvg,dewPt,rain) %>%
    as_tibble();

system("sed -E '/^ +[0-9]+ / p;/MX MN/ p;d' data/rainfall/2221em.txt | awk '/^ [A-z]+/{split($0,a,/ +/);month=a[2]} /^ +[0-9]+ /{print \"2021\", month, $0} ' >trash");
weather.2021 <-
    read.csv("trash",header=FALSE, sep="",
             col.names=c("year","Month","day","tmpMax","tmpMin","tmpAvg",
                         "dewPt","humMax","humMin","humAvg","soilTmp4",
                         "soilTmp20","windAvg","windMax","sol","rain",
                         "etoAZ","etoSTD","heat55","heat50","heat45")) %>%
    rowwise() %>%
    mutate(dateStr=paste0(year,"-",Month,"-",day),
           date=parse_date_time(dateStr, order="ymd"),
           month=month(date)) %>%
    select(date,year,month,day,tmpMax,tmpMin,tmpAvg,dewPt,rain) %>%
    as_tibble();

system("sed -E '/^ +[0-9]+ / p;/MX MN/ p;d' data/rainfall/2222em.txt | awk '/^ [A-z]+/{split($0,a,/ +/);month=a[2]} /^ +[0-9]+ /{print \"2022\", month, $0} ' >trash");
weather.2022 <-
    read.csv("trash",header=FALSE, sep="",
             col.names=c("year","Month","day","tmpMax","tmpMin","tmpAvg",
                         "dewPt","humMax","humMin","humAvg","soilTmp4",
                         "soilTmp20","windAvg","windMax","sol","rain",
                         "etoAZ","etoSTD","heat55","heat50","heat45")) %>%
    rowwise() %>%
    mutate(dateStr=paste0(year,"-",Month,"-",day),
           date=parse_date_time(dateStr, order="ymd"),
           month=month(date)) %>%
    select(date,year,month,day,tmpMax,tmpMin,tmpAvg,dewPt,rain) %>%
    as_tibble();

system("sed -E '/^ +[0-9]+ / p;/MX MN/ p;d' data/rainfall/2223em.txt | awk '/^ [A-z]+/{split($0,a,/ +/);month=a[2]} /^ +[0-9]+ /{print \"2023\", month, $0} ' >trash");
weather.2023 <-
    read.csv("trash",header=FALSE, sep="",
             col.names=c("year","Month","day","tmpMax","tmpMin","tmpAvg",
                         "dewPt","humMax","humMin","humAvg","soilTmp4",
                         "soilTmp20","windAvg","windMax","sol","rain",
                         "etoAZ","etoSTD","heat55","heat50","heat45")) %>%
    rowwise() %>%
    mutate(dateStr=paste0(year,"-",Month,"-",day),
           date=parse_date_time(dateStr, order="ymd"),
           month=month(date)) %>%
    select(date,year,month,day,tmpMax,tmpMin,tmpAvg,dewPt,rain) %>%
    as_tibble();

system("sed -E '/^ +[0-9]+ / p;/MX MN/ p;d' data/rainfall/2224em.txt | awk '/^ [A-z]+/{split($0,a,/ +/);month=a[2]} /^ +[0-9]+ /{print \"2024\", month, $0} ' >trash");
weather.2024 <-
    read.csv("trash",header=FALSE, sep="",
             col.names=c("year","Month","day","tmpMax","tmpMin","tmpAvg",
                         "dewPt","humMax","humMin","humAvg","soilTmp4",
                         "soilTmp20","windAvg","windMax","sol","rain",
                         "etoAZ","etoSTD","heat55","heat50","heat45")) %>%
    rowwise() %>%
    mutate(dateStr=paste0(year,"-",Month,"-",day),
           date=parse_date_time(dateStr, order="ymd"),
           month=month(date)) %>%
    select(date,year,month,day,tmpMax,tmpMin,tmpAvg,dewPt,rain) %>%
    as_tibble();


## Make this all into a huge daily weather record.
weather <- rbind(weather.2017, weather.2018, weather.2019,
                 weather.2020,weather.2021, weather.2022,
                 weather.2023,weather.2024) %>%
    mutate(imnth=convertDateToInteger(year, month));

rm(weather.2017, weather.2018, weather.2019,
   weather.2020, weather.2021, weather.2022,
   weather.2023,weather.2024);
system("rm trash");

## Make a monthly rainfall report from the data at
## https://cales.arizona.edu/azmet/22.htm
## These are the reports in the "Standard reports / Monthly" column.

system("data/rainfall/condense.sh data/rainfall/2224em.txt >trash")
rain.24 <- read.csv("trash", header=FALSE, sep=" ",
                    col.names=c("month","maxTmp","minTmp","avgTmp","rain")) %>%
    as_tibble() %>%
    mutate(year=2024)

system("data/rainfall/condense.sh data/rainfall/2223em.txt >trash")
rain.23 <- read.csv("trash", header=FALSE, sep=" ",
                    col.names=c("month","maxTmp","minTmp","avgTmp","rain")) %>%
    as_tibble() %>%
    mutate(year=2023)

system("data/rainfall/condense.sh data/rainfall/2222em.txt >trash")
rain.22 <- read.csv("trash", header=FALSE, sep=" ",
                    col.names=c("month","maxTmp","minTmp","avgTmp","rain")) %>%
    as_tibble() %>%
    mutate(year=2022)

system("data/rainfall/condense.sh data/rainfall/2221em.txt >trash")
rain.21 <- read.csv("trash", header=FALSE, sep=" ",
                    col.names=c("month","maxTmp","minTmp","avgTmp","rain")) %>%
    as_tibble() %>%
    mutate(year=2021)

system("data/rainfall/condense.sh data/rainfall/2220em.txt >trash")
rain.20 <- read.csv("trash", header=FALSE, sep=" ",
                    col.names=c("month","maxTmp","minTmp","avgTmp","rain")) %>%
    as_tibble() %>%
    mutate(year=2020)

system("data/rainfall/condense.sh data/rainfall/2219em.txt >trash")
rain.19 <- read.csv("trash", header=FALSE, sep=" ",
                    col.names=c("month","maxTmp","minTmp","avgTmp","rain")) %>%
    as_tibble() %>%
    mutate(year=2019)

system("data/rainfall/condense.sh data/rainfall/2218em.txt >trash")
rain.18 <- read.csv("trash", header=FALSE, sep=" ",
                    col.names=c("month","maxTmp","minTmp","avgTmp","rain")) %>%
    as_tibble() %>%
    mutate(year=2018)

system("data/rainfall/condense.sh data/rainfall/2217em.txt >trash")
rain.17 <- read.csv("trash", header=FALSE, sep=" ",
                    col.names=c("month","maxTmp","minTmp","avgTmp","rain")) %>%
    as_tibble() %>%
    mutate(year=2017)

system("rm trash");

months <- rain.17$month;
rain <- rbind(rain.17, rain.18, rain.19, rain.20, rain.21, rain.22,
              rain.23, rain.24) %>%
    rowwise() %>% 
    mutate(month=which(months == month),
           imnth=convertDateToInteger(year, month),
           season=ifelse(month==1,"winter",
                  ifelse(month==2,"winter",
                  ifelse(month==3,"spring",
                  ifelse(month==4,"spring",
                  ifelse(month==5,"spring",
                  ifelse(month==6,"summer",
                  ifelse(month==7,"summer",
                  ifelse(month==8,"summer",
                  ifelse(month==9,"fall",
                  ifelse(month==10,"fall",
                  ifelse(month==11,"fall",
                  ifelse(month==12,"winter","unknown"))))))))))))) %>%
    select(year, month, imnth, maxTmp, minTmp, avgTmp, rain, season) %>%
    left_join(weather %>%
              group_by(imnth) %>%
              dplyr::summarize(rainN=sum(rain>0)),by="imnth")

rm(months)
rm(rain.17)
rm(rain.18)
rm(rain.19)
rm(rain.20)
rm(rain.21)
rm(rain.22)
rm(rain.23)
rm(rain.24)

rain.seasonal <- rain %>%
    mutate(season=floor(imnth/3)) %>%
    group_by(season) %>%
    dplyr::summarize(rain=sum(rain),imnth=max(imnth))

## Try creating a rain anomaly variable to get at the monthly anomaly.
rain <- rain %>%
    left_join(rain %>%
              group_by(month) %>%
              dplyr::summarize(avgRain=mean(rain,na.rm=TRUE),
                               avgRainN=mean(rainN,na.rm=TRUE)),by="month") %>%
    mutate(rainAnom=(rain-avgRain)/avgRain,
           rainNAnom=(rainN-avgRainN)/avgRainN)

## Let's do a temperature anomaly while we're at it.
rain <- rain %>%
    left_join(rain %>%
              group_by(month) %>%
              dplyr::summarize(meanTmp=mean(avgTmp,na.rm=TRUE)),by="month") %>%
    mutate(tmpAnom=(avgTmp-meanTmp))

## The following usage projection functions and approach are from
## master-taz.r, adapted to take the rainfall anomaly into account.
##
##
## As of here, we have the model, a collection of samples for each growth
## area, and a time series, also for each growth area.  We also have the
## billing and actual usage data all in one place.  So now we make a function
## to accept the model and a time (integer month, starting from 7/2017=0) and
## return a prediction of the usage and charges for that period.
##
projectAreaMonthUsageClassPrecip <- function(sample, N, targetTime, rainAnom) {

    sampleN <- dim(sample)[1];
    
    if (sampleN == 0) {
        usage <- 0;
        usage.slp <- 0;
        usage.sea <- 0;
        usage.nse <- 0;
        
        charge <- 0;

    } else {
        ## Convert our time variable to a season.
        targetSeas <- targetTime * pi/6;
        sample <- sample %>%
            mutate(usage=calculateUsage(rainAnom*amp, off, 0, nls, targetSeas),
                   usage.slp=calculateUsage(rainAnom*amp, off, slp, nls, targetSeas),
                   usage.sea=calculateUsage(0.9 * rainAnom* amp, off, 0, nls, targetSeas),
                   usage.nse=calculateUsage(rainAnom*amp, 0.9 * off, 0, nls, targetSeas)) %>%
            mutate(charge=rateFunction.v(parcel, acct, lat, lon, TAZ_2019,
                                       avg, mx, amp, off, slp, nls, subdiv,
                                       rate, class, locClass, fcv, area,
                                       acctAge, section, township, range,
                                       county, address, lat.orig, lon.orig, lv,
                                       usage));
        ## print(sample %>% select(parcel, acct, usage, charge));

        ## THERE SHOULD BE NO NAs HERE!!!!!
        ## Sum up the usage and charges and project the sample to the whole.
        usage <- (N/sampleN) * sum(sample$usage, na.rm=TRUE);
        usage.slp <- (N/sampleN) * sum(sample$usage.slp, na.rm=TRUE);
        usage.sea <- (N/sampleN) * sum(sample$usage.sea, na.rm=TRUE);
        usage.nse <- (N/sampleN) * sum(sample$usage.nse, na.rm=TRUE);
        charge <- (N/sampleN) * sum(sample$charge, na.rm=TRUE);

        if (is.na(usage)) usage <- 0;
        if (is.na(charge)) charge <- 0;
    }

    return(list(N=N, usage=usage, charge=charge,
                usage.slp=usage.slp, usage.sea=usage.sea, usage.nse=usage.nse));
}

projectAreaMonthUsagePrecip <- function(areaModel, targetTime, rainAnom) {

    resResults <- 
        projectAreaMonthUsageClassPrecip(areaModel$resSample,
                                   as.numeric( areaModel$Nt %>%
                                               filter(t == targetTime) %>%
                                               mutate(sum = resN + simResN) %>%
                                               select(sum) ),
                                   targetTime,
                                   rainAnom);
    bldResults <- 
        projectAreaMonthUsageClassPrecip(areaModel$bldSample,
                                   as.numeric( areaModel$Nt %>%
                                               filter(t == targetTime) %>%
                                               mutate(sum = bldN + simBldN) %>%
                                               select(sum) ),
                                   targetTime,
                                   1);

    
    ## This is an adjustment for a slight overestimate of N.  This
    ## appears to be a discrepancy between the billing data and the
    ## actual bills.  Will hunt further. 3/12
    adj <- 1.0; ## adj <- 0.95;

    return(list(resN=round(adj * resResults[["N"]]),
                bldN=round(adj * bldResults[["N"]]),
                resUsage=  adj * resResults[["usage"]],
                resUsage.slp=  adj * resResults[["usage.slp"]],
                resUsage.sea=  adj * resResults[["usage.sea"]],
                resUsage.nse=  adj * resResults[["usage.nse"]],
                resCharge= adj * resResults[["charge"]],
                bldUsage=  adj * bldResults[["usage"]],
                bldCharge= adj * bldResults[["charge"]]));
}

##
## Use the monthly usage projector to construct a tbl of all the TAZs over a
## given time frame.
projectUsagePrecip <- function(model, startTime, endTime, rain, fudge=0.175) {
    out <- as_tibble(data.frame(TAZ_2019=c(), time=c(),
                                resN=c(), resUsage=c(), resUsage.slp=c(),
                                resUsage.sea=c(), resUsage.nse=c(),
                                resCharge=c(),
                                bldN=c(), bldUsage=c(), bldCharge=c(),
                                usage=c(), charge=c()));

    for (time in startTime:endTime) {
        cat("projecting usage for time:", time, "\n");
        for (taz in names(model)) {

            rainAnom <- 1;
            if (time <= max(rain$imnth)) {
                ##cat("rain:",
                rainRow <- rain %>% filter(imnth==time+1);
                ##rainAnom <- 1 - fudge * min(0,rainRow$rainAnom);
                ## rainAnom <- 1 - fudge * rainRow$rainAnom;
                rainAnom <- 1 + fudge*rainRow$tmpAnom;
            }
            
            areaUsage <- projectAreaMonthUsagePrecip(model[[taz]],
                                                     time, rainAnom);

            out <- rbind(out,
                         data.frame(TAZ_2019=c(taz), time=c(time),
                                    resN=c(areaUsage$resN),
                                    resUsage=c(areaUsage$resUsage),
                                    resUsage.slp=c(areaUsage$resUsage.slp),
                                    resUsage.sea=c(areaUsage$resUsage.sea),
                                    resUsage.nse=c(areaUsage$resUsage.nse),
                                    resCharge=c(areaUsage$resCharge),
                                    bldN=c(areaUsage$bldN),
                                    bldUsage=c(areaUsage$bldUsage),
                                    bldCharge=c(areaUsage$bldCharge),
                                    usage=c(areaUsage$bldUsage +
                                            areaUsage$resUsage),
                                    charge=c(areaUsage$bldCharge +
                                            areaUsage$resCharge)));
        }
    }

    return(tibble(out));
}


## Things to try:
## Lag the rain anomaly?
## Smooth it.
## Some kind of threshold thing?

rainRecord <- ggplot(big.usageAnom) +
    geom_line(aes(x=imnth-170,y=rain),color="blue") +
    ylab("Rain (tot)") +
    theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(1,1,0,1), "mm"))
tmpRecord <- ggplot(big.usageAnom) +
    geom_line(aes(x=imnth-170,y=avgTmp),color="red")  +
    ylab("Temp (avg)") +
    theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(1,1,0,1), "mm"))

rainAnomRecord <- ggplot(big.usageAnom) +
    geom_line(aes(x=imnth-170,y=rainAnom),color="blue")  +
    ylab("Rain anomaly") +
    theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(1,1,0,1), "mm"))
tmpAnomRecord <- ggplot(big.usageAnom) +
    geom_line(aes(x=imnth-170,y=tmpAnom),color="red") +
    ylab("Temp anomaly") +
    scale_x_continuous(breaks=c(0,6,12,18,24,30,36,42,48,54,60,66,72,78),
                       labels=c("6/17","12/17","6/18","12/18","6/19","12/19",
                                "6/20","12/20","6/21","12/21","6/22","12/23",
                                "6/23","12/23")) +
    theme(plot.margin = unit(c(1,1,4,1), "mm"),
          axis.title.x=element_blank());

## This funny syntax is from the patchwork library.
ggsave("images/weather.png",
       plot=rainRecord / rainAnomRecord / tmpRecord / tmpAnomRecord,
       width=6,height=6,units="in");
