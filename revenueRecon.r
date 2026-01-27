library(readxl)

rev24 <- read_excel("data/revenueData/FY24 Water and Irrigation Sales.xlsx") 
rev25 <- read_excel("data/revenueData/FY25 Water and Irrigation Sales.xlsx")
rev26 <- read_excel("data/revenueData/FY26 Water and Irrigation Sales.xlsx")



actualRev <- rbind(rev24 %>% 
             pivot_longer(cols=c("JUL","AUG","SEP","OCT","NOV","DEC",
                                 "JAN","FEB","MAR","APR","MAY","JUN"),
                          names_to="dmonth",values_to="revenue") %>%
             mutate(year=ifelse(dmonth %in% c("JUL","AUG","SEP",
                                              "OCT","NOV","DEC"),2023,2024),
                    date=dmy(paste0("15-",dmonth,"-",year))),
             rev25 %>% 
             pivot_longer(cols=c("JUL","AUG","SEP","OCT","NOV","DEC",
                                 "JAN","FEB","MAR","APR","MAY","JUN"),
                          names_to="dmonth",values_to="revenue") %>%
             mutate(year=ifelse(dmonth %in% c("JUL","AUG","SEP",
                                              "OCT","NOV","DEC"),2024,2025),
                    date=dmy(paste0("15-",dmonth,"-",year))),
             rev26 %>% 
             pivot_longer(cols=c("JUL","AUG","SEP","OCT","NOV","DEC",
                                 "JAN","FEB","MAR","APR","MAY","JUN"),
                          names_to="dmonth",values_to="revenue") %>%
             mutate(year=ifelse(dmonth %in% c("JUL","AUG","SEP",
                                              "OCT","NOV","DEC"),2025,2026),
                    date=dmy(paste0("15-",dmonth,"-",year))) %>%
             ## FY26 is not done yet, so filter out the future records.
             filter(revenue > 0) )  %>%
    mutate(account=as.integer(ACCOUNT),
           rateCode=BILLCODE,
           description=DESCRIPTION,
           total=TOTAL,
           month=convertDateToInteger(year,month(date))) %>%
    select(-ACCOUNT,-BILLCODE,-DESCRIPTION,-TOTAL) %>%
    select(account,rateCode,month,revenue,date,dmonth,year,description,total)

## Note that each row of the rates data has a 'validUntilDate'. It's
## set so that the most recent rate has a validUntilDate of 1/1/2100,
## so the join here only has to choose the closest one in the future
## from a given billDate.
rev <- waterResid %>%
    left_join(rates, join_by(rateCode, closest(billDate <= validUntilDate))) %>%
    mutate(billMonth=convertDateToInteger(year(billDate),month(billDate))) %>%
    mutate(billAmount=applyWaterRate(consumption,baseFee,t1,t2,t3,t4),
           predBillAmount=applyWaterRate(predUsage,baseFee,t1,t2,t3,t4))

dateStrings <- rev %>%
    mutate(billMonth=convertDateToInteger(year(billDate),month(billDate))) %>%
    group_by(billMonth) %>%
    dplyr::summarize(month=month(first(billDate)),
                     year=year(first(billDate)),
                     dateString=paste0(month, "/", year)) %>%
    select(billMonth, dateString)

##dateStrings[26,1] <- 72;
##dateStrings[26,2] <- "6/2025";

breaks <- seq(48,max(rev$billMonth, na.rm=TRUE),3)

labels <- dateStrings %>%
    filter(billMonth %in% breaks) %>%
    select(dateString) %>%
    unlist()

revPlot <- rev %>%
##    filter(accountType=="Residential/Single Family") %>%
    group_by(billMonth) %>%
    dplyr::summarize(billAmount=sum(billAmount,na.rm=TRUE),
                     predBillAmount=sum(predBillAmount,na.rm=TRUE)) %>%
    ggplot() +
    geom_line(aes(x=billMonth,y=billAmount),color="blue") +
    geom_line(aes(x=billMonth,y=predBillAmount),color="red") +
    geom_point(aes(x=month,y=revenue),
               data=actualRev %>% group_by(month) %>%
                   dplyr::summarize(revenue=sum(revenue,na.rm=TRUE))) +
    scale_x_continuous(breaks=breaks,
                       labels=labels,
                       limits=c(47,NA),
                       name="") +
    scale_y_continuous(breaks=seq(0,4e6,1e6),
                       labels=c("$0","$1M","$2M","$3M","$4M"),
                       name="Monthly Revenue") +
   theme(axis.text.x = element_text(angle = 45, hjust=1.0, vjust=1.0)) 

errorPlot <- rev %>%
    mutate(billMonth=convertDateToInteger(year(billDate),month(billDate))) %>%
    group_by(billMonth) %>%
    dplyr::summarize(billAmount=sum(billAmount,na.rm=TRUE),
                     predBillAmount=sum(predBillAmount,na.rm=TRUE)) %>%
    ggplot() +
    geom_line(aes(x=billMonth,y=(billAmount/predBillAmount)-1),color="blue") +
    scale_x_continuous(breaks=breaks,
                       labels=dateStrings$dateString[breaks+6],
                       limits=c(47,NA),
                       name="")

##Residential plot
##Updated with x and y scale
resPlot <-rev %>% filter(waterUseSegment=="Residential") %>% 
  mutate(imonth=convertDateToInteger(year(billDate),month(billDate))) %>% group_by(imonth) %>%   
  summarise(billAmount=sum(billAmount,na.rm=TRUE),predBillAmount=sum(predBillAmount,na.rm=TRUE)) %>%
  ggplot() + geom_line(aes(x=imonth,y=billAmount),color="blue") + 
  geom_line(aes(x=imonth,y=predBillAmount),color="red") +
  scale_x_continuous(breaks=breaks,
                     labels=labels,
                     limits=c(47,NA),
                     name="") +
  scale_y_continuous(breaks=seq(0,4e6,1e6),
                     labels=c("$0","$1M","$2M","$3M","$4M"),
                     limits=c(0,4000000),
                     name="Monthly Revenue") +
  theme(axis.text.x = element_text(angle = 45, hjust=1.0, vjust=1.0)) 

