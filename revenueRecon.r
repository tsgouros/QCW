library(readxl)

#rev24 <- read_excel("data/revenueData/FY24 Water and Irrigation Sales.xlsx") 
#rev25 <- read_excel("data/revenueData/FY25 Water and Irrigation Sales.xlsx")

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
                    date=dmy(paste0("15-",dmonth,"-",year))))  %>%
    mutate(account=as.integer(ACCOUNT),
           rateCode=BILLCODE,
           description=DESCRIPTION,
           total=TOTAL,
           month=convertDateToInteger(year,month(date))) %>%
    select(-ACCOUNT,-BILLCODE,-DESCRIPTION,-TOTAL) %>%
    select(account,rateCode,month,revenue,date,dmonth,year,description,total)

rev <- waterResid %>%
    left_join(rates,by="rateCode") %>%
    mutate(billAmount=applyWaterRate(consumption,baseFee,t1,t2,t3,t4),
           predBillAmount=applyWaterRate(predUsage,baseFee,t1,t2,t3,t4))

dateStrings <- rev %>%
    mutate(billMonth=convertDateToInteger(year(billDate),month(billDate))) %>%
    group_by(billMonth) %>%
    dplyr::summarize(month=month(first(billDate)),
                     year=year(first(billDate)),
                     dateString=paste0(month, "/", year)) %>%
    select(billMonth, dateString)

breaks=seq(0,75,3)

revPlot <- rev %>%
##    filter(accountType=="Residential/Single Family") %>%
    mutate(billMonth=convertDateToInteger(year(billDate),month(billDate))) %>%
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
                       labels=dateStrings$dateString[breaks+6],
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

