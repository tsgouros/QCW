## This file just runs the regression machinery on the water billing data.

timeShift <- -pi/12;

getComp <- function(usage, mmnth, myear) {

    ## The months of our study period (7/2017 - 6/2022) expressed as
    ## points on a circle.  2*pi equals one full year and pi/6 is one
    ## month.
    x <- convertDateToInteger(myear, mmnth) * (pi / 6);
    ## Let's ignore data after 4/2021, for covid.
    ##usage <- usage[x < (3.75 * (2 * pi))]; 
    ##x <- x[x < (3.75 * (2 * pi))];
    
    NMonths <- length(usage);

    ## cat("startMonth:", min(x), "NMonths:", NMonths, "\n");
    ## print(x);

    outPars <- list();
    ## See if we have enough points to fit our model.  
    out <- tryCatch({
        if (NMonths < 9) stop("not enough months");
        model <- nls(y ~ ((amp) * (cos(x + timeShift) + 1)) + off + (slp * x),
                     data.frame(x=x, y=usage),
                     start=list(amp=0.05*(max(usage)-min(usage)),
                                off=0.5*(max(usage)+min(usage)),
                                slp=0),
                     algorithm="port",
                     lower=c(0,0,-(0.0005*(max(usage)-min(usage)))/max(x)),
                     upper=c(10000000,10000000,
                     (0.0002*(max(usage)-min(usage)))/max(x)));
###            slp <- 0;
            
        ## Get results as a list...
        outPars <- model$m$getPars();
        ## ... and return them as a single-row data frame.
        data.frame(amp=outPars[["amp"]],
                   off=outPars[["off"]],
                   slp=outPars[["slp"]],
                   nls=TRUE);
    },
    error=function(e) {

        if (length(usage) > 5) {
            ## The nls didn't work. Try the simpler method.
            outPars <- list("amp"=0, "off"=0, "slp"=0);
            ## Figure out which months are Dec-Feb. 
            winter <- which((mmnth == 12) | (mmnth == 1) | (mmnth == 2));

            if (length(winter) > 0) {
                winterMean <- mean(usage[winter], na.rm=TRUE);
            } else {
                winterMean <- mean(usage, na.rm=TRUE);
            }

            ## Find a nice max.
            fuzzyMax <- mean(max(usage, na.rm=TRUE),
                             mean(usage[usage >= mean(usage)],
                                  na.rm=TRUE), na.rm=TRUE);

            return(data.frame(amp = 0.5 * (fuzzyMax - winterMean),
                              off = winterMean,
                              slp = 0,
                              nls = FALSE));
        } else {
            return(out <- data.frame(amp = NA, 
                                     off = NA,
                                     slp = NA,
                                     nls = FALSE));
        }            
    });

    ## We also want to pick up averages for the specific winters.
    win1 <- NA; win2 <- NA; win3 <- NA; win4 <- NA;
    winMonths <- which((x > 2.6) & (x < 3.7));
    if (length(winMonths) > 0) win1 <- mean(usage[winMonths]);
    winMonths <- which((x > 2.6 + (2*pi)) & (x < 3.7 + (2*pi)));
    if (length(winMonths) > 0) win2 <- mean(usage[winMonths]);
    ## This is the covid winter (2020-2021)
    winMonths <- which((x > 2.6 + (4*pi)) & (x < 3.7 + (4*pi)));
    if (length(winMonths) > 0) win3 <- mean(usage[winMonths]);
    winMonths <- which((x > 2.6 + (6*pi)) & (x < 3.7 + (6*pi)));
    if (length(winMonths) > 0) win4 <- mean(usage[winMonths]);

    out <- cbind(out, data.frame(win1=win1, win2=win2, win3=win3, win4=win4,
                                 Nmnths=NMonths));

    return(out);
}

system("date")

## timeShifts <- c();
## resids <- c();
## for (timeShift in seq(-pi/2, pi/2, by=pi/24)) {

## 6/29/23 Grab the table of conversions from old account numbers to
## new.  Note that it also has a sewer flag.
conv <- readxl::read_excel("data/Account\ Mapping\ CIS\ update\ v2.xlsx",
                           sheet="Data",
                           range=cell_limits(c(2,1),c(NA,14)),
                           col_names=c("ccustomer","caccount","status","acctType",
                                       "occType","taxProfile","acct","locID",
                                       "irrigation","landscape","rentalMeters",
                                       "sewer","trash","water"),
                           col_types=c(rep("text",8),
                                       rep("logical",6)));

## Attach these values to the big data array
big <- big %>% left_join(conv);


## Create a list of means and maxima for each account/parcel/rate
## combination.
big.means <- big %>%
##    filter(grepl("^J", subdiv)) %>%
    group_by(acct, parcel, locID) %>%
    dplyr::summarize(avg=mean(usage),
                     mx=max(usage),
                     avgchg=mean(charge),
                     mxchg=max(charge),
                     acctAge=first(acctAge),
                     locClass=first(locClass),
                     class=first(class),
                     rate=first(rate),
                     subdiv=first(subdiv),
                     lat.orig=first(lat),
                     lon.orig=first(lon),
                     startMonth=convertDateToInteger(first(myear), first(mmnth)),
                     endMonth=convertDateToInteger(last(myear), last(mmnth)),
                     tmp=getComp(usage, mmnth, myear),
                     januse=sum(ifelse(mmnth==1,usage,0)),
                     jansum=sum(ifelse(mmnth==1,1,0)),
                     winuse=sum(ifelse((mmnth==12)|(mmnth==1)|(mmnth==2),usage,0)),
                     winsum=sum(ifelse((mmnth==12)|(mmnth==1)|(mmnth==2),1,0)),
                     ccustomer=first(ccustomer),
                     caccount=first(caccount),
                     status=first(status),
                     acctType=first(acctType),
                     taxProfile=first(taxProfile),
                     irrigation=first(irrigation),
                     landscape=first(landscape),
                     rentalMeters=first(rentalMeters),
                     sewer=first(sewer),
                     trash=first(trash),
                     water=first(water),
                     .groups="drop") %>%
    unpack(tmp) %>%
    mutate(cusID=paste0(acct, "-", parcel, "-", locID))

## big.means <- big %>%
##     filter(grepl("^J", subdiv)) %>%
##     group_by(acct, parcel, locID) %>%
##     dplyr::summarize(avg=mean(usage),
##                      mx=max(usage),
##                      avgchg=mean(charge),
##                      mxchg=max(charge),
##                      acctAge=first(acctAge),
##                      locClass=first(locClass),
##                      class=first(class),
##                      rate=first(rate),
##                      subdiv=first(subdiv),
##                      lat.orig=first(lat),
##                      lon.orig=first(lon),
##                      startMonth=convertDateToInteger(first(myear), first(mmnth)),
##                      endMonth=convertDateToInteger(last(myear), last(mmnth)),
##                      .groups="drop") %>%
##     mutate(cusID=paste0(acct, "-", parcel, "-", locID));


## nlsOutput <- data.frame(cusID=c(),
##                         amp=c(),
##                         off=c(),
##                         slp=c(),
##                         nls=c(),
##                         win1=c(),
##                         win2=c(),
##                         win3=c());

## for (i in 1:dim(big.means)[1]) {
##     cat(">>>", i, big.means$cusID[i], "\n");
##     tmp <- big %>%
##         filter(acct==big.means$acct[i],
##                parcel==big.means$parcel[i],
##                locID==big.means$locID[i]);
##     nlsOutput <- rbind(nlsOutput,
##                        getComp(big.means$cusID[i], tmp$usage, tmp$mmnth, tmp$myear));
## }

## stop("aha")


## Now take those model results and paste them back on the original
## data so we can calculate residuals to estimate errors.
big.resid <- big %>%
    mutate(cusID=paste0(acct, "-", parcel, "-", locID),
           cdate=(convertDateToInteger(myear, mmnth) * (pi/6))) %>%
    right_join(big.means %>%
               select(cusID, avg, mx, amp, off, slp, win1, win2, win3),
               by="cusID") %>%
    mutate(predUsage = amp * (cos(cdate + timeShift) + 1) + off + (slp * cdate),
           predResidual=(predUsage-usage)^2);
    
    big.means <- big.resid %>%
    group_by(acct, parcel, rate) %>%
    dplyr::summarize(cusID=first(cusID),
                     rmsUsage = mean(predResidual)^0.5,
                     .groups="drop") %>%
    select(-acct, -parcel, -rate) %>%
    right_join(big.means, by="cusID")


## ## Make a report of the residuals.
## cat("For timeShift=", timeShift,
##     ", rmsResiduals=", big.means %>% filter(class=="RESIDENTIAL") %>% select(rmsUsage) %>% sum(na.rm=TRUE), "\n");

##     timeShifts <- c(timeShifts, timeShift);
##     resids <- c(resids, big.means %>% filter(class=="RESIDENTIAL") %>% select(rmsUsage) %>% sum(na.rm=TRUE));

## }
##
## residPlot <- tibble(resids=resids, timeShifts=timeShifts) %>%
##     ggplot(aes(x=timeShifts, y=resids)) +
##     geom_line() +
##     geom_point() +
##     xlab("Time Shift") +
##     ylab("Residual Errors") +
##     geom_vline(aes(xintercept=-pi/6,color="red")) +
##     theme(legend.position="none")



system("date")


