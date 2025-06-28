## This file just runs the regression machinery on the water billing data.

getComp <- function(usage, rdate) {

    ## The months of our study period expressed as points on a circle.
    ## 2*pi equals one full year and pi/6 is one month.
    x <- convertDate(rdate) * (pi / 6);
    mmnth <- month(rdate);
    myear <- year(rdate);
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
        model <- nls(y ~ ((amp) * (cos(x + tst) + 1)) + off + (slp * x),
                     data.frame(x=x, y=usage),
                     start=list(amp=0.05*(max(usage)-min(usage)),
                                off=0.5*(max(usage)+min(usage)),
                                tst=0,
                                slp=0),
                     algorithm="port",
                     lower=c(0,0,-pi/2,
                             -(0.0005*(max(usage)-min(usage)))/max(x)),
                     upper=c(10000000,10000000,pi/2,
                     (0.0002*(max(usage)-min(usage)))/max(x)));
###            slp <- 0;
            
        ## Get results as a list...
        outPars <- model$m$getPars();
        ## ... and return them as a single-row data frame.
        data.frame(amp=outPars[["amp"]],
                   off=outPars[["off"]],
                   tst=outPars[["tst"]],
                   slp=outPars[["slp"]],
                   nls=TRUE);
    },
    error=function(e) {

        if (length(usage) > 5) {
            ## The nls didn't work. Try the simpler method.
            outPars <- list("amp"=0, "off"=0, "tst"=0, "slp"=0);
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
                              tst = 0,
                              slp = 0,
                              nls = FALSE));
        } else {
            return(out <- data.frame(amp = NA, 
                                     off = NA,
                                     tst = NA,
                                     slp = NA,
                                     nls = FALSE));
        }            
    });

    out <- cbind(out, data.frame(Nmnths=NMonths));

    return(out);
}

cat("start prelim at", format_ISO8601(now(),tz="UTC"), "\n");

## Create a list of means and maxima for each account/parcel/rate
## combination.
## Updated 5-6-25 with waterTable fields

water.means <- waterTable %>%
  ##    filter(grepl("^J", subdiv)) %>%
  group_by(account, parcel) %>%
  dplyr::summarize(avg=mean(consumption),
                   mx=max(consumption),
                   avgchg=mean(currentTrans),
                   mxchg=max(currentTrans),
                   acctType=first(accountType),
                   revClass=first(revenueClass),
                   service=first(service),
                   subdiv=first(subdivision),
                   latitude=first(latitude),
                   longitude=first(longitude),
                   startMonth=convertDateToInteger(first(readingYear), first(readingMonth)),
                   endMonth=convertDateToInteger(last(readingYear), last(readingMonth)),
                   tmp=getComp(consumption, readingDate),
                   januse=sum(ifelse(readingMonth==1,consumption,0)),
                   jansum=sum(ifelse(readingMonth==1,1,0)),
                   winuse=sum(ifelse((readingMonth==12)|(readingMonth==1)|(readingMonth==2),consumption,0)),
                   winsum=sum(ifelse((readingMonth==12)|(readingMonth==1)|(readingMonth==2),1,0)),
                   taxProfile=first(taxProfile),
                   waterUseSegment=first(waterUseSegment),
                   revenueClass=first(revenueClass),
                   cycle=last(cycle),
                   rateCode=last(rateCode),
                   .groups="drop") %>%
  unpack(tmp) %>%
  mutate(cusID=paste0(account, "-", parcel))


## Now take those model results and paste them back on the original
## data so we can calculate residuals to estimate errors.
water.resid <- waterTable %>%
    mutate(cusID=paste0(account, "-", parcel),
           cdate=(convertDate(readingDate) * (pi/6))) %>%
    right_join(water.means %>%
               select(cusID, avg, mx, amp, off, tst, slp),
               by="cusID") %>%
    mutate(predUsage = amp * (cos(cdate + tst) + 1) + off + (slp * cdate),
           predResidual=(predUsage-consumption)^2);
    
water.means <- water.resid %>%
    group_by(account, parcel) %>%
    dplyr::summarize(cusID=first(cusID),
                     rmsUsage = mean(predResidual)^0.5,
                     .groups="drop") %>%
    select(-account, -parcel) %>%
    right_join(water.means, by="cusID")

cat("end prelim at", format_ISO8601(now(),tz="UTC"), "\n");


