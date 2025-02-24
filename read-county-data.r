library(anytime)
library(tidyverse)
library(lubridate)
library(readxl)

## From varhandle package
## Function Description:
##     A function to assess if a vector can be interpreted as numbers

check.numeric <- function(v = NULL, na.rm = FALSE, only.integer = FALSE,
                          exceptions=c(""), ignore.whitespace = TRUE){
    #----[ checking the input ]----#
    {
        # if the only.integer is NOT a single TRUE or FALSE
        if (!is.logical(only.integer) | length(only.integer) != 1) {
            # complain
            stop("The parameter \"only.integer\" should be either TRUE or FALSE.")
        }

        # if user has not defined the vector v
        if (is.null(v)) {
          # complain
            stop("The parameter \"v\" is not defined. It can be character vector, numeric vector, factor vector or logical vector.")
        # if user has defined but the class is NOT character or factor
        }else if (!inherits(v, c("character", "factor"))) {
            # if the class is NOT numeric or integer either
            if (!inherits(v, c("numeric", "integer", "logical"))) {
                # complain
                stop("The parameter \"v\" can only be a character vector, numeric vector, factor vector or logical vector.")
            # if the class is numeric or integer
            }else{
                # if user wants to specifically filter out non-integers, there
                # is a chance that the vector contains some non-integer numbers
                # so we should turn the vector to character and run the function
                if(only.integer){
                    # convert the vector to character
                    v <- as.character(v)
                }else{
                    # since it is already a number
                    return(rep(x = TRUE, length(v)))
                }
            }
        }

        # if the na.rm is NOT a single TRUE or FALSE
        if (!is.logical(na.rm) | length(na.rm) != 1) {
            # complain
            stop("The parameter \"na.rm\" should be either TRUE or FALSE.")
        }



        # if the ignore.whitespace is NOT a single TRUE or FALSE
        if (!is.logical(ignore.whitespace) | length(ignore.whitespace) != 1) {
            # complain
            stop("The parameter \"ignore.whitespace\" should be either TRUE or FALSE.")
        }
    }


    #----[ pre-processing ]----#
    {
        # convert to character if it is vector
        if (inherits(v, "factor")) {
            # convert to character
            v <- as.character(v)
        }

        # if user wants to ignore NAs
        if (na.rm) {
            # if it has some NAs
            if (any(is.na(v))) {
                # remove NAs
                v <- v[-pin.na(v)]
            }
        }

        # if user wants to ignore leading or tailing white space
        if (ignore.whitespace) {
            # substitute whitespaces in the begining and at the ending
            # of each item in v
            v <- gsub("^\\s+|\\s+$", "", v)
        }
    }


    #----[ processing ]----#
    {
        # if user wants to only detect integers
        if (only.integer) {
            regexp_pattern <- "(^(-|\\+)?\\d+$)|(^(-|\\+)?(\\d*)e(-|\\+)?(\\d+)$)"
        # if user wants to detect all numbers
        }else{
            #regexp_pattern <- "^(-|\\+)?\\d+(\\.?\\d+)?$"
            regexp_pattern <- "(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))$)|(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))e(-|\\+)?(\\d+)$)"
        }

        # perform the regexp
        output <- grepl(pattern = regexp_pattern, x = v)

        # check for existance of exceptions
        exception_index <- is.element(v, exceptions)
        # if there are is exception detected
        if (any(exception_index)) {
            # turn their output value to TRUE
            output[exception_index] <- TRUE
        }

        # if user wants to keep NA
        if (!na.rm) {
            # NAs are marked as FALSE by grepl and we replace it with TRUE instead
            output[is.na(v)] <- TRUE
        }


        # return the result
        return(output)
    }
}

##updated 123-154 on 2-24 to read Pinal data in Excel, unsure about row156 test

read.pinal.data <- read_excel("data/countyData/Town of Queen Creek pools 2025 01.23.25.xlsx", skip=1,

      col_names=c("acct", "parcel", "parcelid", "accttype",
                  "propertytype", "impconditiontype", "impquality",
                  "implegalclass", "primarybltasdescription", "yearbuilt",
                  "physicalage", "adjustedyearbuilt", "effectiveage",
                  "totalimpsf", "residentialsf", "commercialsf", "condoimpsf",
                  "landlegalclass", "landnetacrecount", "taxareacode", "fcv",
                  "impvalue", "landvalue", "receptionno", "saledate", "saleprice",
                  "ownername1", "ownername2", "co", "mailingaddress", "ownercity",
                  "ownerstatecode", "ownerzipcode", "ownerprovince",
                  "ownerpostalcode", "ownercountry", "propertystreetno",
                  "propertypredirection", "propertystreetname",
                  "propertystreettype", "propertypostdirection",
                  "propertyunitname", "propertycity", "propertyzipcode",
                  "section", "township", "range", "status", "pool"),

      col_types=c("text", "text", "text", "text",
                  "text","text","text",
                  "text","text","numeric",
                  "numeric","numeric","numeric",
                  "numeric","numeric","numeric","numeric",
                  "text","numeric","text","numeric",
                  "numeric","numeric","text","text","numeric",
                  "text","text","text","text","text",
                  "text","text","text",
                  "text","text","text",
                  "text","text",
                  "text","text",
                  "text","text","text",
                  "text","text","text","text",
                  "text"))
    
 test <- read_excel("data/countyData/Town of Queen Creek pools 2025 01.23.25.xlsx", header=TRUE,
                     col.names=cnames, colClasses=cclasses);

    test$county <- rep("pinal", length(test$parcel));

    convert.strings <- function(ins) {
        outs <- c();
        for (s in ins) {
            outs <- c(outs, ifelse(is.na(as.numeric(s)),s,
                                   as.character(as.numeric(s))));
        }
        return(factor(outs, levels=c("3", "4", "2", "1", "9", "5", "")));
    }

    ## Convert pool data to logical.
    test$pool <- test$pool == "Yes";


    ## Maricopa has pool area data, so add a dummy variable to match it.
    test$poolarea <- rep(NA, length(test$parcel));
    
    return(tibble(parcel=test$parcel,
                  type=test$propertytype,
                  ##condition=test$impconditiontype,
                  impvalue=test$impvalue,
                  landvalue=test$landvalue,
                  fcv=test$fcv,
                  totalsf=ifelse(is.na(test$totalimpsf),
                                 0, as.numeric(test$totalimpsf)),
                  impclass=convert.strings(substring(test$implegalclass,1,2)),
                  landclass=convert.strings(substring(test$landlegalclass,1,2)),
                  area=test$landnetacrecount,
                  address=gsub("^\\s+|\\s+$", "",
                               gsub("  ", " ",
                                    paste(test$propertystreetno,
                                          test$propertypredirection,
                                          test$propertystreetname,
                                          test$propertystreettype,
                                          test$propertypostdirection,
                                          test$propertyunitname,
                                          sep=" "))),
                  section=factor(sapply(test$section,
                                        function(x) { if (nchar(x)==1)
                                                          paste0("0",x,sep="")
                                                      else x},
                                        simplify=TRUE)),
                  township=factor(gsub("^0", "", test$township),
                                  levels=c("2S", "3S", "1N")),
                  range=(gsub("^0", "", test$range)),
                  ownercity=test$ownercity,
                  propertycity=test$propertycity,
                  county=factor(test$county, levels=c("maricopa", "pinal")),
                  poolarea=test$poolarea,
                  pool=test$pool,
                  yearbuilt=test$yearbuilt
                  ));
}

pinal <- read.pinal.data("data/pinal/Account-Inventory-with-Land-Values-and-Pools-from-Imported-2020-List-Queen-Creek-Water-Board-5-24-2022.csv");
pinal.new <- read.pinal.data("data/pinal/pinal-new-parcel-data.csv");

pinal <- rbind(pinal, pinal.new);
rm(pinal.new);


## Given a list of parcel numbers, returns something that looks like
## the pinal data, but only for the given parcels.
read.maricopa.data <- function(desiredparcels) {

    # returns string w/o leading or trailing whitespace
    trim <- function(str) gsub("^\\s+|\\s+$", "", str);
    trimz <- function(str) gsub("^0", "", trim(str));
    trimdub <- function(str) gsub("  ", " ", str);

    parcels <- c();
    types <- c();
    limcashvalues <- c();
    landcashvalues <- c();
    impcashvalues <- c();
    acres <- c();
    totalsf <- c();
    addresses <- c();
    sections <- c();
    townships <- c();
    ranges <- c();
    ownercities <- c();
    propertycities <- c();

    landclasses <- c();
    impclasses <- c();

    ## Legal classes in Maricopa can be fractional.  This returns the
    ## majority class of the four listed.  The problem is that it's
    ## not always the biggest share that's listed first.
    ## Unfortunately, as of rcs1.3, this does not work very well,
    ## though I'm not sure why not.
    pick.class <- function(instring) {

        class <- c(); frac <- c();

        class[1] <- trim(substr(instring, start=1, stop=2));
        if (class[1] == "") return("");

        frac[1] <- as.numeric(substr(instring, start=3, stop=5));
        class[2] <- trim(substr(instring, start=6, stop=7));
        frac[2] <- as.numeric(substr(instring, start=8, stop=10));
        class[3] <- trim(substr(instring, start=11, stop=12));
        frac[3] <- as.numeric(substr(instring, start=13, stop=15));
        class[4] <- trim(substr(instring, start=16, stop=17));
        frac[4] <- as.numeric(substr(instring, start=18, stop=20));

        return(class[ which(frac == max(frac)) ]);
    }

    for (filename in c("data/maricopa/QUEE_42061CT_01.dat",
                       "data/maricopa/MESA_42061CT_01.dat",
                       "data/maricopa/MESA_42061CT_02.dat",
                       "data/maricopa/CNTY_42061CT_01.dat")) {
        ## Read the file as line images.
        src <- read.delim(filename, head=F, sep="|", stringsAsFactors=FALSE)

        for (record in src) {
            parcels <- c(parcels, trim(substr(record, start=4, stop=12)));

            types <- c(types, trim(substr(record, start=14, stop=17)));

            limcashvalues <- c(limcashvalues,
                               as.numeric(substr(record, start=432, stop=443)));
            landcashvalues <- c(landcashvalues,
                                as.numeric(substr(record, start=459, stop=470)));
            impcashvalues <- c(impcashvalues,
                               as.numeric(substr(record, start=486, stop=497)));
            acres <- c(acres, as.numeric(substr(record, start=535, stop=546)));

            rec <- substr(record, start=603, stop=617);
            totalsf <-
                c(totalsf,
                  ifelse((check.numeric(rec) & !grepl("^ *$", rec)), rec, 0));


            address <-
                trim(trimdub(paste(trim(substr(record, start=52, stop=57)),
                                   trim(substr(record, start=58, stop=59)),
                                   trim(substr(record, start=60, stop=99)),
                                   trim(substr(record, start=100, stop=111)),
                                   trim(substr(record, start=112, stop=121)),
                                   trim(substr(record, start=122, stop=125)),
                                   trim(substr(record, start=126, stop=132)),
                                   sep=" ")));
            addresses <- c(addresses, address);

            sections <- c(sections,
                          gsub("\\\\", "0",  ## Changes \9 to 09
                               trim(substr(record, start=27, stop=28))));
            townships <- c(townships, trimz(substr(record, start=29, stop=31)));
            ranges <- c(ranges, trimz(substr(record, start=32, stop=34)));

            ownercities <- c(ownercities, trim(substr(record, start=378, stop=417)));
            propertycities <- c(propertycities,
                                trim(substr(record, start=133, stop=162)));

            landclasses <- c(landclasses, trim(substr(record, start=563, stop=564)));
            impclasses <- c(impclasses, trim(substr(record, start=583, stop=584)));
##            landclasses <- c(landclasses, pick.class(substr(record, start=563, stop=582)));
##            impclasses <- c(impclasses, pick.class(substr(record, start=583, stop=602)));
        }
    }

    compress <- parcels %in% desiredparcels;
    cat("From ", filename, ", keeping ", sum(compress), " out of ",
        length(compress), " parcels.\n", sep="");
    county <- rep("maricopa", sum(compress));

    out <- tibble(parcel=parcels[compress],
                  type=types[compress],
                  impvalue=limcashvalues[compress]+impcashvalues[compress],
                  landvalue=landcashvalues[compress],
                  fcv=limcashvalues[compress]+impcashvalues[compress]+landcashvalues[compress],
                  ## Data is in 1/1000 s.f. !?  convert to acres.
                  totalsf=as.numeric(totalsf[compress]),
                  impclass=factor(impclasses[compress],
                                  levels=c("3", "4", "2", "1", "9", "5", "")),
                  landclass=factor(landclasses[compress],
                                  levels=c("3", "4", "2", "1", "9", "5", "")),
                  area=acres[compress]/(1000 * 43560.04),
                  address=addresses[compress],
                  section=factor(sapply(sections[compress],
                                        function(x) { if (nchar(x)==1)
                                                          paste0("0",x,sep="")
                                                      else x},
                                        simplify=TRUE)),
                  township=factor(townships[compress],
                                  levels=c("2S", "3S", "1N")),
                  range=ranges[compress],
                  ownercity=ownercities[compress],
                  propertycity=propertycities[compress],
                  county=factor(county, levels=c("maricopa", "pinal"))
                  );

    newparcels <- c();
    yearbuilts <- c();
    poolarea <- c();
    
    for (filename in c("data/maricopa/QUEE_42032ct.dat",
                       "data/maricopa/MESA_42032ct.dat",
                       "data/maricopa/CNTY_42032ct.dat")) {

        src <- read.delim(filename, head=F, sep="|", stringsAsFactors=FALSE);

        for (record in src) {
            newparcels <- c(newparcels, trim(substr(record, start=1, stop=12)));
            yearbuilts <-
                c(yearbuilts,
                  as.numeric(trim(substr(record, start=42, stop=45))));
            poolarea  <- c(poolarea, 
                  as.numeric(trim(substr(record, start=49, stop=53))));
        }
    }

    out <- out %>%
        left_join(tibble(parcel=newparcels,
                         poolarea=poolarea,
                         pool=!is.na(poolarea),
                         yearbuilt=yearbuilts),
                  by="parcel")

    return(out);
}

mari <- read.maricopa.data(unique(big$parcel));

print(colnames(pinal));
print(colnames(mari));

prop <- rbind(pinal, mari);


