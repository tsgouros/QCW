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

##updated 123-154 on 2-24 to read Pinal data in Excel, set names before running read_excel
#

## The returnMatches argument controls whether the return contains the
## parcels in desiredParcels that were found in the county data, or a
## list of the parcels that were *not* found there.
read.pinal.data <- function(fileName, desiredParcels, returnMatches=TRUE) {

    cnames <- c("acct", "parcel", "parcelid", "accttype",
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
            "section", "township", "range", "status", "pool")
    
    cclasses <- c("text", "text", "text", "text",
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
                  "text")

    test <- read_excel(fileName, skip=1, col_names=cnames, col_types=cclasses) %>%
        filter(!is.na(acct));

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

    out <- tibble(parcel=test$parcel,
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
                                    paste(ifelse(is.na(test$propertystreetno),"",
                                                 test$propertystreetno),
                                          ifelse(is.na(test$propertypredirection),"",
                                                 test$propertypredirection),
                                          ifelse(is.na(test$propertystreetname),"",
                                                 test$propertystreetname),
                                          ifelse(is.na(test$propertystreettype),"",
                                                 test$propertystreettype),
                                          ifelse(is.na(test$propertypostdirection),"",
                                                 test$propertypostdirection),
                                          ifelse(is.na(test$propertyunitname),"",
                                                 test$propertyunitname),
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
                  );

    ## Figure out whether to return the matches or the mismatches.
    if (returnMatches) {
        out <- out %>% filter(parcel %in% desiredParcels);
        cat("Found ", dim(out)[1], " out of ", length(desiredParcels), "requested.\n")
        return(out);
    } else {
        return(tibble(requestedParcels=desiredParcels[!desiredParcels %in% out$parcel]));
    }   
}

pinal <- read.pinal.data("data/countyData/pinal/Town of Queen Creek pools 2025 03.06.25.xlsx",
                         unique((addressTable %>% filter(taxProfile==4|taxProfile==5))$parcel),
                         returnMatches=TRUE)

cat("Finished reading Pinal County data.\n");

## Given a list of parcel numbers, returns something that looks like
## the pinal data for the given parcels. If returnMatches is set to
## TRUE, the function will return a list of the parcels that were
## found in the data files (this is the default). If returnMatches is
## FALSE, the function will return the parcels for which there was no
## county data.  Note also that the data files should really be an
## input here, but they are complicated, so the file names are
## hard-coded in.
read.maricopa.data <- function(dataDir, desiredParcels, returnMatches=TRUE) {

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

    for (filename in c(paste0(dataDir, "/QUEE_42061CT_01.dat"),
                       paste0(dataDir, "/MESA_42061CT_01.dat"),
                       paste0(dataDir, "/MESA_42061CT_02.dat"),
                       paste0(dataDir, "/CNTY_42061CT_01.dat")) ){
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

    compress <- parcels %in% desiredParcels;
    cat("From ", filename, ", \nkeeping ", sum(compress), " out of ",
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
    
    for (filename in c(paste0(dataDir, "/QUEE_42032ct.dat"),
                       paste0(dataDir, "/MESA_42032ct.dat"),
                       paste0(dataDir, "/CNTY_42032ct.dat")) ) {
        
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


    ## Figure out whether to return the matches or the mismatches.
    if (returnMatches) {
        out <- out %>% filter(parcel %in% desiredParcels);
        cat("Found ", dim(out)[1], " out of ", length(desiredParcels), "requested.\n")
        return(out);
    } else {
        return(tibble(requestedParcels=desiredParcels[!desiredParcels %in% out$parcel]));
    }   
}

mari <- read.maricopa.data("data/countyData/maricopa",
                           unique((addressTable %>% filter(taxProfile==1|taxProfile==3))$parcel),
                           returnMatches=TRUE)

cat("Finished reading Maricopa County data.\n");

## Show that the two sets of data have been successfully conformed
## to one another. These colname lists should be the same.
##print(colnames(pinal));
##print(colnames(mari));

## And since they are they same, they can be combined into one big
## data table.
prop <- rbind(pinal, mari);


##import r116 residential files from Maricopa County Assessor. Complete files are in the OWN version 
##these three data files are not in the same format so the select function after each read makes the data frame consistent

residentialQC <- read.delim("data/countyData/maricopa/R116_QU_OWN_2025.txt", header= FALSE, sep ="|", dec = ".",
                            col.names=c("countyID", "parcel", "proportionComplete", "class", "storyCount", "airConType", 
                                        "heatingType", "bathroomFixtures", "extWallMaterial", "roofMaterial", "roofStyle",
                                        "constructionYear", "livingSqft", "1stFloor", "2ndFloor", "3rdFloor",
                                        "basement", "parkCode", "patios", "poolArea", "salePrice", "saleDate",
                                        "addedArea", "detachArea", "puc", "ownersName", "ownersAddr1", "ownersAddr2",
                                        "ownerCity", "ownersState", "ownersZipCode", "ownerCountry", "streetNum",
                                        "streetDir1", "streetName1", "streetType1", "streetPostDir1", "suite1", "city1", "zip")) %>%
                              select(-countyID) %>%
                              as_tibble();


residentialMesa <- read.delim("data/countyData/maricopa/R116_ME_OWN_2025.txt", header= FALSE, sep ="|", dec = ".",
                              col.names=c("countyID", "parcel", "proportionComplete", "class", "storyCount", "airConType", 
                                          "heatingType", "bathroomFixtures", "extWallMaterial", "roofMaterial", "roofStyle",
                                          "constructionYear", "livingSqft", "1stFloor", "2ndFloor", "3rdFloor",
                                          "basement", "parkCode", "patios", "poolArea", "salePrice", "saleDate",
                                          "addedArea", "detachArea", "puc", "ownersName", "ownersAddr1", "ownersAddr2",
                                          "ownerCity", "ownersState", "ownersZipCode", "ownerCountry", "streetNum",
                                          "streetDir1", "streetName1", "streetType1", "streetPostDir1", "suite1", "city1", "zip")) %>%
                              select(-countyID) %>%
                              as_tibble();
                              

residentialCounty <- read.delim("data/countyData/maricopa/R116_OWN_2025.txt", header= FALSE, sep ="|", dec = ".",
                                col.names=c("parcel", "proportionComplete", "class", "storyCount", "airConType", 
                                            "heatingType", "bathroomFixtures", "extWallMaterial", "roofMaterial", "roofStyle",
                                            "constructionYear", "livingSqft", "1stFloor", "2ndFloor", "3rdFloor",
                                            "basement", "parkCode", "patios", "poolArea", "salePrice", "saleDate",
                                            "addedArea", "detachArea", "puc", "ownersName", "ownersAddr1", "ownersAddr2",
                                            "ownerCity", "ownersState", "ownersZipCode", "ownerCountry", "streetNum",
                                            "streetDir1", "streetName1", "streetType1", "streetPostDir1", "suite1", "city1", "zip",
                                            "na1", "na2", "na3", "na4")) %>%
                              select(-na1,-na2,-na3,-na4) %>%
                              as_tibble();


residential <- rbind(residentialQC,
                     residentialMesa,
                     residentialCounty)

##import r117 commercial data ##


commercial <- read.delim("data/countyData/maricopa/R117_2025.txt", header= FALSE, sep ="|", quote = "\"", dec = ".",
                         col.names=c("countyID", "parcel", "improvementID", "occNum", "occRank", "structClass",
                                     "occDescr", "improvementFcv", "stories", "wallHeight", "gfPermeter",
                                     "gfArea", "floorArea", "constructionYear", "pctComplete", "pctObsolete",
                                     "pctModern", "pctPhysical", "pctOwnership", "subMktAdj"));

## 1770 is the premium secured master data to use. QC area parcels are in tax roll 6 in the 2025 file ##


r1770 <- read.delim("data/countyData/maricopa/R1170_SecMaster_2025_TAX_ROLL_6.txt", header= FALSE, sep ="|", dec = ".",
                    col.names=c("parcel", "propertyStatus", "propertyUseCode", "taxAreaCode", "orgExemptionCode",
                                "orgExemptIndicator", "section", "town", "range", "sectionQuarter", "lot", "block",
                                "tract", "streetNum", "streetDir","streetName", "streetType", "streetPostDir", "suite",
                                "city", "zip", "ownersName", "ownersDeed", "ownersDeedDate", "feesName", "feesDeed",
                                "feesDeedDate","ownersAddr1stLine","ownersAddr2ndLine","ownersCity","ownersState",
                                "ownersZipCode", "ownerCountry", "lpvPercent", "lpvAmount", "lpvAssessedValue", "fcv",
                                "landAssessmentPerc", "landValue", "imprAssessmentPerc", "impValue", "widvetIndicator",
                                "widvetlpvexAssessedValue", "widvetfcvxAssessedValue", "sqFootage", "valSource",
                                "overrideCode", "landClass1st", "landRatio1st", "land2nd", "landRatio2nd", "land3rd", 
                                "landRatio3rd", "land4th","landRatio4th", "improvementClass1st","improvedmentRatio1st",
                                "improvement2nd", "improvementRatio2nd", "improvement3rd", "improvementRatio3rd",
                                "improvement4th", "improvementRatio4th", "landAreaType", "neighborhoodCode",
                                "marketAreaCode", "mcr","subdivision", "mailingDate", "economicUnit", "numberofUnits", 
                                "districtCode1", "districtCodeValue1", "districtCode2", "districtCodeValue2", 
                                "districtCode3", "disctrictCodeValue3", "districtCode4", "districtCodeValue4", 
                                "districtCode5", "districtCodeValue5", "districtCode6", "districtCodeValue6", "lglDesc", "na"));

merged <- merge(r1770,residential, by = "parcel")

maricopa <- tibble(parcel=merged$parcel,
                   type=merged$propertyUseCode,
                   impvalue=merged$impValue,
                   landvalue=merged$landValue,
                   fcv=merged$fcv,
                   totalsf=merged$sqFootage,
                   impclass=merged$improvementClass1st,
                   landclass=merged$landClass1st,
                   area=merged$sqFootage,
                   address=merged$streetName,
                   section=merged$section,
                   township=merged$town,
                   range=merged$range,
                   ownercity=merged$ownersCity,
                   propertycity=merged$city,
                   county="maricopa",
                   poolarea=merged$poolArea,
                   pool=ifelse(is.na(merged$poolArea),FALSE,TRUE),
                   yearbuilt=merged$constructionYear
);
