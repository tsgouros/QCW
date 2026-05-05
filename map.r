## TODO: Fix this. There appears to be an issue with me not
## understanding how st_join works. Also some kind of coordinate
## issue, maybe with the st_crs usage? Anyway, the maptiles comes out
## in the right location, but the other parts come out with incorrect
## lat/lon values.


library(sf)
library(tidyverse)
library(tigris)
library(maptiles)
library(tidyterra)

## Ok, I told you there were three steps: 1. get the map data --
## outlines of the spatial regions, 2. Align your data with the
## spatial regions, and 3. map the data. Here they are, with some
## extra stuff thrown in to make it look better.

## Step 1: Read the TAZ outlines in from the "shapefile".
tazShapes <- read_sf("data/shapefiles/QCTAZ2019/QCTAZ2019.shp") %>%
    mutate(TAZ_2019=as.character(TAZ_2019))

## Here are the names of the TAZs. This is occasionally useful to
## know, but is not essential to anything.
tazNames <- c("1940","1583","2851","1585","1944","1952","1589","1582",
              "1590","1588","1941","1950","1586","1959","1943","1579",
              "1931","1580","1591", "2114","1584","1962","2358",
              "2359","1593","1581","1592","2722","2852", "2850",
              "1955","1949","1936","2185","1987","1983","2086","2080",
              "3109", "1594","2474","2077","1937","3200","1989",
              "2031","2195","2476","1587", "1942","2416","1986",
              "2196","2000","2431","2079")

## Step 2: Make some data to look at, and group it by the TAZ regions.
wmAreaData <- waterMeans %>%
    ## Select which users we want to see.
    filter(waterUseSegment=="Residential", parcel != "") %>%
    left_join(prop, by="parcel", relationship="many-to-one") %>%
    ## Do whatever analysis needs to happen as a function of the TAZ.
    group_by(TAZ_2019) %>%
    dplyr::summarize(myMean=mean(log10(max(1,amp)),na.rm=TRUE)) %>%
    mutate(TAZ_2019=as.character(TAZ_2019))

## Step 3, map the data. Note that "tazShapes" is a data frame that is
## also a real geographic object at this point, while "wmAreaData" is
## just a data frame. So we use a left_join to preserve the geographic
## object nature of tazShapes.
niceMap <- tazShapes %>%
    left_join(wmAreaData, by="TAZ_2019") %>%
    ggplot() +
    ## This draws the background map for a bounding box defined by the
    ## tazShapes array.
    geom_spatraster_rgb(data=get_tiles(st_bbox(tazShapes), 
                                       provider="OpenStreetMap",
                                       crop=TRUE)) +
    ## This actually draws the map. "alpha" is computer-graphics-speak
    ## for "transparency, on a scale from zero (invisible) to 1 (opaque).
    geom_sf(aes(fill=myMean),alpha=0.7)

## Step 3a: Same thing, but making the map variable into a categorical
## variable, so it will be easier to see borders in the map.
niceMapCut <- tazShapes %>%
    left_join(wmAreaData, by="TAZ_2019") %>%
    mutate(myMeanCut=cut(log(myMean, base=10), 10)) %>%
    ## Reorder the factor levels so the legend comes out sensibly.
    mutate(myMeanCut=factor(myMeanCut, levels = rev(levels(myMeanCut)))) %>%    
    ggplot() +
    geom_spatraster_rgb(data=get_tiles(st_bbox(tazShapes),
                                       provider="OpenStreetMap",
                                       crop=TRUE)) +
    geom_sf(aes(fill=myMeanCut),alpha=0.7)


