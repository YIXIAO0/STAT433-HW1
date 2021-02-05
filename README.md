STAT433-HW1-README
================
Yi Xiao
2/2/2021

``` r
library(readr)
library(MASS)
library(rgenoud)
```

    ## ##  rgenoud (Version 5.8-3.0, Build Date: 2019-01-22)
    ## ##  See http://sekhon.berkeley.edu/rgenoud for additional documentation.
    ## ##  Please cite software as:
    ## ##   Walter Mebane, Jr. and Jasjeet S. Sekhon. 2011.
    ## ##   ``Genetic Optimization Using Derivatives: The rgenoud package for R.''
    ## ##   Journal of Statistical Software, 42(11): 1-26. 
    ## ##

``` r
library(anchors)
```

    ## 
    ## ##  anchors (Version 3.0-8, Build Date: 2014-02-24)
    ## ##  See http://wand.stanford.edu/anchors for additional documentation and support.

``` r
library(ggplot2)
```

**Read all bridges data at 2019**

``` r
allRecords2019 <- read.csv("2019AllRecordsDelimitedAllStates.csv") 
```

**Briefly check the database**

``` r
## head(allRecords2019)
```

From the data description pdf, I find:

Bridge ID: Item 8 - Structure Number

Year Built: Item 27 - Year Built

Fips Code: (Item 1 - State Code,

Item 3 - County Code,

Item 4 - Place Code)

Condition Rating: Item 58 through 62

(Item 58 - Deck,

Item 59 - Superstructure,

Item 60 - Substructure,

Item 61 - Channel and Channel Protection,

Item 62 - Culverts)

**Select Variables**

``` r
BridgeID <- allRecords2019$STRUCTURE_NUMBER_008
FipCodeState <- allRecords2019$STATE_CODE_001
FipCodeCounty <- allRecords2019$COUNTY_CODE_003
FipCodePlace <- allRecords2019$PLACE_CODE_004
YearBuilt <- allRecords2019$YEAR_BUILT_027
CRDeck <- allRecords2019$DECK_COND_058 #CR stands for Condition Rating
CRSuperstructure <- allRecords2019$SUPERSTRUCTURE_COND_059
CRSubstructure <- allRecords2019$SUBSTRUCTURE_COND_060
CRChannel <- allRecords2019$CHANNEL_COND_061
CRCulverts <- allRecords2019$CULVERT_COND_062
```

**Write data into a dataframe**

``` r
Bridges2019 <- data.frame(
  "BridgeID" = BridgeID,
  "FipCodeState" = FipCodeState,
  "FipCodeCounty" = FipCodeCounty,
  "FIpCodePlace" = FipCodePlace,
  "YearBuilt" = YearBuilt,
  "ConRDeck" = CRDeck,
  "ConRSuperstructure" = CRSuperstructure,
  "ConRSubstructure" = CRSubstructure,
  "ConRCulverts" = CRCulverts
)
```

Bridge condition is determined by the lowest rating of National Bridge
Inventory (NBI) condition ratings for conRDeck, conRSuperstructure,
conRSubstructure, conRCulverts. So, I will create a new variable “ConR”,
which is the lowest value of columns conRDeck, conRSuperstructure,
conRSubstructure, if the three values are Na, then we will use the
ConRCulverts as ConR.

First, I change all ‘N’ value to 0, so we can filter the lowest value
faster

``` r
Bridges2019 = replace.value(Bridges2019, c("ConRDeck","ConRSuperstructure","ConRSubstructure","ConRCulverts"), from = 'N', to = as.integer(0), verbose = FALSE)
```

**Set the four variables ConRDeck, ConRSuperstructure, ConRSubstructure,
ConRCulvert to numeric**

``` r
Bridges2019$ConRDeck = as.numeric(Bridges2019$ConRDeck)
Bridges2019$ConRSuperstructure = as.numeric(Bridges2019$ConRSuperstructure)
Bridges2019$ConRSubstructure = as.numeric(Bridges2019$ConRSubstructure)
Bridges2019$ConRCulverts = as.numeric(Bridges2019$ConRCulverts)
```

**Find the lowest value among the ConRDeck, ConRSuperstructure,
ConRSubstructure, ConRCulvert for every bridge**

``` r
for (i in 1:nrow(Bridges2019)){
  if (isTRUE(Bridges2019$ConRCulverts[i] == 0)){
    Bridges2019$ConR[i] <- min(Bridges2019[i, c("ConRDeck", "ConRSuperstructure", "ConRSubstructure")])
  }else{
      Bridges2019$ConR[i] = Bridges2019$ConRCulverts[i]
    }
}
```

Write a csv file of all bridges in 2019, each bridge has its own
BridgeID, FipCode, YearBuilt, Condition Rating (Deck), Condition Rating
(Superstructure), Condition Rating (Substructure), Condition Rating
(Culverts), and ConR (the lowest value among the four variable desribed
just before)

``` r
write.csv(Bridges2019, "BridgeRecords2019.csv")
```

Find all bridges in Wisconsin at 2019, and we have the FipCode of
Wisconsin is 55

``` r
WIBridges <- Bridges2019[Bridges2019$FipCodeState == 55,]
head(WIBridges)
```

    ##               BridgeID FipCodeState FipCodeCounty FIpCodePlace YearBuilt
    ## 433116 00000000000F303           55            51            0      1932
    ## 433117 00000000000F304           55            51            0      1974
    ## 433118 00000000000F310           55           115            0      1948
    ## 433119 00000000000F311           55           115            0      1979
    ## 433120 00000000000F315           55             3            0      1977
    ## 433121 00000000000F317           55             3        59450      1980
    ##        ConRDeck ConRSuperstructure ConRSubstructure ConRCulverts ConR
    ## 433116        4                  5                5            0    4
    ## 433117        5                  5                4            0    4
    ## 433118        5                  5                7            0    5
    ## 433119        5                  7                8            0    5
    ## 433120        5                  5                7            0    5
    ## 433121        7                  8                7            0    7

``` r
WIBridges$ConR = as.numeric(WIBridges$ConR)
summary(WIBridges$ConR)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   0.000   5.000   6.000   6.215   7.000  53.900       1

From the summary, we could see there is a weird ConR value. The
condition rating should be 0 - 10 as indicated in the data description.
(In order to make the following plot clear, I would like to delete the
row with ConR of 53.9)

``` r
WIBridges = WIBridges[-c(4030), ] ## this row had index of 4030
```

``` r
ggplot(WIBridges, aes(ConR)) +
  geom_bar() +
  xlab("Condition Rating") +
  scale_x_binned(limits = c(0, 10), breaks = 0:10)
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

From the data description, we know: If the lowest rating(ConR) &gt;= 7,
the bridge is Good (G). If the lowest rating(ConR) &lt;= 4, the bridge
is Poor (P). If the 4 &lt; lowest rating(ConR) &lt; 7, the bridge is
Fair (F).

Based on the bar plot, we can conclude that most bridges in Wisconsin
are Fair, and relatively less bridges are Poor.

Then, I want to know whether the age of bridge (Year of Built) has
impacts on the Condition Rating of bridges.

``` r
summary(WIBridges$YearBuilt)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##       9    1962    1980    1976    1997    2018       1

``` r
ggplot(WIBridges, aes(x = YearBuilt, y = ConR)) +
  geom_jitter() +
  xlim(1930, 2020) +
  ylim(0, 10)
```

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

From the above plot, we can see the shorter the age of the bridge, the
higher the condition rating it would have.
