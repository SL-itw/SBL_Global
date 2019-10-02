SBL\_analysis
================
Steven Lawrence
September 13, 2019

Data Retrival and Manipulation
==============================

Here I am installing all libraries neccessary for analysis

``` r
install.packages(c("tidyverse","ggplot2","tseries","DataCombine","forecast","dplyr","knitr","pastecs","grid","formattable","gridExtra","viridis","viewpoint"))
```

Loading Libraries

``` r
library(tidyverse)
library(ggplot2)
library(tseries)
library(DataCombine)
library(forecast)
library(dplyr)
library(knitr)
library(pastecs)
library(grid)
library(formattable)
library(gridExtra)
library(viridis)
library(magrittr)
library(qwraps2)
```

Geographic Analysis
-------------------

Temporal Trend
--------------

Both temporal datasets were retrieve from trends.google.com via csv files and are depicted below showing the head and tail of each.

Head and Tail of Skin Bleaching temporal trend dataset

    ##      Time RSV
    ## 2 2004-01  11
    ## 3 2004-02  22
    ## 4 2004-03  39
    ## 5 2004-04  43
    ## 6 2004-05  46
    ## 7 2004-06  42

    ##        Time RSV
    ## 182 2019-01  42
    ## 183 2019-02  39
    ## 184 2019-03  73
    ## 185 2019-04  54
    ## 186 2019-05  44
    ## 187 2019-06  56

Head and tail of skin lightening temporal trend dataset

    ##      Time RSV
    ## 2 2004-01  15
    ## 3 2004-02  23
    ## 4 2004-03  15
    ## 5 2004-04  37
    ## 6 2004-05  39
    ## 7 2004-06  40

    ##        Time RSV
    ## 182 2019-01  64
    ## 183 2019-02  72
    ## 184 2019-03  82
    ## 185 2019-04  78
    ## 186 2019-05  78
    ## 187 2019-06  85

Here I merged the two temporal datasets to create a new variable that is the average of the two. The head and tail is shown below.

``` r
## merging bleaching and lightening data temporal data sets relative search volumes by time##

SKBLT<- merge(skBT, skLT, by = "Time", sort = T)

colnames(SKBLT)<- c("Date","Bleaching", "Lightening")

#creating an average variable of bleaching and lightening temporal RSV's

SKBLT$aveRSV<- (SKBLT$Bleaching+SKBLT$Lightening)/2
#creating a subset of SKBLT only time and average RSV 
SKBLT.p<- SKBLT[,c(1,4)]
head(SKBLT.p)
```

    ##      Date aveRSV
    ## 1 2004-01   13.0
    ## 2 2004-02   22.5
    ## 3 2004-03   27.0
    ## 4 2004-04   40.0
    ## 5 2004-05   42.5
    ## 6 2004-06   41.0

``` r
tail(SKBLT.p)
```

    ##        Date aveRSV
    ## 181 2019-01   53.0
    ## 182 2019-02   55.5
    ## 183 2019-03   77.5
    ## 184 2019-04   66.0
    ## 185 2019-05   61.0
    ## 186 2019-06   70.5

Using the tseries and forecast package we analyazed the temporal trend.

``` r
#creating a time series object in order to plot the temporal trend as a time series element
SKBLT.ts<- ts(SKBLT.p$aveRSV)
```

This plot is SBL queries by month in the United States from 2004 - 2019.

``` r
#plotting sbl quries over time with a resolution of months 
plot(SKBLT.ts , type = "o", main = "SKin Bleaching and Lightening Trend from 2004 To 2019", xlab = "Months", ylab = "Relative Search Volume")
```

![](SBL_Analysis_files/figure-markdown_github/temporal%20trend%20plot-1.png) Here I provide some discriptive statistics of the overall temporal trend.

Then data is not normally distributed there for I recorded the median and IQR `median_iqr(SKBLT.p$aveRSV)`

``` r
hist(SKBLT.p$aveRSV, n = 40)
```

![](SBL_Analysis_files/figure-markdown_github/Discriptibe%20Statisics-1.png)

``` r
mean_skbl<-mean(SKBLT.p$aveRSV)
mean_skbl
```

    ## [1] 45.77151

SKBLT.pc&lt;-PercChange(SKBLT.p, Var = "aveRSV", slideBy = -1) SKBLT.pc summary(SKBLT.pc$`aveRSV_PChangeFrom-1`) \# Min. 1st Qu. Median Mean 3rd Qu. Max. NA's \#-49.689 -9.184 1.449 2.661 9.630 111.268 1

SKBLT.p9&lt;- SKBLT.p\[-c(1:60),\] SKBLT.pc9&lt;-PercChange(SKBLT.p9, Var = "aveRSV", slideBy = -1) SKBLT.pc9\[is.na(SKBLT.pc9)\]&lt;- 0 summary(SKBLT.pc9$`aveRSV_PChangeFrom-1`) \# Min. 1st Qu. Median Mean 3rd Qu. Max. \#-49.6894 -7.1545 0.8283 1.9474 8.7086 111.2676 \#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

Checking seasonality
====================

count\_ma&lt;- ts(na.omit(SKBLT.p$aveRSF), frequency = 12) decomp&lt;-stl(count\_ma, s.window = "periodic") plot(decomp) decomp&lt;- as.ts(decomp) decomp deseason&lt;- seasadj(decomp)

install.packages("forecast") library(forecast) autoplot(decomp) ggseasonplot(deseason, year.lables = T, continuous = T, polar = T)

######################### 

Creating a data frame of the non white population which will be inverted values of the white population scores.

``` r
Region<- c("Alabama", "Alaska","Arizona", "Arkansas", "California", "Colorado","Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", " Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada","New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Puerto Rico", "Rhode Island", "South Carolina", " South Dakota", "Tennessee", "Texas", "Utah", "Vermont", " Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

wonly<- c(65.4,60.3,54.4,72.2,36.8,67.9,66.5,61.9,37.1,53.5,52.4,21.8,81.7,61.0,78.9,85.3,75.7,84.3, 58.6,93.1,50.5,71.4,74.9,79.5,56.5, 79.3,85.9,78.6,48.7,90.0,54.9,37.1,55.4,62.8,84.0,78.7,65.3,75.3,76.1,0.8,72.0,63.7,81.4,73.7,41.5,78,92.5,61.5,68.0,92.1,81.1,83.8 )

woPop<- data.frame(Region,wonly)
woPop %>% mutate_if(is.factor, as.character) -> woPop
```

skblr.naomit

skblmmp&lt;- merge(woPop, skblr.naomit, by ="Region", all = T ) skblmmp

skblmmp.naomit&lt;- na.omit(skblmmp)

skblmmp.naomit&lt;- skblmmp.naomit\[order(-skblmmp.naomit$aveRSF),\]

skblmmp.naomit*n**w**p* &lt; −100 − *s**k**b**l**m**m**p*.*n**a**o**m**i**t*wonly

skblmmp.naomit&lt;- skblmmp.naomit\[order(-skblmmp.naomit$nwp),\] skblmmp.naomit

skblm1&lt;- as\_tibble(skblmmp.naomit) skblm1&lt;- skblm1%&gt;%dplyr::mutate(region=stringr::str\_to\_lower(Region)) sMap&lt;- ggplot2::map\_data("state") \#skblmm = skin bleaching lightening merge map skblmm1&lt;- merge(sMap, skblm1, by = 'region') skblmm1&lt;- skblmm1%&gt;% dplyr::left\_join(x=., y = sMap, by ='region') skblmm1 skblmm1&lt;- skblmm1\[,-c(7,)\] colnamesskblmm1

skblmm1%&gt;% ggplot(aes(x=long, y = lat.y))+ geom\_polygon(aes(group = group.y, fill = log(nwp)), colour = 'white')+theme2()+ggtitle("Skin Bleaching and Skin Lightening by Region")

Extracting Census data
