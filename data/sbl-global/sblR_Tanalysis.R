######################################################
# Sbl - global (US) temporal and  regional analysis  #
#                                                    #
# Steven Lawrence                                    #
# Cleaned code 09.06.19                              #
######################################################

#####################
### Pachages used ###
#####################

library(tseries)
library(DataCombine)

#####################

### Loading Skin bleaching and skin lightening temporal data sets ########################################################################################

skBT<- read.csv("H:/Personal/DesktopFiles/GtrendData/SkinBleachingTime0419.csv", header = T, sep = ",",stringsAsFactors = F, col.names = c("Time","RSV"))
skBT<- skBT[-1,]
skBT$RSV<- as.numeric(skBT$RSV)

skLT<- read.csv("H:/Personal/DesktopFiles/GtrendData/SkinLighteningTime0419.csv", header = T, sep = ",", stringsAsFactors = F, col.names = c("Time","RSV"))
skLT<- skLT[-1,]
skLT$RSV<- as.numeric(skLT$RSV)

## merging bleaching and lightening data temporal data sets relative search volumes by time##

SKBLT<- merge(skBT, skLT, by = "Time", sort = T)

colnames(SKBLT)<- c("Date","Bleaching", "Lightening")

#creating an average variable of bleaching and lightening temporal RSV's

SKBLT$aveRSV<- (SKBLT$Bleaching+SKBLT$Lightening)/2
head(SKBLT)
#creating a subset of SKBLT only time and average RSV 
SKBLT.p<- SKBLT[,c(1,4)]
head(SKBLT.p)


###########################
# Temporal trend analysis #
#################################################################################################################################

#creating a time series object in order to plot the temporal trend as a time series element
SKBLT.ts<- ts(SKBLT.p$aveRSV)

#plotting sbl quries over time with a resolution of months 
plot(SKBLT.ts , type = "o", main = "SKin Bleaching and Lightening Trend from 2004 To 2019", xlab = "Months: 0 = 2004-01, 186 = 2019-06", ylab = "Relative Search Popularity")+plot(SKBLT.ts, type = "o")

### calculating percent change over time. ###


SKBLT.pc<-PercChange(SKBLT.p, Var = "aveRSV", slideBy = -1)
SKBLT.pc
summary(SKBLT.pc$`aveRSV_PChangeFrom-1`)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-49.689  -9.184   1.449   2.661   9.630 111.268       1 

SKBLT.p9<- SKBLT.p[-c(1:60),]
SKBLT.pc9<-PercChange(SKBLT.p9, Var = "aveRSV", slideBy = -1)
SKBLT.pc9[is.na(SKBLT.pc9)]<- 0 
summary(SKBLT.pc9$`aveRSV_PChangeFrom-1`)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-49.6894  -7.1545   0.8283   1.9474   8.7086 111.2676
#########################

#Checking seasonality
count_ma<- ts(na.omit(SKBLT.p$aveRSF), frequency = 12)
decomp<-stl(count_ma, s.window = "periodic")
plot(decomp)
decomp<- as.ts(decomp)
decomp
deseason<- seasadj(decomp)

install.packages("forecast")
library(forecast)
autoplot(decomp)
ggseasonplot(deseason, year.lables = T, continuous = T, polar = T)
```


```{r}
setwd()
skbr<- read.csv(file="H:/Personal/DesktopFiles/GtrendData/SkinBleachingRegion0419.csv", header = F, sep = ",", stringsAsFactors = F,col.names = c("Region","RSF"))
skbr
skbr<- skbr[c(-1,-2),]
skbr$RSF<- as.numeric(skbr$RSF)
head(skbr)

sklr<- read.csv("H:/Personal/DesktopFiles/GtrendData/SkinLighteningRegion0419.csv", header = T, sep = ",", stringsAsFactors = F,col.names = c("Region","RSF"))
sklr<- sklr[-1,]
sklr$RSF<- as.numeric(sklr$RSF)
sklr

skblr<- merge(skbr,sklr, by = "Region", sort = T)
colnames(skblr)<- c("Region","Bleaching","Lightening")
skblr

skblr.naomit<- na.omit(skblr)

skblr
skblr.naomit

skblr.naomit$aveRSF<- (skblr.naomit$Bleaching + skblr.naomit$Lightening)/2

skblr.naomit

skblr.ave<- skblr.naomit[,c(1,4)]

skblr.ave



summary(skblr.ave$aveRSF)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 29.50   43.75   56.75   58.21   72.38  100.00 
skblr.ave<- skblr.ave[order(-skblr.ave$aveRSF),]
skblr.ave
skblr.3qt<- skblr.ave[skblr.ave$aveRSF>=72.38,]
skblr.3qt

#21	Maryland	100.0		
#33	New York	96.5		
#11	Georgia	92.5		
#10	Florida	83.0		
#25	Mississippi	82.5		
#9	District of Columbia	76.0		
#31	New Jersey	76.0		
#44	Texas	73.0		
#5	California	72.5		
#29	Nevada	72.5	
skblr.3qt$nonWPop<-c(100-50.5,100-55.4	,100-52.4	,100-53.5	,100-56.5	,100-37.1	,100-54.9	,100-41.5	,100-36.8	,100-48.7)

skblr.3qt

skblr.nwpop<- skblr.3qt[order(-skblr.3qt$nonWPop),]

print(skblr.nwpop)



```
```{r}
library(tidyverse)
install.packages(dplyr)
library(dplyr)
#skblm = skin bleaching lightening merge
skblm<- as_tibble(skblr.naomit)
skblm<- skblm%>%dplyr::mutate(region=stringr::str_to_lower(Region))
sMap<- ggplot2::map_data("state")

#skblmm = skin bleaching lightening merge map
skblmm<- merge(sMap, skblm, by = 'region')
skblmm<- skblm%>% dplyr::left_join(x=., y = sMap, by ='region')

theme1<- function() {
  theme_bw() +
    theme(panel.background = element_blank()) +
    theme(plot.background = element_rect(fill = "seashell")) +
    theme(panel.border = element_blank()) +                     # facet border
    theme(strip.background = element_blank()) +                 # facet title background
    theme(plot.margin = unit(c(.5, .5, .5, .5), "cm")) +
    theme(panel.spacing = unit(3, "lines")) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(legend.background = element_blank()) +
    theme(legend.key = element_blank()) +
    theme(legend.title = element_blank())
}

theme2 <- function(){
  theme1()+
    theme(axis.title = element_blank())+
    theme(axis.text = element_blank())+
    theme(axis.ticks = element_blank())
}
head(skblmm)
skblmm

skblmm%>% ggplot(aes(x=long, y = lat))+ geom_polygon(aes(group = group, fill = log(aveRSF)), colour = 'white')+theme2()+ggtitle("Skin Bleaching and Skin Lightening by Region 2004 - 2019")



```


```{r}
year<- read.csv("year.csv", header = T, stringsAsFactors = F, sep =",")
dim(year)
year<- year[,c(8,9,10,11)]
year<-na.omit(year)
dim(year)
year
summary(year$percent.Difference.1)*100
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-17.053   4.256  12.079  11.688  16.188  45.038 
```

```{r}
install.packages("pastecs")
library(pastecs)

data(marbio)
marbio
#data is very skewed right
hist(marbio[,8])
trend.test(marbio[,8])
#data:  marbio[, 8] and time(marbio[, 8])
#S = 43853, p-value = 0.1841
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#      rho 
#0.1630113 
mm<- trend.test(marbio[,8], R=99)
mm
mm$p.value
plot(mm)
#######################################

SKBLT.ts
trend.test(SKBLT.ts)
acf(SKBLT.ts, lag.max = 48, main= "Correlogram of SBL Searchs from 2004 - 2019")

# to correct for serial correlation
(acf(diff(SKBLT.ts), lag.max = 48, main= "Correlogram of SBL Searchs from 2004 - 2019"))

acf(SKBLT.ts, type = "partial")
#Cannot compute exact p-value with ties
#	Spearman's rank correlation rho

#data:  SKBLT.p[, 2] and time(SKBLT.p[, 2])
#S = 224180, p-value < 2.2e-16
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#      rho 
#0.7909619 
trend.skblt<- trend.test(SKBLT.ts)
plot(trend.skblt)
trend.skblt$p.value
trend.skblt

trend.skblt2<- trend.test(diff(SKBLT.ts))
trend.skblt2
version

summary(linmod)
corr
sqrt(0.6)
```


```{r}
Region<- c("Alabama", "Alaska","Arizona", "Arkansas", "California", "Colorado","Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", " Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada","New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Puerto Rico", "Rhode Island", "South Carolina", " South Dakota", "Tennessee", "Texas", "Utah", "Vermont", " Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

wonly<- c(65.4,60.3,54.4,72.2,36.8,67.9,66.5,61.9,37.1,53.5,52.4,21.8,81.7,61.0,78.9,85.3,75.7,84.3, 58.6,93.1,50.5,71.4,74.9,79.5,56.5, 79.3,85.9,78.6,48.7,90.0,54.9,37.1,55.4,62.8,84.0,78.7,65.3,75.3,76.1,0.8,72.0,63.7,81.4,73.7,41.5,78,92.5,61.5,68.0,92.1,81.1,83.8 )

woPop<- data.frame(Region,wonly)
woPop %>% mutate_if(is.factor, as.character) -> woPop

woPop
skblr.naomit

skblmmp<- merge(woPop, skblr.naomit, by ="Region", all = T )
skblmmp

skblmmp.naomit<- na.omit(skblmmp)

skblmmp.naomit<- skblmmp.naomit[order(-skblmmp.naomit$aveRSF),]

skblmmp.naomit$nwp<- 100 - skblmmp.naomit$wonly

skblmmp.naomit<- skblmmp.naomit[order(-skblmmp.naomit$nwp),]
skblmmp.naomit

skblm1<- as_tibble(skblmmp.naomit)
skblm1<- skblm1%>%dplyr::mutate(region=stringr::str_to_lower(Region))
sMap<- ggplot2::map_data("state")
#skblmm = skin bleaching lightening merge map
skblmm1<- merge(sMap, skblm1, by = 'region')
skblmm1<- skblmm1%>% dplyr::left_join(x=., y = sMap, by ='region')
skblmm1
skblmm1<- skblmm1[,-c(7,)]
colnamesskblmm1

skblmm1%>% ggplot(aes(x=long, y = lat.y))+ geom_polygon(aes(group = group.y, fill = log(nwp)), colour = 'white')+theme2()+ggtitle("Skin Bleaching and Skin Lightening by Region")

```

```{r}
###### pop estimates
cenpop<- read.csv("H:/Data/Personal/RStudio/census/popestimates1317.csv", header = T, sep = ",",stringsAsFactors = F)
head(cenpop)

write.csv(cenpopr,"H:/Data/Personal/RStudio/census/popestimatesr.csv" )


cenpopr<- as.data.frame(t(cenpop))
cenpopr<- cenpopr[-2,]
cenpoprr<- read.csv("H:/Data/Personal/RStudio/census/popestimatesr.csv",header = T, sep=",", stringsAsFactors = F)
dim(cenpoprr)
cenpoprr$Foreign.born.persons..percent..2013.2017<-as.numeric(as.factor(cenpoprr$Foreign.born.persons..percent..2013.2017))
cenpoprr[,-1]<-as.numeric(as.factor(cenpoprr[,-1]))

summary(cenpoprr$Foreign.born.persons..percent..2013.2017)

skblmmp<- merge(cenpoprr, skblr.naomit, by ="Region", all = T )

cor.test(skblmmp$Foreign.born.persons..percent..2013.2017, skblmmp$aveRSF)

cor.test(skblmmp$Hispanic.or.Latino..percent, skblmmp$aveRSF)
0.17

cenpoprr

cenpopr$V18
cenpopr
summary(cenpopr$V18)

cenf<-as.numeric(cenpopr$V18)

cenf
```


```{r}
library(viridis)
library(ggplot2)
library(dplyr)
library(formattable)
library(gridExtra)
library(forecast)
library(grid)
library(formattable)
###SEASESonal plots######
jpeg("//users17/users17$/lawres13/Data/Personal/DesktopFiles/GtrendGraphs/SBLpolar_0419.jpeg",width=600,height=400,res = 120)
ggseasonplot(count_ma, continuous = T, polar = T, main = "SBL Interest polar plot by Month 2004 - 2019", col = c("black","red"))+theme_bw()
dev.off()
jpeg("//users17/users17$/lawres13/Data/Personal/DesktopFiles/GtrendGraphs/SBLlinear_0419.jpeg",width=600,height=400,res = 120)
ggseasonplot(count_ma, continuous = T, polar = F, main = "SBL Interest Linear plot by Month 2004 - 2019",col = c("black","red"))+theme_bw()
dev.off()
jpeg("//users17/users17$/lawres13/Data/Personal/DesktopFiles/GtrendGraphs/SBLseasonal_0419.jpeg",width=600,height=400,res = 120)
ggsubseriesplot(count_ma, main = "SBL Interest with Mean bar by Month 2004 - 2019",col = c("black","red"), ylab="SBL Interest")+theme_bw()
dev.off()

# to correct for serial correlation

jpeg("//users17/users17$/lawres13/Data/Personal/DesktopFiles/GtrendGraphs/SBLacf_0419.jpeg",width=600,height=400,res = 120)
ggAcf(count_ma, main = "Autocorrelation Correlogram of SBL Interest by Month")
dev.off()
jpeg("//users17/users17$/lawres13/Data/Personal/DesktopFiles/GtrendGraphs/SBLdiffacf_0419.jpeg",width=600,height=400,res = 120)
ggAcf(diff(count_ma), main = "Autocorrelation Correlogram of SBL Interest differenced by Month")
dev.off()

####

jpeg("//users17/users17$/lawres13/Data/Personal/DesktopFiles/GtrendGraphs/SBL_0419.jpeg",width=600,height=400,res = 120)
skblmm%>% ggplot(aes(x=long, y = lat))+ geom_polygon(aes(group = group, fill = aveRS), colour = 'white')+ggtitle("Skin Bleaching and Skin Lightening by Region 2004 - 2019")+theme_void()+scale_fill_gradient( limit = c(0,100), name = "Percent\nInterest", low = "white", high = "#FFAB00")
dev.off()

jpeg("//users17/users17$/lawres13/Data/Personal/DesktopFiles/GtrendGraphs/NON White1015.jpeg",width=600,height=400,res = 120)
skblmm1%>% ggplot(aes(x=long, y = lat))+ geom_polygon(aes(group = group, fill = nwp), colour = 'white')+theme2()+ggtitle("Non-White Population 2010 - 2015")+scale_fill_gradient(limit = c(0,100), name = "Percent\nPopulation", low = "white", high = "#FFAB00")+theme_void()
dev.off()

### Table for 3quartile states and respective non white population ###
customOr = "#FFEFCD"
customOr0 = "#FFAB00"

formattable(skblr.3qt, align =c("l","c","r"),
            list(`Region` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold"))
              ,`SBL_Interest`= color_tile(customOr, customOr0), `Non_White Population`= color_tile(customOr, customOr0)
            ))

#### Bivariate map
library(viewpoint)

jpeg("//users17/users17$/lawres13/Data/Personal/DesktopFiles/GtrendGraphs/bimapnwpsbl.jpeg",width=600,height=400,res = 120)
map2<- skblmm1%>% ggplot(aes(x=long, y = lat), colour = "white" , fill = NA)+ geom_polygon(aes(group = group,fill =atan(yyy/xxx),alpha=xxx+yyy), colour = 'white')+ggtitle("SBL Interest and Non-White Population by State", subtitle = "Bivariate Choropleth map")+theme_void()+scale_fill_viridis( limit = c(0,4),direction = -1, option = "B")+guides(alpha=F,fill=F)
vp<-viewport(width=0.24,height=0.35,x=0.84,y=0.25)
print(map2)
print(g.legend+labs(title=""),vp=vp)
dev.off()

```




```{r}
#skBT<- read.csv("SkinBleachingTime0419.csv", header = T, sep = ",",stringsAsFactors = F, col.names = c("Time","RSF"))
skBT<- read.csv("H:/Personal/DesktopFiles/GtrendData/SkinBleachingTime0419.csv", header = T, sep = ",",stringsAsFactors = F, col.names = c("Time","RSF"))
skBT<- skBT[-1,]
skBT$RSF<- as.numeric(skBT$RSF)
skBT
head(skBT)

skLT<- read.csv("H:/Personal/DesktopFiles/GtrendData/SkinLighteningTime0419.csv", header = T, sep = ",", stringsAsFactors = F, col.names = c("Time","RSF"))
skLT<- skLT[-1,]
skLT$RSF<- as.numeric(skLT$RSF)

################# SBL States

CaliB<-read.csv("H:/Data/Personal/RStudio/data for states/Skin Bleaching/CalB.csv", header = T, sep = ",", stringsAsFactors = F, col.names = c("Time","RSF"))
CaliB<- CaliB[c(-1,-189,-188),]
CaliB$RSF<- as.numeric(CaliB$RSF)

CaliL<-read.csv("H:/Data/Personal/RStudio/data for states/Skin Lighting/CaliforniaLTrend.csv", header = T, sep = ",", stringsAsFactors = F, col.names = c("Time","RSF"))
CaliL<- CaliL[c(-1,-189,-188),]
CaliL$RSF<- as.numeric(CaliL$RSF)

CalSBL<- merge(CaliB, CaliL, by = "Time", sort = T)

colnames(CalSBL)<- c("Date","Bleaching", "Lightening")

CalSBL$CaliforniaSBL<- (CalSBL$Bleaching+CalSBL$Lightening)/2

summary(CalSBL$CaliforniaSBL)
sd(CalSBL$CaliforniaSBL)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   Sd
# 13.00   33.00   45.50   44.90   56.88   99.00  15.39

#####################################

```


```{r}
DCB<-read.csv("H:/Data/Personal/RStudio/data for states/Skin Bleaching/DCBTrend.csv", header = T, sep = ",", stringsAsFactors = F, col.names = c("Time","RSF"))
DCB<- DCB[c(-1,-189,-188),]
DCB$RSF<- as.numeric(DCB$RSF)

DCL<-read.csv("H:/Data/Personal/RStudio/data for states/Skin Lighting/DCLTrend.csv", header = T, sep = ",", stringsAsFactors = F, col.names = c("Time","RSF"))
DCL<- DCL[c(-1,-189,-188),]
DCL$RSF<- as.numeric(DCL$RSF)

DCBL<- merge(DCB, DCL, by = "Time", sort = T)

colnames(DCBL)<- c("Date","Bleaching", "Lightening")

DCBL$DistricofColumbiaSBL<- (DCBL$Bleaching+DCBL$Lightening)/2

summary(DCBL$DistricofColumbiaSBL)
sd(DCBL$DistricofColumbiaSBL)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  Sd
# 0.00   12.00   17.00   17.40   21.38   90.50  10.25
################################
```


```{r}

FlorB<-read.csv("H:/Data/Personal/RStudio/data for states/Skin Bleaching/FloridaTrend.csv", header = T, sep = ",", stringsAsFactors = F, col.names = c("Time","RSF"))
FlorB<- FlorB[c(-1,-189,-188),]
FlorB$RSF<- as.numeric(FlorB$RSF)

FlorL<-read.csv("H:/Data/Personal/RStudio/data for states/Skin Lighting/FlorLTrend.csv", header = T, sep = ",", stringsAsFactors = F, col.names = c("Time","RSF"))
FlorL<- FlorL[c(-1,-189,-188),]
FlorL$RSF<- as.numeric(FlorL$RSF)

head(FlorL)


FlorBL<- merge(FlorB, FlorL, by = "Time", sort = T)

colnames(FlorBL)<- c("Date","Bleaching", "Lightening")

FlorBL$FloridaSBL<- (FlorBL$Bleaching+FlorBL$Lightening)/2
summary(FlorBL$FloridaSBL)
sd(FlorBL$FloridaSBL)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   29.12   36.50   35.10   42.38  100.00 
# Sd 13.07095

#######################################

```


```{r}

GeorB<-read.csv("H:/Data/Personal/RStudio/data for states/Skin Bleaching/GeorBTrend.csv", header = T, sep = ",", stringsAsFactors = F, col.names = c("Time","RSF"))
GeorB<- GeorB[c(-1,-189,-188),]
GeorB$RSF<- as.numeric(GeorB$RSF)

GeorL<-read.csv("H:/Data/Personal/RStudio/data for states/Skin Lighting/GeorLTrend.csv", header = T, sep = ",", stringsAsFactors = F, col.names = c("Time","RSF"))
GeorL<- GeorL[c(-1,-189,-188),]
GeorL$RSF<- as.numeric(GeorL$RSF)

GeorBL<- merge(GeorB, GeorL, by = "Time", sort = T)

colnames(GeorBL)<- c("Date","Bleaching", "Lightening")

GeorBL$GeorgiaSBL<- (GeorBL$Bleaching+GeorBL$Lightening)/2

summary(GeorBL$GeorgiaSBL)
sd(GeorBL$GeorgiaSBL)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   22.50   29.50   29.62   37.00   77.50 
# sd 12.89498
#########################################
```


```{r}
MaryB<-read.csv("H:/Data/Personal/RStudio/data for states/Skin Bleaching/MarylandTrend.csv", header = T, sep = ",", stringsAsFactors = F, col.names = c("Time","RSF"))
MaryB<- MaryB[c(-1,-189,-188),]
MaryB$RSF<- as.numeric(MaryB$RSF)

MaryL<-read.csv("H:/Data/Personal/RStudio/data for states/Skin Lighting/MarylandTrendSL.csv", header = T, sep = ",", stringsAsFactors = F, col.names = c("Time","RSF"))
MaryL<-MaryL[c(-1,-189,-188),]
MaryL$RSF<- as.numeric(MaryL$RSF)

MaryBL<- merge(MaryB, MaryL, by = "Time", sort = T)

colnames(MaryBL)<- c("Date","Bleaching", "Lightening")

MaryBL$MarySBL<- (MaryBL$Bleaching+MaryBL$Lightening)/2

summary(MaryBL$MarySBL)
sd(MaryBL$MarySBL)

##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.00   15.62   21.25   20.17   26.00   56.50 
##  sd 10.28252
################################################################

```

```{r}
MissB<-read.csv("H:/Data/Personal/RStudio/data for states/Skin Bleaching/MississippiTrend.csv", header = T, sep = ",", stringsAsFactors = F, col.names = c("Time","RSF"))
MissB<- MissB[c(-1,-189,-188),]
MissB$RSF<- as.numeric(MissB$RSF)

MissL<-read.csv("H:/Data/Personal/RStudio/data for states/Skin Lighting/MississippiTrendSL.csv", header = T, sep = ",", stringsAsFactors = F, col.names = c("Time","RSF"))
MissL<-MissL[c(-1,-189,-188),]
MissL$RSF<- as.numeric(MissL$RSF)

MissBL<- merge(MissB, MissL, by = "Time", sort = T)

colnames(MissBL)<- c("Date","Bleaching", "Lightening")

MissBL$MissSBL<- (MissBL$Bleaching+MissBL$Lightening)/2
#############################################


summary(MissBL$MissSBL)
sd(MissBL$MissSBL)


# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.000   2.500   6.000   6.559   8.000  50.000 
#  sd 7.278569
```


```{r}
NevB<-read.csv("H:/Data/Personal/RStudio/data for states/Skin Bleaching/NevadaTrend.csv", header = T, sep = ",", stringsAsFactors = F, col.names = c("Time","RSF"))
NevB<- NevB[c(-1,-189,-188),]
NevB$RSF<- as.numeric(NevB$RSF)

NevL<-read.csv("H:/Data/Personal/RStudio/data for states/Skin Lighting/NevadaTrendSL.csv", header = T, sep = ",", stringsAsFactors = F, col.names = c("Time","RSF"))
NevL<-NevL[c(-1,-189,-188),]
NevL$RSF<- as.numeric(NevL$RSF)

NevBL<- merge(NevB,NevL, by = "Time", sort = T)

colnames(NevBL)<- c("Date","Bleaching", "Lightening")

NevBL$NevSBL<- (NevBL$Bleaching+NevBL$Lightening)/2


summary(NevBL$NevSBL)
sd(NevBL$NevSBL)
##########################################

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00    6.50   10.00   11.42   13.50   50.00 
# sd 9.721934
```


```{r}

NewJB<-read.csv("H:/Data/Personal/RStudio/data for states/Skin Bleaching/NewJerseyTrend.csv", header = T, sep = ",", stringsAsFactors = F, col.names = c("Time","RSF"))
NewJB<- NewJB[c(-1,-189,-188),]
NewJB$RSF<- as.numeric(NewJB$RSF)

NewJL<-read.csv("H:/Data/Personal/RStudio/data for states/Skin Lighting/NewJerseyTrend SL.csv", header = T, sep = ",", stringsAsFactors = F, col.names = c("Time","RSF"))
NewJL<-NewJL[c(-1,-189,-188),]
NewJL$RSF<- as.numeric(NewJL$RSF)

NewJBL<- merge(NewJB,NewJL, by = "Time", sort = T)

colnames(NewJBL)<- c("Date","Bleaching", "Lightening")

NewJBL$NewJSBL<- (NewJBL$Bleaching+NewJBL$Lightening)/2

summary(NewJBL$NewJSBL)
sd(NewJBL$NewJSBL)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   23.12   29.50   30.53   36.38   85.50 
## SD 13.40318
#######################################
```


```{r}

NewYB<-read.csv("H:/Data/Personal/RStudio/data for states/Skin Bleaching/NewYorkTrend.csv", header = T, sep = ",", stringsAsFactors = F, col.names = c("Time","RSF"))
NewYB<- NewYB[c(-1,-189,-188),]
NewYB$RSF<- as.numeric(NewYB$RSF)

NewYL<-read.csv("H:/Data/Personal/RStudio/data for states/Skin Lighting/NewYorkTrendSL.csv", header = T, sep = ",", stringsAsFactors = F, col.names = c("Time","RSF"))
NewYL<-NewYL[c(-1,-189,-188),]
NewYL$RSF<- as.numeric(NewYL$RSF)

NewYBL<- merge(NewYB,NewYL, by = "Time", sort = T)

colnames(NewYBL)<- c("Date","Bleaching", "Lightening")

NewYBL$NewYSBL<- (NewYBL$Bleaching+NewYBL$Lightening)/2
############################################

summary(NewYBL$NewYSBL)
sd(NewYBL$NewYSBL)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#7.00   32.00   45.25   44.37   56.50   79.50 
# sd 15.75851
```



```{r}

TexB<-read.csv("H:/Data/Personal/RStudio/data for states/Skin Bleaching/TexusTrend.csv", header = T, sep = ",", stringsAsFactors = F, col.names = c("Time","RSF"))
TexB<- TexB[c(-1,-189,-188),]
TexB$RSF<- as.numeric(TexB$RSF)

TexL<-read.csv("H:/Data/Personal/RStudio/data for states/Skin Lighting/NewYorkTrendSL.csv", header = T, sep = ",", stringsAsFactors = F, col.names = c("Time","RSF"))
TexL<-TexL[c(-1,-189,-188),]
TexL$RSF<- as.numeric(TexL$RSF)

TexBL<- merge(TexB,TexL, by = "Time", sort = T)

colnames(TexBL)<- c("Date","Bleaching", "Lightening")

TexBL$TexSBL<- (TexBL$Bleaching+TexBL$Lightening)/2
#########################################
summary(TexBL$TexSBL)
sd(TexBL$TexSBL)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   27.12   37.25   36.82   45.50   72.50 
# sd13.35678
```

```{r}

SKBLT<- merge(skBT, skLT, by = "Time", sort = T)

colnames(SKBLT)<- c("Date","Bleaching", "Lightening")

SKBLT$aveRSF<- (SKBLT$Bleaching+SKBLT$Lightening)/2
SKBLT
SKBLT.p<- SKBLT[,c(1,4)]



SKBLT.ts<- ts(SKBLT.p$aveRSF)

#x axis is in number of months equal to 174 months
plot(SKBLT.ts , type = "o", main = "SKin Bleaching and Lightening Trend from 2004 To 2019", xlab = "Months: 0 = 2004-01, 186 = 2019-06", ylab = "Relative Search Popularity")+plot(SKBLT.ts, type = "o")
#SKBLT.p<- SKBLT[,c(1,4)]
#x axis is in number of months equal to 174 months
#plot(SKBLT.p$aveRSF, type = "o", main = "SKin Bleaching and Lightening Trend from 2004 To 2019", xlab = "Months: 0 = 2004-01, 186 = 2019-06", ylab = "Relative Search Popularity")
SKBLT.p
```

```{r}
#################################################
library(ggplot2)

DFFF<- data.frame( SKBLT.p$aveRSF,MaryBL$MarySBL,NewYBL$NewYSBL,GeorBL$GeorgiaSBL, FlorBL$FloridaSBL,MissBL$MissSBL,DCBL$DistricofColumbiaSBL,NewJBL$NewJSBL,TexBL$TexSBL,CalSBL$CaliforniaSBL, NevBL$NevSBL)

library(ggplot2)
ggplot(DFFF, aes(x= DFFF$SKBLT.p.Date, y = value, color = variable))+geom_point(aes(y = DFFF$SKBLT.p.aveRSF, col = "DFFF$SKBLT.p.aveRSF"))+geom_point(aes(y = DFFF$CalSBL.CaliforniaSBL, col = "DFFF$CalSBL.CaliforniaSBL"))

#########################################

library(viridis)
ggplot(mm, aes(mm$SKBLT.p.Date, mm$value, color = mm$variable))+geom_line(stat = "identity", aes(group = mm$variable) )+theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + scale_x_discrete(breaks = mm$SKBLT.p.Date[c(T,F,F,F,F,F)])+geom_line()
install.packages("forcast")
library(forecast)
mm
DFFF
DFFF.ts<- ts(DFFF)
DFFF.ts
colnames(DFFF.ts)<- c("Overall","MD","NY","GA","FL","MS", "DC", "NJ", "TX", "CA","NV")
autoplot(DFFF.ts, facets = T, main = "SBL Interest of Top Quartile States 2004 - 2019", ylab = "SBL Interest", xlab = "Months")
```

```{r}

### This data is all states by year 
AllB<-read.csv("H:/Data/Personal/RStudio/data for states/Skin Bleaching/AllStatesSkinBleachingbyYear.csv", header = T, sep = ",", stringsAsFactors = F)
AllB$skin.bleaching<- as.numeric(AllB$skin.bleaching)

head(AllB)

ggplot(AllB, aes(AllB$Year, AllB$skin.bleaching ,color = AllB$Region))+geom_line(stat = "identity", aes(group = AllB$Region))+theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

write.csv(AllB,"H:/Data/Personal/RStudio/data for states/Skin Bleaching/AllStatesSkinBleachingbyYear.csv", row.names = T )

```

```{r}
AllL<-read.csv("H:/Data/Personal/RStudio/data for states/Skin Lighting/AllStatesSkinLighteningByYear2.csv", header = T, sep = ",", stringsAsFactors = F)
AllL$skin.lightening<- as.numeric(AllL$skin.lightening)

AllL<- AllL[order(AllL$Year),]

AllL[is.na(AllL)]<- 0

AllL
install.packages("grid")
library(grid)
library(ggplot2)
library(dplyr)
library(tidyverse)

group_region <- AllL %>% group_by(Region[AllL$skin.lightening==100]) 
head(group_region)

ddd<- data.frame(AllL$Region[AllL$skin.lightening==100], AllL$Year[AllL$skin.lightening==100])
ddd<- reshape(ddd, idvar = "AllL.Region.AllL.skin.lightening....100.", direction = "wide", timevar = "AllL.Year.AllL.skin.lightening....100.")
ddd
ddd<-as.data.frame(table(ddd))
ddd$AllL.Region.AllL.skin.lightening....100.<- as.character(ddd$AllL.Region.AllL.skin.lightening....100.)
ddd$Freq<- as.numeric(ddd$Freq)
ddd$sum<- sapply(ddd, FUN = sum)

frequency(ddd)
ggplot(AllL, aes(AllL$Year, AllL$skin.lightening ,color = AllL$Region))+geom_line(stat = "identity", aes(group = AllL$Region), labels(list(AllL$Region))+theme(axis.text.x = element_text(angle = 90, vjust = 0.5)))+labs(x = "Year", y = "Skin Lightening Interest")

#theme(legend.position = "none")

table(ddd)

write.csv(AllL,"H:/Data/Personal/RStudio/data for states/Skin Lighting/AllStatesSkinlightingbyYear.csv", row.names = T )

```


```{r}
AllBL<- merge(AllB, AllL, by = c("Region", "Year"), sort = T)

AllBL$SBL<- (AllBL$skin.bleaching + AllBL$skin.lightening)/2

AllBL

AllBL.t<- AllBL[,c(2,1,5)]
colnames(AllBL.t)<- c("Year", "variable", "value")
AllBL.t

ggplot(AllBL.t, aes(AllBL.t$Year, AllBL.t$value, color = AllBL.t$variable))+geom_line(stat = "identity", aes(group = AllBL.t$variable))+theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


AllBL.tsw<- reshape(AllBL.t, 
                    timevar = "Year",
                    idvar = "variable",
                    direction = "wide")

AllBL.ts<- reshape(AllBL.t, 
                   timevar = "variable",
                   idvar = "Year",
                   direction = "wide")


head(AllBL.tsw)
AllBL.tsws<- ts(AllBL.tsw)
head(AllBL.tsws)
AllBL.tsw

autoplot(AllBL.tsw, facets = T)

head(AllBL.ts)
head(AllBL.tsw)

AllBL.ts<- ts(AllBL.ts)
AllBL.ts

autoplot(AllBL.ts, facets = T)

summary(AllBL.ts$value.Alabama)
AllBL.tsw[is.na(AllBL.tsw)]<- 0
is.na(AllBL.tsw)

AllBL.tsw<- AllBL.tsw[order(-AllBL.tsw$value.2004,-AllBL.tsw$value.2005,-AllBL.tsw$value.2006,-AllBL.tsw$value.2007,-AllBL.tsw$value.2008,-AllBL.tsw$value.2009,-AllBL.tsw$value.2010,-AllBL.tsw$value.2011,-AllBL.tsw$value.2012,-AllBL.tsw$value.2013,-AllBL.tsw$value.2014,-AllBL.tsw$value.2015,-AllBL.tsw$value.2016,-AllBL.tsw$value.2017,-AllBL.tsw$value.2018,-AllBL.tsw$value.2019),]

AllBL.tsw

```

```{r}
######################

##Census data


cen1317<- read.csv("H:/Data/Personal/RStudio/InternetAccess/internetaccess_1317.csv", header = T, sep = ",", stringsAsFactors = F)
head(cen1317)
cen1317$Internet.Access<- cen1317$Internet.Access*100


cen03<- read.csv("H:/Data/Personal/RStudio/InternetAccess/InternetAccess 2003.csv", header = T, sep = ",", stringsAsFactors = F)
head(cen03)

cenMerge<- merge(cen1317, cen03, by = "Region")

cenMerge$aveIntAcc<- (cenMerge$Internet.Access+cenMerge$PI.Percent)/2

cenM<- data.frame(cenMerge$Region,cenMerge$Internet.Access,cenMerge$PI.Percent, cenMerge$aveIntAcc)

colnames(cenM)<- c("Region", "Internet1317", "Internet2003", "AveIntAcc")

cenM


AllLr<- reshape(AllL, idvar = "Region", direction = "wide", timevar = "Year")

cenSL <- merge(cenM, AllLr , by = "Region", sort = T)

cenSL

dim(cenSBL)
hist(cenSL$Internet2003, n = 10)
sd(cenSBL)



scatter.smooth(cenSL$Internet2003[cenSL$skin.lightening.2004>0], cenSL$skin.lightening.2004[cenSL$skin.lightening.2004>0], ylab= "SL Interest 2004", xlab= "Internet Access 2003")
scatter.smooth(cenSL$Internet1317[cenSL$skin.lightening.2017>0], cenSL$skin.lightening.2017[cenSL$skin.lightening.2017>0], ylab = "SL Interest 2017", xlab = "Internet Access 1317")
cor.test(cenSBL$Internet2003, cenSBL$skin.lightening.2004)
cor.test(cenSBL$Internet1317[cenSL$skin.lightening.2017>0], cenSBL$skin.lightening.2017[cenSL$skin.lightening.2017>0])
ccc<- data.frame(cenSL$Region[cenSL$Internet2003>60])

ccc

cenSL

scatter.smooth(cenSL$Internet1317[cenSL$skin.lightening.2017>0]/cenSL$Internet2003[cenSL$skin.lightening.2017>0], cenSL$skin.lightening.2017[cenSL$skin.lightening.2017>0], ylab = "SL Interest 2017", xlab = "Internet Access 2003 - 2017")

write.csv(cenSL,"H:/Data/Personal/RStudio/InternetAccess/census_Int_SL.csv", row.names = T)
```


