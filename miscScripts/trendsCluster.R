# raw precip/temp trends by cluster
# MAC 08/16/23
# adapted from percTransitions.R

library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(readr)

##### WATER YEAR 
# water year means
wyPrec<-read_csv("data/csv_5cluster/Cluster5_12mo_total_precip_PRISM_1895-2022.csv")
wyTemp<-read_csv("data/csv_5cluster/Cluster5_12mo_mean_temp_PRISM_1895-2022.csv")

# subset to seasons
wyPrec<-subset(wyPrec, month %in% c(9))
wyTemp<-subset(wyTemp, month %in% c(9))

# convert to long for plotting
wyPrecLong<-tidyr::gather(wyPrec, cluster,prec, 4:8)
wyTempLong<-tidyr::gather(wyTemp, cluster,temp, 4:8)

##### PRECIP
# mean by group
wyPrecLong<-subset(wyPrecLong, !is.na(prec))
wyPrecLong<-wyPrecLong %>% group_by(cluster) %>%
                            mutate(meanPrec=mean(prec, na.rm=TRUE))
wyPrecLong$anom<-wyPrecLong$prec-wyPrecLong$meanPrec
wyPrecLong$anomCat<-ifelse(wyPrecLong$anom>0, "wet","dry")

ggplot(wyPrecLong)+
  geom_bar(aes(year,anom, fill=anomCat),stat="identity",position="identity")+
  scale_fill_manual(values=c("brown","green"))+
  #geom_smooth(aes(year,anom),method=lm)+
  scale_x_continuous(breaks=seq(1900,2020,10))+
  facet_wrap(.~cluster, ncol=1)+
  ggtitle("Water Year Precipitation Anomaly by Cluster")

##### TEMPS
# mean by group
wyTempLong<-subset(wyTempLong, !is.na(temp))
wyTempLong<-wyTempLong %>% group_by(cluster) %>%
  mutate(meanTemp=mean(temp, na.rm=TRUE))
wyTempLong$anom<-wyTempLong$temp-wyTempLong$meanTemp
wyTempLong$anomCat<-ifelse(wyTempLong$anom>0, "warm","cool")

ggplot(wyTempLong)+
  geom_bar(aes(year,anom, fill=anomCat),stat="identity",position="identity")+
  scale_fill_manual(values=c("blue","red"))+
  #geom_smooth(aes(year,anom),method=lm)+
  scale_x_continuous(breaks=seq(1900,2020,10))+
  facet_wrap(.~cluster, ncol=1)+
  ggtitle("Water Year Temperature Anomaly by Cluster")
#####


##### 
# seasonal means/sums
seasPrec<-read_csv("data/csv_5cluster/Cluster5_3mo_total_precip_PRISM_1895-2022.csv")
seasTemp<-read_csv("data/csv_5cluster/Cluster5_3mo_mean_temp_PRISM_1895-2022.csv")

# subset to seasons
seasPrec<-subset(seasPrec, month %in% c(3,6,9,12))
seasTemp<-subset(seasTemp, month %in% c(3,6,9,12))

# adjust to water year
seasPrec$year<-ifelse(seasPrec$month==12,seasPrec$year+1,seasPrec$year)
seasTemp$year<-ifelse(seasTemp$month==12,seasTemp$year+1,seasTemp$year)
# trim first year -prec
#seasPrec<-subset(seasPrec, dates>="1895-12-01")
seasPrec<-subset(seasPrec,year>1895 & year<2023)
seasPrec$wyDate<-as.Date(paste0(seasPrec$year,"-",seasPrec$month,"-01"))  
# trim first year -temp
#seasTemp<-subset(seasTemp, dates>="1895-12-01")
seasTemp<-subset(seasTemp,year>1895 & year<2023)
seasTemp$wyDate<-as.Date(paste0(seasTemp$year,"-",seasTemp$month,"-01"))  

# convert to long for plotting - prec
seasPrecLong<-tidyr::gather(seasPrec, cluster,prec, 4:8)
seasPrecLong$month<-factor(seasPrecLong$month, levels=c(12,3,6,9))

# convert to long for plotting - temp
seasTempLong<-tidyr::gather(seasTemp, cluster,temp, 4:8)
seasTempLong$month<-factor(seasTempLong$month, levels=c(12,3,6,9))

##### PRECIP SEAS
# mean by group
seasPrecLong<-subset(seasPrecLong, !is.na(prec))
seasPrecLong<-seasPrecLong %>% group_by(cluster, month) %>%
  mutate(meanPrec=mean(prec, na.rm=TRUE))
seasPrecLong$anom<-seasPrecLong$prec-seasPrecLong$meanPrec
seasPrecLong$anomCat<-ifelse(seasPrecLong$anom>0, "wet","dry")

ggplot(seasPrecLong)+
  geom_bar(aes(year,anom, fill=anomCat),stat="identity",position="identity")+
  scale_fill_manual(values=c("brown","forestgreen"))+
  geom_smooth(aes(year,anom),method=lm, se=F)+
  #scale_x_continuous(breaks=seq(1900,2020,10))+
  facet_grid(cluster~month)+
  ggtitle("Seasonal Precipitation Anomaly by Cluster")+
  theme_bw()

##### TEMPS SEAS
# mean by group
seasTempLong<-subset(seasTempLong, !is.na(temp))
seasTempLong<-seasTempLong %>% group_by(cluster,month) %>%
  mutate(meanTemp=mean(temp, na.rm=TRUE))
seasTempLong$anom<-seasTempLong$temp-seasTempLong$meanTemp
seasTempLong$anomCat<-ifelse(seasTempLong$anom>0, "warm","cool")

ggplot(seasTempLong)+
  geom_bar(aes(year,anom, fill=anomCat),stat="identity",position="identity")+
  scale_fill_manual(values=c("blue","red"))+
  geom_smooth(aes(year,anom),method=lm, se=F)+
  #scale_x_continuous(breaks=seq(1900,2020,10))+
  facet_grid(cluster~month)+
  ggtitle("Seasonal Temperature Anomaly by Cluster")+
  theme_bw()
#####


##### combined anomalies --- precip
# add in seas codes
seasDF<-cbind.data.frame(month=c(12,3,6,9),
                         seas=c("OND","JFM","AMJ","JAS"))
seasPrecLong<-merge(seasPrecLong, seasDF, by="month")

# add in wy code
wyPrecLong$seas<-"WY"

# adjusted prec dfs
temp1<-seasPrecLong[,-2]
temp2<-wyPrecLong
colnames(temp2)[1]<-"wyDate"
precLong<-rbind.data.frame(temp1,temp2)

precLong$seas<-factor(precLong$seas, levels=c("OND","JFM","AMJ","JAS","WY"))

ggplot(precLong)+
  geom_bar(aes(year,anom, fill=anomCat),stat="identity",position="identity")+
  scale_fill_manual(values=c("brown","forestgreen"))+
  #scale_y_continuous(limits=c(-200, 250), oob=squish)+
  #geom_smooth(aes(year,anom),method=lm, se=F)+
  #scale_x_continuous(breaks=seq(1900,2020,10))+
  facet_grid(seas~cluster)+
  ggtitle("Seasonal Precipitation Anomaly by Cluster")+
  theme_bw()
#####

##### combined anomalies --- temp
# add in seas codes
seasDF<-cbind.data.frame(month=c(12,3,6,9),
                         seas=c("OND","JFM","AMJ","JAS"))
seasTempLong<-merge(seasTempLong, seasDF, by="month")

# add in wy code
wyTempLong$seas<-"WY"

# adjusted prec dfs
temp1<-seasTempLong[,-2]
temp2<-wyTempLong
colnames(temp2)[1]<-"wyDate"
tempLong<-rbind.data.frame(temp1,temp2)

tempLong$seas<-factor(tempLong$seas, levels=c("OND","JFM","AMJ","JAS","WY"))

ggplot(tempLong)+
  geom_bar(aes(year,anom, fill=anomCat),stat="identity",position="identity")+
  scale_fill_manual(values=c("blue","red"))+
  #scale_y_continuous(limits=c(-200, 200), oob=squish)+
  geom_smooth(aes(year,anom),method=lm, se=F)+
  #scale_x_continuous(breaks=seq(1900,2020,10))+
  facet_grid(seas~cluster)+
  ggtitle("Seasonal Temp Anomaly by Cluster")+
  theme_bw()
#####

# model WY anoms by seasonal anoms
precWide<-spread(precLong[,c(2,4,7,9)], seas, anom, drop=TRUE)

library(broom)
library(dplyr)
library(tidyr)
library(purrr)

wyAnomMod<-precWide %>% group_by(cluster) %>%
  do(tidy(lm(WY ~ OND + JFM + AMJ + JAS, .)))
wyAnomMod<-precWide %>% group_by(cluster) %>%
  do(glance(lm(WY ~ OND + JFM + AMJ + JAS, .)))



moModels<- burnListDF %>% group_by(STA_NAME,month) %>%
  do(tidy(lm(scale(bhrs20) ~ scale(meanDP) + scale(meanT), .)))
moModelsR2<- burnListDF %>% group_by(STA_NAME,month) %>%
  do(glance(lm(scale(bhrs20) ~ scale(meanDP) + scale(meanT), .)))
summary(moModelsR2$r.squared)
