# precip/temp percentile transitions for cluster time series
# MAC 08/13/23
# adapted from tsTransitions

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

##### WATER YEAR
# water year percentiles
wyPrec<-read_csv("data/csv_5cluster/Cluster5_12mo_total_precip_percentile_PRISM_1895-2022.csv")
wyTemp<-read_csv("data/csv_5cluster/Cluster5_12mo_mean_temp_percentile_PRISM_1895-2022.csv")

# subset to seasons
wyPrec<-subset(wyPrec, month %in% c(9))
wyTemp<-subset(wyTemp, month %in% c(9))

# convert to long for plotting
wyPrecLong<-tidyr::gather(wyPrec, cluster,prec, 4:8)
wyTempLong<-tidyr::gather(wyTemp, cluster,temp, 4:8)

p1<-ggplot(wyPrecLong, aes(x = year, y = as.factor(cluster), fill = prec)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "brown", mid="white",high = "green", midpoint = 0.5)+
  #geom_text(aes(label = prec), color = "black", size = 4) +
  #facet_wrap(.~cluster, nrow=5)+
  #coord_fixed()+
  scale_x_continuous(breaks=seq(1900,2020,10))+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Water Year Total Precip Percentiles")

p2<-ggplot(wyTempLong, aes(x = year, y = as.factor(cluster), fill = temp)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "blue", mid="white",high = "red", midpoint = 0.5)+
  #geom_text(aes(label = prec), color = "black", size = 4) +
  #facet_wrap(.~cluster, nrow=5)+
  #coord_fixed()+
  scale_x_continuous(breaks=seq(1900,2020,10))+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Water Year Mean Temp Percentiles")

cowplot::plot_grid(p1, p2, labels = "AUTO", align = "v",ncol = 1)

# percentile categories
# percent rank code - precip
catVal<-0.33 
wyPrecCats<-cbind.data.frame(wyPrec[,1:3],
                             as.data.frame(lapply(wyPrec[, 4:8],
                                                  cut, br=c(0,(catVal),(1-catVal),Inf),
                                                  labels=c("dry","avg","wet"))))

wyPrecCatsLong<-tidyr::gather(wyPrecCats, cluster,cat, 4:8)
wyPrecCatsLong$cat<-factor(wyPrecCatsLong$cat, levels=c("dry","avg","wet"))

p1<-ggplot(wyPrecCatsLong, aes(x = year, y = as.factor(cluster), fill = cat)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = c("brown", "white", "green"), name = "Precip Cat", 
                    guide = guide_legend(reverse = TRUE))+
  #geom_text(aes(label = prec), color = "black", size = 4) +
  #facet_wrap(.~cluster, nrow=5)+
  #coord_fixed()+
  scale_x_continuous(breaks=seq(1900,2020,10))+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Water Year Total Precip Percentile Categories")

# percent rank code - temp
catVal<-0.33 
wyTempCats<-cbind.data.frame(wyTemp[,1:3],
                             as.data.frame(lapply(wyTemp[, 4:8],
                                                  cut, br=c(0,(catVal),(catVal*2),Inf),
                                                  labels=c("cool","avg","warm"))))

wyTempCatsLong<-tidyr::gather(wyTempCats, cluster,cat, 4:8)
wyTempCatsLong$cat<-factor(wyTempCatsLong$cat, levels=c("cool","avg","warm"))

p2<-ggplot(wyTempCatsLong, aes(x = year, y = as.factor(cluster), fill = cat)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = c("blue", "white", "red"), name = "Temp Cat", 
                    guide = guide_legend(reverse = TRUE))+
  #geom_text(aes(label = prec), color = "black", size = 4) +
  #facet_wrap(.~cluster, nrow=5)+
  #coord_fixed()+
  scale_x_continuous(breaks=seq(1900,2020,10))+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Water Year Mean Temp Percentile Categories")

cowplot::plot_grid(p1, p2, labels = "AUTO", align = "v",ncol = 1)

# combined temp/precip
wyCatsLong<-merge(wyTempCatsLong, wyPrecCatsLong, by=c('dates','cluster'))
wyCatsLong$code<-paste0(wyCatsLong$cat.x,"-",wyCatsLong$cat.y)

# load custom pallete
load("./data/colorPal.RData")
wyCatsLong<-merge(wyCatsLong,colorsTP,by="code")

p3<-ggplot(subset(wyCatsLong,prec=="dry"), aes(x = year.x, y = as.factor(cluster), fill = code)) +
  geom_tile(color = "black") +
  # scale_fill_manual(values = colorsTP[order(colorsTP$code),"rgbComb"], name = "Climate Cat", 
  #                   guide = guide_legend(reverse = TRUE))+
  scale_fill_manual(values = c("darkgoldenrod1","lightgoldenrod1","brown"), name="Drought Cat")+
  
  #geom_text(aes(label = prec), color = "black", size = 4) +
  #facet_wrap(.~cluster, nrow=5)+
  #coord_fixed()+
  scale_x_continuous(breaks=seq(1900,2020,10))+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Water Year Climate Percentile Categories")

cowplot::plot_grid(p1, p2,p3, labels = "AUTO", align = "v",ncol = 1)

# ggplot(wyCatsLong, aes(x=year.x, y=as.factor(cluster))) + 
#   geom_tile(aes(fill=rgbComb), color="grey") + 
#   scale_fill_identity()


##### Count tables
# all cluster heat map 

#spiCatLong<-tidyr::spread(spiCatLong[,c(2:5)], month, cat)
#colnames(spiCatLong)[3:4]<-c("Cool","Warm")
wyCatsLong$wyCode<-paste0(wyCatsLong$temp,"-",wyCatsLong$prec)  

##### running counts of warm-dry categories -----
temp<-wyCatsLong
#temp$warmXdry<-ifelse(temp$wyCode=="warm-dry", 1,0)
temp$warmXdry<-ifelse(temp$temp=="warm", 1,0)
temp<-temp %>%
  #group_by(cluster) %>%
  arrange(cluster,year.x) %>%
  mutate(WarmDryCt = zoo::rollapply(warmXdry,10,sum, fill = NA, align = "right"))

ggplot(temp, aes(year.x,WarmDryCt, color=cluster))+
  geom_line()+
  ggtitle("10-year Moving Sum of Warm Years")
#####  

# codeCounts<- wyCatsLong %>% group_by(cluster, wyCode) %>%
#   summarise(count=n()) %>%
#   spread(cluster,count, fill=0)

temp<-subset(wyCatsLong, year.x>=1991)

codeCountsDF<- temp %>% group_by(cluster, wyCode) %>%
  summarise(count=n(),
            percTotal=round(n()/127*100,0),
            temp=first(temp),
            prec=first(prec))

ggplot(codeCountsDF, aes(x = temp, y = prec, fill = percTotal)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "white", high = "red")+
  geom_text(aes(label = percTotal), color = "black", size = 4) +
  coord_fixed()+
  facet_wrap(.~cluster)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Water Year Temp/Prec Categories - percent of total year (1991-2022, n=32)")

###############



#####

##### 
# seasonal percentiles
seasPrec<-read_csv("data/csv_5cluster/Cluster5_3mo_total_precip_percentile_PRISM_1895-2022.csv")
seasTemp<-read_csv("data/csv_5cluster/Cluster5_3mo_mean_temp_percentile_PRISM_1895-2022.csv")

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
seasPrecLong$seas<-seasPrecLong$month
seasPrecLong<- seasPrecLong %>% mutate(seas = case_match(month, 
                                                         "12" ~ "OND", 
                                                         "3" ~ "JFM",
                                                         "6" ~ "AMJ",
                                                         "9" ~ "JAS",
                                                         .default = seas))
#seasPrecLong$seas<-factor(seasPrecLong$seas, levels=c("OND","JFM","AMJ","JAS"))
seasPrecLong$seas<-factor(seasPrecLong$seas, levels=rev(c("OND","JFM","AMJ","JAS")))


p1<-ggplot(seasPrecLong, aes(x = year, y = cluster, fill = prec)) + # witch seas/cluster
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "brown", mid="white",high = "green", midpoint = 0.5)+
  #geom_text(aes(label = prec), color = "black", size = 4) +
  #facet_wrap(cluster~., nrow=5)+
  facet_grid(seas~.)+ # switch seas/cluster
  #coord_fixed()+
  scale_x_continuous(breaks=seq(1900,2020,10))+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Seasonal Total Precip Percentiles")

# convert to long for plotting - temp
seasTempLong<-tidyr::gather(seasTemp, cluster,temp, 4:8)
seasTempLong$month<-factor(seasTempLong$month, levels=c(12,3,6,9))
seasTempLong$seas<-seasTempLong$month
seasTempLong<- seasTempLong %>% mutate(seas = case_match(month, 
                                                         "12" ~ "OND", 
                                                         "3" ~ "JFM",
                                                         "6" ~ "AMJ",
                                                         "9" ~ "JAS",
                                                         .default = seas))
#seasPrecLong$seas<-factor(seasPrecLong$seas, levels=c("OND","JFM","AMJ","JAS"))
seasTempLong$seas<-factor(seasTempLong$seas, levels=rev(c("OND","JFM","AMJ","JAS")))


p2<-ggplot(seasTempLong, aes(x = year, y = cluster, fill = temp)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "blue", mid="white",high = "red", midpoint = 0.5)+
  #geom_text(aes(label = prec), color = "black", size = 4) +
  #facet_wrap(.~cluster, nrow=5)+
  facet_grid(seas~.)+
  #coord_fixed()+
  scale_x_continuous(breaks=seq(1900,2020,10))+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Seasonal Mean Temp Percentiles")

cowplot::plot_grid(p1, p2, labels = "AUTO", align = "v",ncol = 1)

## CATEGORIES
## seasons categorical counts - prec 
percThresh<-0.33
catVal<-percThresh 
seasPrecCats<-cbind.data.frame(seasPrec[,1:3],
                               as.data.frame(lapply(seasPrec[, 4:8],
                                                    cut, br=c(0,(catVal),(1-catVal),Inf),
                                                    labels=c("dry","avg","wet"))))

seasPrecCatsLong<-tidyr::gather(seasPrecCats, cluster,cat, 4:8)
seasPrecCatsLong$cat<-factor(seasPrecCatsLong$cat, levels=c("dry","avg","wet"))
seasPrecCatsLong$month<-factor(seasPrecCatsLong$month, levels=c(12,3,6,9))
# refactor to seasons
seasPrecCatsLong$seas<-seasPrecCatsLong$month
seasPrecCatsLong<- seasPrecCatsLong %>% mutate(seas = case_match(month, 
                                                                 "12" ~ "OND", 
                                                                 "3" ~ "JFM",
                                                                 "6" ~ "AMJ",
                                                                 "9" ~ "JAS",
                                                                 .default = seas))
#seasPrecCatsLong$seas<-factor(seasPrecCatsLong$seas, levels=c("OND","JFM","AMJ","JAS"))
seasPrecCatsLong$seas<-factor(seasPrecCatsLong$seas, levels=(c("OND","JFM","AMJ","JAS")))


p1<-ggplot(seasPrecCatsLong, aes(x = year, y = as.factor(cluster), fill = cat)) + # change y to cluster/month
  geom_tile(color = "black") +
  scale_fill_manual(values = c("brown", "white", "green"), name = "Precip Cat", 
                    guide = guide_legend(reverse = TRUE))+
  #geom_text(aes(label = prec), color = "black", size = 4) +
  #facet_wrap(cluster~., nrow=5)+
  facet_grid((seas~.))+ # change to cluster/month
  #coord_fixed()+
  scale_x_continuous(breaks=seq(1900,2020,10))+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Seasonal Total Precip Percentiles Categories")


# temperatures
catVal<-percThresh 
seasTempCats<-cbind.data.frame(seasTemp[,1:3],
                               as.data.frame(lapply(seasTemp[, 4:8],
                                                    cut, br=c(0,(catVal),(1-catVal),Inf),
                                                    labels=c("cool","avg","warm"))))

seasTempCatsLong<-tidyr::gather(seasTempCats, cluster,cat, 4:8)
seasTempCatsLong$cat<-factor(seasTempCatsLong$cat, levels=c("cool","avg","warm"))
seasTempCatsLong$month<-factor(seasTempCatsLong$month, levels=c(12,3,6,9))
# refactor to seasons
seasTempCatsLong$seas<-seasTempCatsLong$month
seasTempCatsLong<- seasTempCatsLong %>% mutate(seas = case_match(month, 
                                                                 "12" ~ "OND", 
                                                                 "3" ~ "JFM",
                                                                 "6" ~ "AMJ",
                                                                 "9" ~ "JAS",
                                                                 .default = seas))
#seasTempCatsLong$seas<-factor(seasTempCatsLong$seas, levels=c("OND","JFM","AMJ","JAS"))
seasTempCatsLong$seas<-factor(seasTempCatsLong$seas, levels=(c("OND","JFM","AMJ","JAS")))

p2<-ggplot(seasTempCatsLong, aes(x = year, y = as.factor(cluster), fill = cat)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = c("blue", "white", "red"), name = "Temp Cat", 
                    guide = guide_legend(reverse = TRUE))+
  #geom_text(aes(label = prec), color = "black", size = 4) +
  #facet_wrap(cluster~., nrow=5)+
  facet_grid(seas~.)+
  #coord_fixed()+
  scale_x_continuous(breaks=seq(1900,2020,10))+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Seasonal Mean Temp Percentiles Categories")

cowplot::plot_grid(p1, p2, labels = "AUTO", align = "v",ncol = 1)

# combined seasonal plots
seasCatsLong<-merge(seasTempCatsLong, seasPrecCatsLong, by=c('dates','cluster'))
seasCatsLong$code<-paste0(seasCatsLong$cat.x,"-",seasCatsLong$cat.y)
seasCatsLong$month.x<-factor(seasCatsLong$month.x, levels=c(12,3,6,9))

# load custom pallete
load("./data/colorPal.RData")
seasCatsLong<-merge(seasCatsLong,colorsTP,by="code")

p3<-ggplot(seasCatsLong, aes(x = year.x, y = as.factor(cluster), fill = code)) + # change y to month.x vs cluster
  geom_tile(color = "black") +
  scale_fill_manual(values = colorsTP[order(colorsTP$code),"rgbComb"], name = "Climate Cat", 
                    guide = guide_legend(reverse = TRUE))+
  #scale_fill_manual(values = c("darkgoldenrod1","lightgoldenrod1","brown"), name="Drought Cat")+
  #geom_text(aes(label = prec), color = "black", size = 4) +
  #facet_wrap(.~cluster, nrow=5)+
  #coord_fixed()+
  scale_x_continuous(breaks=seq(1900,2020,10))+
  facet_grid(seas.x~.)+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Seasonal Climate Percentile Categories")

cowplot::plot_grid(p1, p2,p3, labels = "AUTO", align = "v",ncol = 1)


##### counts of climate percentile cats by period
# seasonal count tables

catCounts<- seasCatsLong %>% group_by(code, year.x) %>%
  summarize(count=n(),
            rgbcode=first(rgbComb))
ggplot(catCounts, aes(year.x,count,fill=code))+
  geom_bar(stat="identity")+
  scale_fill_manual(values = colorsTP[order(colorsTP$code),"rgbComb"], name = "Climate Cat", 
                    guide = guide_legend(reverse = TRUE))+
  scale_x_continuous(breaks=seq(1900,2020,10))

length(unique(seasCatsLong$year.x))/8
seq(1896+16,2022,by=16)
seasCatsLong$period<-cut(seasCatsLong$year.x, breaks = c(seq(1895,2022,by=16),2022), dig.lab=4)
seasCatsLong$period<-gsub(']', '', seasCatsLong$period)
seasCatsLong$period<-gsub('\\(', '', seasCatsLong$period)
seasCatsLong$period<-gsub(',', '-', seasCatsLong$period)
seasCatsLong$period<-as.factor(seasCatsLong$period)
levels(seasCatsLong$period)

catCounts<- seasCatsLong %>% group_by(code, period) %>%
  summarize(count=n(),
            rgbcode=first(rgbComb))
ggplot(catCounts, aes(x=period,y=count,fill=code))+
  geom_bar(stat="identity")+
  #geom_bar(position="fill")+
  scale_fill_manual(values = colorsTP[order(colorsTP$code),"rgbComb"], name = "Climate Cat", 
                    guide = guide_legend(reverse = TRUE))

save(catCounts, colorsTP, file="data/catCounts.RData")

# only warm 
ggplot(subset(catCounts, code %in% c("warm-wet","warm-dry","warm-avg")), aes(x=period,y=(count/(16*4*5)*100),fill=code))+
  geom_bar(stat="identity")+
  #geom_bar(position="fill")+
  scale_fill_manual(values = colorsTP[order(colorsTP$code),"rgbComb"][7:9], name = "Climate Cat", 
                    guide = guide_legend(reverse = TRUE))+
  ylab("% occurrence")


#####

# drought time series
ggplot(subset(seasCatsLong,cat.y=="dry"), aes(dates,cluster, fill=as.factor(code)))+
  #geom_point(shape=21, alpha=0.75)+
  geom_tile()+
  #scale_fill_manual(values = c("#fdbf6f","#6a3d9a","#b15928"), name="Drought Cat")+
  scale_fill_manual(values = c("#b15928","#b15928","#b15928"), name="Drought Cat")+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  #facet_grid(cluster~.)+
  theme_bw()+
  ggtitle("Seasonal Drought Sequences")


##### seasonal count tables
seasCatsLong$seasCode<-paste0(seasCatsLong$temp,"-",seasCatsLong$prec)  

##### running counts of warm-dry categories -----
temp<-seasCatsLong
temp$warmXdry<-ifelse(temp$seasCode=="warm-dry", 1,0)
#temp$warmXdry<-ifelse(temp$temp=="warm", 1,0)
temp<-temp %>%
  #group_by(cluster) %>%
  arrange(cluster,month.x,year.x) %>%
  mutate(WarmDryCt = zoo::rollapply(warmXdry,10,sum, fill = NA, align = "right"))

ggplot(temp, aes(year.x,WarmDryCt, color=seas.x))+
  geom_line()+
  facet_wrap(.~cluster)+
  ggtitle("10-year Moving Sum of Warm-Dry Years by Season/Cluster")
#####  

# codeCounts<- wyCatsLong %>% group_by(cluster, wyCode) %>%
#   summarise(count=n()) %>%
#   spread(cluster,count, fill=0)

temp<-seasCatsLong
#temp<-subset(seasCatsLong, year.x>=1991)

codeCountsDF<- temp %>% group_by(cluster, seasCode, month.x) %>%
  summarise(count=n(),
            percTotal=round(n()/127*100,0),
            percAnom=round(n()/127*100,0)-11,
            temp=first(temp),
            prec=first(prec))

ggplot(codeCountsDF, aes(x = temp, y = prec, fill = percTotal)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "white", high = "red")+
  #scale_fill_gradient2(low = "blue",mid="white", high = "red", midpoint=0)+
  geom_text(aes(label = percTotal), color = "black", size = 4) +
  coord_fixed()+
  facet_wrap(month.x~cluster)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Water Year Temp/Prec Categories - percent of total (1896-2022)")


ggplot(codeCountsDF, aes(x = temp, y = prec, fill = percAnom)) +
  geom_tile(color = "black") +
  #scale_fill_gradient(low = "white", high = "red")+
  scale_fill_gradient2(low = "blue",mid="white", high = "red", midpoint=0)+
  geom_text(aes(label = percAnom), color = "black", size = 4) +
  coord_fixed()+
  facet_wrap(month.x~cluster)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Water Year Temp/Prec Categories - percent of total anomaly (1896-2022)")

# seasonal precip sequences
temp<-subset(seasPrecCatsLong,cat!="avg")
seasPrecCatsWide<-tidyr::spread(temp[,3:6], seas, cat)
seasPrecCatsWide<-seasPrecCatsWide[,c(1,2,6,5,4,3)]
seasPrecCatsWide$seasCode<-paste0(seasPrecCatsWide$JFM,"-",seasPrecCatsWide$JAS)

WintSumrTable<-as.data.frame(table(seasPrecCatsWide$cluster,seasPrecCatsWide$seasCode))

ggplot(WintSumrTable, aes(Var2,Freq))+
  geom_bar(stat = "identity", position="dodge")+
  facet_wrap(.~Var1)

# ENSO values
enso<-read.table("https://www.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/detrend.nino34.ascii.txt", header = TRUE)
enso$Date<-as.Date(paste0(enso$YR,"-",enso$MON,"-01"))

ensoSeas<-cbind.data.frame(enso[,c(6,2,1)],zoo::rollapply(enso[,"ANOM"], FUN = mean, width = 3,
                                                          fill=NA,align="right", by.column = TRUE))
colnames(ensoSeas)[4]<-"ONI"

