# SPI/SPEI transitions for cluster time series
# MAC 06/28/23

library(ggplot2)
library(dplyr)
library(tidyr)


#spi<-readr::read_csv("data/csv/Cluster_6mo_SPI_PRISM_1895-2022.csv")
spi<-readr::read_csv("data/csv/Cluster_6mo_SPEI_PRISM_1895-2022.csv")
spi<-subset(spi, year>1895)

# subset to cool/warm season
spiSeas<-subset(spi, month %in% c(3,9))


spiCats<-cbind.data.frame(spiSeas[,1:3],
                          as.data.frame(lapply(spiSeas[, 4:9],
                          cut, br=c(-Inf,-1,1,Inf),
                          labels=c("dry","avg","wet"))))

# ggplot(spiCats, aes(dates,cluster1))+
#   geom_point()

# look at winter-spring-summer sequencing
#spiCats<-subset(spiCats, month %in% c(3,6,9))

spiCatLong<-tidyr::gather(spiCats, cluster,cat, 4:9)
spiCatLong$seas<-ifelse(spiCatLong$month==3,"Cool","Warm")
spiCatLong$seasCode<-paste0(spiCatLong$seas,"-",spiCatLong$cat)

# all cluster heat map 
spiCatLong<-tidyr::spread(spiCatLong[,c(2:5)], month, cat)
  colnames(spiCatLong)[3:4]<-c("Cool","Warm")
spiCatLong$seasCode<-paste0(spiCatLong$Cool,"-",spiCatLong$Warm)  
  
codeCounts<- spiCatLong %>% group_by(cluster, seasCode) %>%
                      summarise(count=n()) %>%
                      spread(cluster,count, fill=0)

codeCountsDF<- spiCatLong %>% group_by(cluster, seasCode) %>%
  summarise(count=n())

ggplot(codeCountsDF, aes(x = seasCode, y = cluster, fill = count)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "white", high = "red")+
  geom_text(aes(label = count), color = "black", size = 4) +
  coord_fixed()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Cool-Warm Season Sequences")

ggplot(spiCatLong, aes(year,seasCode))+
  geom_point()+
  facet_wrap(.~cluster)

# heat map by cluster
codeCountsDF<- spiCatLong %>% group_by(cluster, seasCode) %>%
  summarise(count=n())

codeCountsDF <- codeCountsDF %>% separate(seasCode, c('Cool', 'Warm'), sep="-")
  codeCountsDF$Cool<-factor(codeCountsDF$Cool, levels=c("dry","avg","wet"))
  #levels(codeCountsDF$Cool)<-c("dry","avg","wet")
  codeCountsDF$Warm<-factor(codeCountsDF$Warm, levels=c("dry","avg","wet"))
  #levels(codeCountsDF$Warm)<-c("dry","avg","wet")
  
ggplot(codeCountsDF, aes(x = Cool, y = Warm, fill = count)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "white", high = "red")+
  geom_text(aes(label = count), color = "black", size = 4) +
  facet_wrap(.~cluster)+
  coord_fixed()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Cool-Warm Season Sequences")

codeCountsDF$anom<-codeCountsDF$count-round((ifelse(codeCountsDF$Cool=="avg",0.68, 0.16)*
                        ifelse(codeCountsDF$Warm=="avg",0.68, 0.16))*127,0)
  
ggplot(codeCountsDF, aes(x = Cool, y = Warm, fill = anom)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "blue",mid="white",high = "red", midpoint = 0)+
  geom_text(aes(label = anom), color = "black", size = 4) +
  facet_wrap(.~cluster)+
  coord_fixed()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Cool-Warm Season Sequences (Count Anomalies)")

#####
# just extreme values 
spiCatLongExt<-tidyr::gather(spiCats, cluster,cat, 4:9)
#spiCatLongExt<-subset(spiCatLongExt, cat!="avg")

spiCatLongExt<-tidyr::spread(spiCatLongExt[,c(2:5)], month, cat)
colnames(spiCatLongExt)[3:4]<-c("Cool","Warm")
spiCatLongExt$seasCode<-paste0(spiCatLongExt$Cool,"-",spiCatLongExt$Warm)  
# subset out avgs
spiCatLongExt<-subset(spiCatLongExt, Cool!="avg")
spiCatLongExt<-subset(spiCatLongExt, Warm!="avg")

codeCounts<- spiCatLongExt %>% group_by(cluster, seasCode) %>%
  summarise(count=n()) %>%
  spread(cluster,count, fill=0)

codeCountsDF<- spiCatLongExt %>% group_by(cluster, seasCode) %>%
  summarise(count=n())

ggplot(codeCountsDF, aes(x = seasCode, y = cluster, fill = count)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "white", high = "red")+
  geom_text(aes(label = count), color = "black", size = 4) +
  coord_fixed()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Cool-Warm Season Extremes Sequences")

ggplot(spiCatLongExt, aes(year,seasCode))+
  geom_point()+
  facet_wrap(.~cluster)+
  ggtitle("Cool-Warm Season Extremes Sequences")

