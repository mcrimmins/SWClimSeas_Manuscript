# SPI/SPEI transitions for cluster time series
# MAC 06/28/23

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

#spi<-read_csv("data/csv_5cluster/Cluster5_3mo_SPI_PRISM_1895-2022.csv")
#spi<-read_csv("data/csv_5cluster/Cluster5_3mo_SPEI_PRISM_1895-2022.csv")
spi<-read_csv("data/csv_5cluster/Cluster5_3mo_total_precip_percentile_PRISM_1895-2022.csv")
spi<-subset(spi, year>1895)

spiSeas<-subset(spi, month %in% c(3,6,9,12))

# plot time series
# spiSeasLong<-tidyr::gather(spiSeas, cluster,value, 4:8)
# ggplot(spiSeasLong, aes(dates,value, color=as.factor(month)))+
#   #geom_bar(stat = "identity", position="dodge")+
#   geom_line()+
#   facet_wrap(.~cluster)
  



# relate SPI to percentile
qnorm(0.3333333); qnorm(0.6666666)
spiVal<-0.33 

spiCats<-cbind.data.frame(spiSeas[,1:3],
                          as.data.frame(lapply(spiSeas[, 4:8],
                          cut, br=c(-Inf,-(spiVal),(spiVal),Inf),
                          labels=c("dry","avg","wet"))))

# percent rank code
spiVal<-0.33 
spiCats<-cbind.data.frame(spiSeas[,1:3],
                          as.data.frame(lapply(spiSeas[, 4:8],
                                               cut, br=c(0,(spiVal),(spiVal*2),Inf),
                                               labels=c("dry","avg","wet"))))

# ggplot(spiCats, aes(dates,cluster1))+
#   geom_point()

# look at winter-spring-summer sequencing
spiCats<-subset(spiCats, month %in% c(3,6,9))

spiCatLong<-tidyr::gather(spiCats, cluster,cat, 4:8)

spiCatLong<-tidyr::spread(spiCatLong[,c(2:5)], month, cat)
  colnames(spiCatLong)[3:5]<-c("winter","spring","summer")
spiCatLong$seasCode<-paste0(spiCatLong$winter,"-",spiCatLong$spring,"-",spiCatLong$summer)  
  
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
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#####
# just extreme values 
spiCatLongExt<-tidyr::gather(spiCats, cluster,cat, 4:8)
#spiCatLongExt<-subset(spiCatLongExt, cat!="avg")

spiCatLongExt<-tidyr::spread(spiCatLongExt[,c(2:5)], month, cat)
colnames(spiCatLongExt)[3:5]<-c("Winter","Spring","Summer")
spiCatLongExt$seasCode<-paste0(spiCatLongExt$Winter,"-",spiCatLongExt$Spring,"-",spiCatLongExt$Summer)  
# subset out avgs
spiCatLongExt<-subset(spiCatLongExt, Winter!="avg")
spiCatLongExt<-subset(spiCatLongExt, Spring!="avg")
spiCatLongExt<-subset(spiCatLongExt, Summer!="avg")

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
  ggtitle("Winter-Spring-Summer Season Extremes Sequences (lower/upper terciles)")

ggplot(spiCatLongExt, aes(year,seasCode))+
  geom_point()+
  facet_wrap(.~cluster)+
  ggtitle("Winter-Spring-Summer Season Extremes Sequences (lower/upper terciles)")



ggplot(spiCatLong, aes(year,seasCode))+
  geom_point()+
  facet_wrap(.~cluster)


