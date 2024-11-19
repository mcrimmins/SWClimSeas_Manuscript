# spatial clustering on monthly average precip
# 02/04/23 MAC

# more on model based clustering
# https://en.proft.me/2017/02/1/model-based-clustering-r/

library(raster)
library(mclust)
library(dplyr)
library(rasterVis)
library(viridis)

# set rasteroptions
rasterOptions(progress = 'text')

# load data
# watershed boundaries
huc4<-rgdal::readOGR(dsn = "~/RProjects/SOMs/monsoonPrecip/shapes/wbdhu4_a_us_september2019/wbdhu4_a_us_september2019.gdb", layer="WBDHU4")
SWhucs<-subset(huc4, HUC4 %in% c("1504","1506","1502","1302","1408"))
# try other polygons...MLRAs, ecoregions?
states <- getData('GADM', country='United States', level=1)
# mlra
mlra<-rgdal::readOGR(dsn = "~/RProjects/LivnehDrought/shapes/mlra/", layer="mlra_v42")

# dates 1895-2022 PRISM data
dates=seq(as.Date("1895-01-01"), as.Date("2022-12-31"), by="month")
dates<-as.data.frame(dates)
dates$month<-as.numeric(format(dates$dates, "%m"))
dates$year<-as.numeric(format(dates$dates, "%Y"))
length(unique(dates$year))

# update scratch dir with PRISM data 1895-2022 monthly precip/mean temp
# use ~/RProjects/PRISMDownload/monthyDownloadPRISM.R
# process to subset using ~/RProjects/WinterSummerPrecip/processPRISM.R
prec<-stack("/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_prec_1895_2022.grd")
tmean<-stack("/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_tmean_1895_2022.grd")
# add in PET from monthlyHargreavesPETraster.R
pet<-stack("/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_hargreaves_1895_2022.grd")

# nClimGrid precip - use processnClimGrid.R
#prec<-stack("/scratch/crimmins/climgrid/processed/sw/SWmonthly_nClimGrid_prec_1895_2022.grd")



# trim shapes to prec
SWmlras<-crop(mlra, extent(prec))
SWhucs<-crop(huc4, extent(prec))

# monthly averages - prec
monthSum<- stackApply(prec, dates$month, fun = sum)
moAvgPrec<-monthSum/length(unique(dates$year))
names(moAvgPrec)<-month.abb
# monthly averages - PET
monthSum<- stackApply(pet, dates$month, fun = sum)
moAvgPET<-monthSum/length(unique(dates$year))
names(moAvgPET)<-month.abb

#rm(prec, pet)

# use monthly scaled data to perc of annual total
moScalePrec<-calc(moAvgPrec, fun=function(x){(x/sum(x))*100})
names(moScalePrec)<-paste0(month.abb,"Prec")
moScalePET<-calc(moAvgPET, fun=function(x){(x/sum(x))*100})
names(moScalePET)<-paste0(month.abb,"PET")  
plot(moScalePrec, colNA="red")

cols <- colorRampPalette(RColorBrewer::brewer.pal(9,"YlGn"))
rasterVis::levelplot(moScalePrec, col.regions=cols,  main="Monthly % of Annual Total Precip")+
  latticeExtra::layer(sp.polygons(states, fill=NA, col = "grey40"))

clusterGrid<-moScalePrec

# crop to smaller area? AZ/NM 
clusterGrid<-crop(clusterGrid, extent(-114.88,-102.9,31.22,37.08))
#plot(clusterGrid, colNA="red")
# rasterVis::levelplot(clusterGrid,
#                      par.settings=rasterTheme(viridis_pal(option = "D")(255)),
#                      margin=FALSE, main="Monthly Percent of Annual Total")+
#   latticeExtra::layer(sp.polygons(states, fill=NA, col = "grey88"))

# add in lat/lon rasters for clusters
#lon <- init(clusterGrid[[1]], 'x')*0.5
#lat <- init(clusterGrid[[1]], 'y')*0.5
#clusterGrid<-stack(clusterGrid,lat,lon)


# clustering
library(RStoolbox)
library(parallel)
library(rasterVis)

rsOpts(verbose=TRUE)

# cluster data
ptm <- proc.time()
set.seed(1234)
clusterN<-5
#unC <- unsuperClass(clusterGrid, nSamples = 5000000, nClasses = clusterN, nStarts = 25, nIter = 1000, norm = FALSE, output = 'distances') # or allGDD
unC <- unsuperClass(clusterGrid, clusterMap=FALSE, nClasses = clusterN, nStarts = 50, nIter = 5000, norm = FALSE, output = 'classes', algorithm = "Hartigan-Wong") # or allGDD
#unC <- unsuperClass(clusterGrid, clusterMap=FALSE, nClasses = clusterN, nStarts = 50, nIter = 5000, norm = FALSE, output = 'distances') # or allGDD

proc.time() - ptm

# random colors -- https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
qual_col_pals = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
darkcols<-sample(col_vector, clusterN)

classMap<-as.factor(unC$map)
#classMap<-unC$map
rat <- levels(classMap)[[1]]
# cluster names
# rat[["cluster"]]<-rev(c("Eastside","Oro Valley","Rita Ranch","Gates Pass",
#                         "Twin Peaks","Saddlebrook","Foothills","Central"))

rat[["cluster"]] <- as.character(seq(1, clusterN, by=1))
levels(classMap) <- rat 

# save classified map
#writeRaster(classMap, filename = "./data/classMap5.grd", overwrite=TRUE)

# plot classified map
rasterVis::levelplot(classMap, col.regions=darkcols, par.settings=list(panel.background=list(col="white")),
                     margin=FALSE, main="Precip Season Clusters")+
  latticeExtra::layer(sp.polygons(states, fill=NA, col = "grey40"))+
  # layer(sp.polygons(huc4, fill=NA, col = "black"))
  latticeExtra::layer(sp.polygons(SWhucs, fill=NA, col = "black",lwd=0.5))
#latticeExtra::layer(sp.polygons(SWmlras, fill=NA, col = "black",lwd=0.5))
# lty=2, http://www.sthda.com/english/wiki/line-types-in-r-lty 

#test <- focal(classMap, w=matrix(3, 3, 1), min, na.rm=TRUE)
#plot(test)

# library(viridis)
# rasterVis::levelplot(unC$map, 
#                      par.settings=rasterTheme(viridis_pal(option = "D",direction=-1,)(255)),
#                      main="Precip Cluster Distances")+
#   latticeExtra::layer(sp.polygons(states, fill=NA, col = "grey40"))+
#   # layer(sp.polygons(huc4, fill=NA, col = "black"))
#   latticeExtra::layer(sp.polygons(SWhucs, fill=NA, col = "black",lwd=0.5))


# cluster centers - months
moCluster<-as.data.frame(unC$model$centers)
colnames(moCluster)<-seq(1,12,1)
moCluster$cluster<-seq(1,nrow(moCluster),1)
moCluster <- tidyr::gather(moCluster, month, precip, 1:12, factor_key=TRUE)
moCluster$month<-as.numeric(moCluster$month)

# plot monthly data
library(ggplot2)

ggplot(moCluster, aes(month,precip))+
  geom_bar(stat = "identity")+
  scale_x_continuous(name="Month", breaks=c(seq(1,12,1)))+
  ylab("% of annual total")+
  facet_wrap(.~cluster)+
  ggtitle("K-mean cluster centers - Percent of annual total by month")+
  theme_bw()

ggplot(moCluster, aes(month,precip, color=as.factor(cluster)))+
  geom_line()+
  geom_point(size=2)+
  scale_color_manual(values=c("grey","#7570B3","#B2DF8A","#8DD3C7","#80B1D3"), name="region")+
  scale_x_continuous(breaks=seq(1,12,1))+
  #ggtitle("K-mean cluster centers - Percent of annual total by month")+
  ylab("% of annual total precip")+
  theme_bw()

#####
# zonal summary of scaled PET and Precip
zPET<-as.data.frame(zonal(moScalePET, classMap, fun='mean', na.rm=TRUE))
colnames(zPET)[2:13]<-1:12
moPETz <- tidyr::gather(zPET, month, value, 2:13, factor_key=TRUE)
moPETz$var<-"PET"
moPETz$month<-as.numeric(moPETz$month)

ggplot(moPETz, aes(month,value, color=as.factor(zone)))+
  geom_line()+
  geom_point()+
  #ggtitle("K-mean cluster centers - Percent of annual total by month")+
  theme_bw()

zPPT<-as.data.frame(zonal(moScalePrec, classMap, fun='mean', na.rm=TRUE))
colnames(zPPT)[2:13]<-1:12
moPPTz <- tidyr::gather(zPPT, month, value, 2:13, factor_key=TRUE)
moPPTz$var<-"PPT"
moPPTz$month<-as.numeric(moPPTz$month)

ggplot(moPPTz, aes(month,value, color=as.factor(zone)))+
  geom_line()+
  geom_point()+
  #ggtitle("K-mean cluster centers - Percent of annual total by month")+
  theme_bw()  

moClim<-rbind.data.frame(moPETz,moPPTz)

ggplot(moClim, aes(month,value, color=as.factor(var)))+
  geom_line()+
  geom_point()+
  #ggtitle("K-mean cluster centers - Percent of annual total by month")+
  scale_x_continuous(name="Month", breaks=c(seq(1,12,1)))+
  ylab("% of annual total")+
  facet_wrap(.~zone)+
  ggtitle("K-mean cluster centers - Percent of annual total by month")+
  theme_bw() 
#####

#####
# zonal summary of monthly avg PET and Precip
zPET<-as.data.frame(zonal(moAvgPET, classMap, fun='mean', na.rm=TRUE))
colnames(zPET)[2:13]<-1:12
moPETz <- tidyr::gather(zPET, month, value, 2:13, factor_key=TRUE)
moPETz$var<-"PET"
moPETz$month<-as.numeric(moPETz$month)

ggplot(moPETz, aes(month,value, color=as.factor(zone)))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept=quantile(moPETz$value, 0.5))+
  #geom_hline(yintercept=quantile(moPETz$value, 0.33))+
  #geom_hline(yintercept=quantile(moPETz$value, 0.66))+
  geom_vline(xintercept = 4)+
  geom_vline(xintercept = 10)+
  scale_x_continuous(name="Month", breaks=c(seq(1,12,1)))+
  ylab("Avg PET (mm)")+
  #ggtitle("K-mean cluster centers - Percent of annual total by month")+
  theme_bw()

# define seasons based on quintiles?



zPPT<-as.data.frame(zonal(moAvgPrec, classMap, fun='mean', na.rm=TRUE))
colnames(zPPT)[2:13]<-1:12
moPPTz <- tidyr::gather(zPPT, month, value, 2:13, factor_key=TRUE)
moPPTz$var<-"PPT"
moPPTz$month<-as.numeric(moPPTz$month)

ggplot(moPPTz, aes(month,value, color=as.factor(zone)))+
  geom_line()+
  geom_point()+
  #ggtitle("K-mean cluster centers - Percent of annual total by month")+
  theme_bw()  

moClim<-rbind.data.frame(moPETz,moPPTz)

ggplot(moClim, aes(month,value, color=as.factor(var)))+
  geom_line()+
  geom_point()+
  #ggtitle("K-mean cluster centers - Percent of annual total by month")+
  scale_x_continuous(name="Month", breaks=c(seq(1,12,1)))+
  ylab("mm")+
  facet_wrap(.~zone)+
  ggtitle("K-mean cluster centers - Monthly Avg PET/Precip (mm)")+
  theme_bw() 

# mo water balance
#zBal<-as.data.frame(zPPT-zPET)
zBal<-as.data.frame(zPPT/zPET)
zBal$zone<-1:nrow(zBal)
moBalZ <- tidyr::gather(zBal, month, value, 2:13, factor_key=TRUE)
moBalZ$var<-"Balance"
moBalZ$month<-as.numeric(moBalZ$month)

ggplot(moBalZ, aes(month,value, color=as.factor(var)))+
  geom_line()+
  geom_point()+
  #ggtitle("K-mean cluster centers - Percent of annual total by month")+
  scale_x_continuous(name="Month", breaks=c(seq(1,12,1)))+
  ylab("Aridity Index (P/PET)")+
  facet_wrap(.~zone)+
  ggtitle("K-mean cluster centers - Monthly Aridity Index (P/PET)")+
  geom_hline(yintercept = 1)+
  theme_bw() 


#####

#####
# seasonality indices
# https://github.com/feng-ecohydro/seasonality-indices
# define cool/warm and/or spring/fall

zPPT<-as.data.frame(t(zonal(moAvgPrec, classMap, fun='mean', na.rm=TRUE)))
zPPT<-zPPT[2:nrow(zPPT),]
SI<-list() 
for(i in 1:ncol(zPPT)){
  SI[[i]]<-1/sum(zPPT[,i])*sum(abs(zPPT[,i]-sum(zPPT[,i])/12))
}

SI<-do.call(rbind.data.frame,SI)
colnames(SI)[1]<-"Seasonality Index"
SI$cluster<-1:nrow(SI)
SI<-SI[,c(2,1)]

library(kableExtra)
SI %>%
  kbl(caption = "Precipitation Seasonality Index") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  save_kable("SI.png")
#####

##### 
# predict to normals periods...

periods<-list(seq(1901,1991, by=10),
              seq(1930,2020, by=10))

# get map colors
# random colors -- https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
qual_col_pals = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
darkcols<-sample(col_vector, clusterN)

# add to stack in loop
periodClusters<-stack()

# cluster on each subperiod
y<-length(periods[[1]]) 

for(i in 1:y){
  
  # subset precip
  subPrec<-subset(prec, which(dates$year>=periods[[1]][i] & dates$year<=periods[[2]][i]))
  
  subDates<-dates[which(dates$year>=periods[[1]][i] & dates$year<=periods[[2]][i]),]
  # monthly averages
  monthSum<- stackApply(subPrec, subDates$month, fun = sum)
  moAvgPrec<-monthSum/length(unique(subDates$year))
  
  # rescaled monthly averages
  moScalePrec<-calc(moAvgPrec, fun=function(x){(x/sum(x))*100})
  names(moScalePrec)<-paste0(month.abb,"Prec")
  
  predClusters <- predict(unC, moScalePrec, output="classes")
  
  # factorize classMap
  classMap<-as.factor(predClusters)
  
  # cluster names
  rat <- levels(classMap)[[1]]
  rat[["cluster"]] <- as.character(seq(1, clusterN, by=1))
  #rat <- rat[order(as.numeric(centers$cluster)),]
  #rat[["cluster"]]<-centers$cluster
  #rat[["cluster"]] <- as.character(seq(1, clusterN, by=1))
  levels(classMap) <- rat 
  
  # put into stack
  names(classMap)<-paste0("Normals",periods[[1]][i],"-",periods[[2]][i])
  
  periodClusters<-stack(periodClusters,classMap)
  
}

pClust<-rasterVis::levelplot(periodClusters, col.regions=darkcols, par.settings=list(panel.background=list(col="white")),
                             margin=FALSE, main="Precip Season Clusters")+
  latticeExtra::layer(sp.polygons(states, fill=NA, col = "grey40"))+
  # layer(sp.polygons(huc4, fill=NA, col = "black"))
  latticeExtra::layer(sp.polygons(SWhucs, fill=NA, col = "black",lwd=0.5))

png("~/RProjects/WinterSummerPrecip/figs/predictClusters2normals.png", width = 11, height = 8.5, units = "in", res = 300L)
#grid.newpage()
print(pClust, newpage = FALSE)
dev.off()   

#####  

#####
# climate differences between periods
# define periods 
periods<-list(seq(1901,1991, by=10),
              seq(1930,2020, by=10))

zPET<-list()
zPPT<-list()
perList<-list()

j=1
for(i in c(4,10)){
  
  # subset precip
  subPrec<-subset(prec, which(dates$year>=periods[[1]][i] & dates$year<=periods[[2]][i]))
  subPET<-subset(pet, which(dates$year>=periods[[1]][i] & dates$year<=periods[[2]][i]))
  subDates<-dates[which(dates$year>=periods[[1]][i] & dates$year<=periods[[2]][i]),]
  # monthly averages
  monthSum<- stackApply(subPrec, subDates$month, fun = sum)
  moAvgPrec<-monthSum/length(unique(subDates$year))
  monthSum<- stackApply(subPET, subDates$month, fun = sum)
  moAvgPET<-monthSum/length(unique(subDates$year))
  
  # rescaled monthly averages
  #moScale<-calc(moAvgPrec, fun=function(x){(x-mean(x))/sd(x)})
  moScalePET<-calc(moAvgPET, fun=function(x){(x/sum(x))*100})
  moScalePrec<-calc(moAvgPrec, fun=function(x){(x/sum(x))*100})
  #moClusterScale<-calc(moAvgPrec, scale)
  
  print(paste0(periods[[1]][i],"-",periods[[2]][i]))
  perList[[j]]<-paste0(periods[[1]][i],"-",periods[[2]][i])
  
  zPET[[j]]<-as.data.frame(zonal(moAvgPET, classMap, fun='mean', na.rm=TRUE))
  zPPT[[j]]<-as.data.frame(zonal(moAvgPrec, classMap, fun='mean', na.rm=TRUE))
  
  j=j+1
  
}

zPET<-zPET[[2]]-zPET[[1]]
#zPET<-zPET[[2]]
colnames(zPET)[2:13]<-1:12
zPET$zone<-1:nrow(zPET)
moPETz <- tidyr::gather(zPET, month, value, 2:13, factor_key=TRUE)
moPETz$var<-"PET"
moPETz$month<-as.numeric(moPETz$month)

zPPT<-zPPT[[2]]-zPPT[[1]]
#zPPT<-zPPT[[2]]
colnames(zPPT)[2:13]<-1:12
zPPT$zone<-1:nrow(zPPT)
moPPTz <- tidyr::gather(zPPT, month, value, 2:13, factor_key=TRUE)
moPPTz$var<-"PPT"
moPPTz$month<-as.numeric(moPPTz$month)

moClim<-rbind.data.frame(moPETz,moPPTz)

ggplot(moClim, aes(month,value, color=as.factor(var)))+
  geom_line()+
  geom_point()+
  #ggtitle("K-mean cluster centers - Percent of annual total by month")+
  scale_x_continuous(name="Month", breaks=c(seq(1,12,1)))+
  ylab("mm")+
  facet_wrap(.~zone)+
  ggtitle(paste0("K-mean cluster centers - Monthly Avg PET/Precip (mm): ",perList[[2]]," minus ",perList[[1]]))+
  geom_hline(yintercept = 1)+
  theme_bw() 

#####

# time series of monthly water balances? PET, Precip, Temps... 

# get zonal time series of climate values from clusters

precTS<-as.data.frame(t(zonal(prec,classMap, fun="mean",na.rm=TRUE)))
precTS<-precTS[2:nrow(precTS),]
colnames(precTS)<-paste0("cluster",seq(1,clusterN,1))  
# add in dates
precTS<-cbind.data.frame(dates,precTS)
# cut seasons
# dates$seas = cut(dates$month,c(0,3,6,9,12))
# levels(dates$seas) = c("JFM","AMJ","JAS","OND")
precTS$seas<-cut(precTS$month,c(0,3,6,9,12))
levels(precTS$seas) = c("JFM","AMJ","JAS","OND")

# write out csv
#write.csv(precTS,file='Cluster_seasonal_total_precip_PRISM_1895-2022.csv', row.names=FALSE)

# gather into long format
precTSlong <- tidyr::gather(precTS, cluster, precip, 5:10, factor_key=TRUE)
# get monthly seasonal sums
precSeasSum <- precTSlong %>% group_by(year,seas,cluster) %>%
  summarize(precip=sum(precip, na.rm=TRUE))

# calculate monthly anomalies
moPrecAvg<-precTSlong %>% group_by(month,cluster) %>% 
  summarize(avgPrec=mean(precip))
precTSlongAnom <- left_join(precTSlong, moPrecAvg, by = c("month","cluster"))
precTSlongAnom$anom<-precTSlongAnom$precip-precTSlongAnom$avgPrec


# make some plots
library(ggplot2)

ggplot(precSeasSum, aes(year,precip, color=cluster))+
  geom_line()+
  facet_wrap(.~seas)+
  geom_smooth(aes(year, precip, colour=cluster),method = "lm")

ggplot(subset(precSeasSum, seas %in% c("AMJ"))) + 
  geom_line(aes(year, precip, colour=cluster)) +
  ggtitle("AMJ Precip by Cluster") +
  geom_smooth(aes(year, precip, colour=cluster),method = "lm")

ggplot(subset(precTSlong, month %in% c(3,4,5,6,7))) + 
  geom_line(aes(year, precip, colour=as.factor(month))) +
  ggtitle("Spring Precip by Cluster") +
  geom_smooth(aes(year, precip, colour=as.factor(month)),method = "lm")+
  facet_wrap(.~cluster)

ggplot(subset(precTSlongAnom, month %in% c(3,7))) + 
  geom_line(aes(year, anom, colour=as.factor(month))) +
  ggtitle("Spring Precip Anom by Cluster") +
  geom_smooth(aes(year, anom, colour=as.factor(month)),method = "lm")+
  facet_wrap(.~cluster)

