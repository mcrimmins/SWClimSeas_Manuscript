# Testing seasonality clustering
# MAC 12/15/22

library(raster)

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


# dates 1895-2020 PRISM data
dates=seq(as.Date("1895-01-01"), as.Date("2022-12-31"), by="month")
dates<-as.data.frame(dates)
dates$month<-as.numeric(format(dates$dates, "%m"))
dates$year<-as.numeric(format(dates$dates, "%Y"))
length(unique(dates$year))

# update scratch dir with PRISM data 1895-2020 monthly precip/mean temp
# use ~/RProjects/PRISMDownload/monthyDownloadPRISM.R
# process to subset using ~/RProjects/WinterSummerPrecip/processPRISM.R
prec<-stack("/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_prec_1895_2022.grd")

# nClimGrid precip - use processnClimGrid.R
#prec<-stack("/scratch/crimmins/climgrid/processed/sw/SWmonthly_nClimGrid_prec_1895_2020.grd")

# trim extent of shapes
test<-crop(mlra, extent(prec))



# subset precip?
#prec<-subset(prec, which(dates$year>=1991 & dates$year<=2021))

# monthly averages
monthSum<- stackApply(prec, dates$month, fun = sum)
moAvgPrec<-monthSum/length(unique(dates$year))

# seasonal totals/averages
# create 3-month season factors
dates$seas = cut(dates$month,c(0,3,6,9,12))
levels(dates$seas) = c("JFM","AMJ","JAS","OND")

# create 6-month season factors
dates$seas = cut(dates$month,c(0,3,6,9,12))
levels(dates$seas) = c("ONDJFM","AMJJAS","AMJJAS","ONDJFM")


# seas average
seasSum<- stackApply(prec, dates$seas, fun = sum)
seasAvgPrec<-seasSum/length(unique(dates$year))

# perc contribution by seas
seasScale<-calc(seasAvgPrec, fun=function(x){(x/sum(x))*100})
plot(seasScale)

#####
# get HUC values, plot table
# Extract raster values 
r.vals <- extract(seasScale[[1]], SWhucs)
# Use list apply to calculate mean for each polygon
r.mean <- lapply(r.vals, FUN=mean)
HUCAvg<-cbind.data.frame(SWhucs@data$NAME, round(do.call(rbind,r.mean),0))
colnames(HUCAvg)<-c("HUC Name","Avg ONDJFM % of annual")

library(kableExtra)
HUCAvg %>%
  kbl(caption = "Avg HUC ONDJFM contrib to annual total") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  save_kable("station_climate_summary.png")
#####

# plot climo values
cols <- colorRampPalette(RColorBrewer::brewer.pal(9,"YlGnBu"))
cols <- colorRampPalette(RColorBrewer::brewer.pal(9,"YlOrRd"))

rasterVis::levelplot(seasScale, col.regions=cols, par.settings=list(panel.background=list(col="white")),
                     margin=FALSE, main="Seas contrib to annual total (%, PRISM 1895-2020)")+
  latticeExtra::layer(sp.polygons(states, fill=NA, col = "grey40"))+
  # layer(sp.polygons(huc4, fill=NA, col = "black"))
  latticeExtra::layer(sp.polygons(SWhucs, fill=NA, col = "black",lwd=0.5))

# rescaled monthly averages
#moScale<-calc(moAvgPrec, fun=function(x){(x-mean(x))/sd(x)})
moScale<-calc(moAvgPrec, fun=function(x){(x/sum(x))*100})
#moClusterScale<-calc(moAvgPrec, scale)

clusterGrid<-moScale
clusterGrid<-seasScale


# clustering
library(RStoolbox)
library(parallel)
library(rasterVis)

rsOpts(verbose=TRUE)

# add in lat/lon rasters for clusters
# lon <- init(clusterGrid[[1]], 'x')*0.1
# lat <- init(clusterGrid[[1]], 'y')*0.1
# clusterGrid<-stack(clusterGrid,lat,lon)

# crop to smaller area? AZ/NM 
clusterGrid<-crop(clusterGrid, extent(-114.88,-102.9,31.22,37.08))

# cluster data
ptm <- proc.time()
set.seed(1234)
  clusterN<-5
  unC <- unsuperClass(clusterGrid, nSamples = 5000000, nClasses = clusterN, nStarts = 25, nIter = 1000, norm = FALSE) # or allGDD
proc.time() - ptm

# random colors -- https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
qual_col_pals = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
darkcols<-sample(col_vector, clusterN)

classMap<-as.factor(unC$map)
rat <- levels(classMap)[[1]]
# cluster names
# rat[["cluster"]]<-rev(c("Eastside","Oro Valley","Rita Ranch","Gates Pass",
#                         "Twin Peaks","Saddlebrook","Foothills","Central"))

rat[["cluster"]] <- as.character(seq(1, clusterN, by=1))
levels(classMap) <- rat 

# plot classified map
rasterVis::levelplot(classMap, col.regions=darkcols, par.settings=list(panel.background=list(col="white")),
                     margin=FALSE, main="Precip Season Clusters")+
  latticeExtra::layer(sp.polygons(states, fill=NA, col = "grey40"))+
  # layer(sp.polygons(huc4, fill=NA, col = "black"))
  latticeExtra::layer(sp.polygons(SWhucs, fill=NA, col = "black",lwd=0.5))
  # lty=2, http://www.sthda.com/english/wiki/line-types-in-r-lty 

#####
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
  geom_point()+
  ggtitle("K-mean cluster centers - Percent of annual total by month")+
  theme_bw()


# look for jenks break
# library(classInt)
# moJenks<-subset(moCluster, cluster==1)
# jenks<-plotJenks(moJenks$precip, n=4)
# jenks$class.data

#####

#####
# cluster centers - seas
moCluster<-as.data.frame(unC$model$centers)
colnames(moCluster)<-c("JFM","AMJ","JAS","OND")
moCluster$cluster<-seq(1,nrow(moCluster),1)
moCluster <- tidyr::gather(moCluster, seas, precip, 1:4, factor_key=TRUE)
#moCluster$month<-as.numeric(moCluster$month)

# plot monthly data
library(ggplot2)

ggplot(moCluster, aes(seas,precip))+
  geom_bar(stat = "identity")+
  #scale_x_continuous(name="Month", breaks=c(seq(1,12,1)))+
  ylab("% of annual total")+
  facet_wrap(.~cluster)+
  ggtitle("K-mean cluster centers - Percent of annual total by seas")+
  theme_bw()
#####


# CLUSTER Diagnostics
#cluster correlations
library(corrplot)
centers<-as.data.frame(t(unC$model$centers))
res <- cor(centers, method = "pearson")
corrplot(res, type = "upper", order = "original",
         tl.col = "black", tl.srt = 45, title="Intercluster Pearson Corrs")

# # # # find optimal cluster number
clusterN=10
wss<-c(NA, seq(2,clusterN,1))
 minwss<-c(NA, seq(2,clusterN,1))
 totss<-c(NA, seq(2,clusterN,1))
 btwss<-c(NA, seq(2,clusterN,1))
for (i in 2:clusterN){
  set.seed(1234)
  unC <- unsuperClass(clusterGrid, nSamples = 250000, nClasses = i, nStarts = 25, nIter = 1000)
  wss[i]<-unC$model$tot.withinss
   minwss[i]<-min(unC$model$withinss)
   totss[i]<-unC$model$totss
   btwss[i]<-unC$model$betweenss
   print(i)
}
# save cluster diag vars
 save(clusterN,wss,minwss,totss,btwss,
      file="moScale_FindClusterDiagnostics.RData")
load("moScale_FindClusterDiagnostics.RData")
 
 plot(2:clusterN, wss[2:clusterN], type="b", xlab="Number of Clusters",
      ylab="Within groups sum of squares", ylim=c(min(wss[2:clusterN]),max(wss[2:clusterN])))
 plot(3:clusterN, diff(wss[2:clusterN])*-1, type="b", xlab="Number of Clusters",
      ylab="Diff Within groups sum of squares")#, ylim=c(min(diff(wss[2:clusterN])),max(diff(wss[2:clusterN]))))
 plot(2:clusterN, btwss[2:clusterN], type="b", xlab="Number of Clusters",
      ylab="Between groups sum of squares",  ylim=c(min(btwss[2:clusterN]),max(btwss[2:clusterN])))
 plot(2:clusterN, minwss[2:clusterN], type="b", xlab="Number of Clusters",
      ylab="Min groups sum of squares",  ylim=c(min(minwss[2:clusterN]),max(minwss[2:clusterN])))
 plot(3:clusterN, diff(minwss[2:clusterN])*-1, type="b", xlab="Number of Clusters",
      ylab="Diff Min groups sum of squares")#,ylim=c(min(minwss[2:clusterN]),max(minwss[2:clusterN])))
 plot(2:clusterN, btwss[2:clusterN]/totss[2:clusterN], type="b", xlab="Number of Clusters",
      ylab="BSS/TSS Ratio")#,  ylim=c(min(totss[2:clusterN]),max(totss[2:clusterN])))
 
 

#####
# Compare sub-periods
# look at https://www.noaa.gov/news/new-us-climate-normals-are-here-what-do-they-tell-us-about-climate-change 
# clustering
library(RStoolbox)
library(parallel)
library(rasterVis)
 
rsOpts(verbose=TRUE)
 
# define periods 
periods<-list(seq(1901,1991, by=10),
              seq(1930,2020, by=10))

# set cluster n
clusterN<-9

# get map colors
# random colors -- https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
qual_col_pals = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
darkcols<-sample(col_vector, clusterN)

# add to stack in loop
periodClusters<-stack()

# cluster centroids in list
clustCenter<-list()

# cluster on each subperiod
y<-length(periods[[1]]) 
#y=3

for(i in 1:y){
  
  # subset precip
  subPrec<-subset(prec, which(dates$year>=periods[[1]][i] & dates$year<=periods[[2]][i]))
  subDates<-dates[which(dates$year>=periods[[1]][i] & dates$year<=periods[[2]][i]),]
  
  # monthly averages
  monthSum<- stackApply(subPrec, subDates$month, fun = sum)
  moAvgPrec<-monthSum/length(unique(subDates$year))
  
  # rescaled monthly averages
  #moScale<-calc(moAvgPrec, fun=function(x){(x-mean(x))/sd(x)})
  moScale<-calc(moAvgPrec, fun=function(x){(x/sum(x))*100})
  #moClusterScale<-calc(moAvgPrec, scale)
  
  clusterGrid<-moScale
  
  # cluster data
  ptm <- proc.time()
    set.seed(1234)
    unC <- unsuperClass(clusterGrid, nSamples = 5000000, nClasses = clusterN, nStarts = 25, nIter = 1000, norm = FALSE) # or allGDD
  proc.time() - ptm
  
  # factorize classMap
  classMap<-as.factor(unC$map)
  
  # get centroids
  clustPoly<-rasterToPolygons(classMap, dissolve = TRUE)
  trueCentroids = rgeos::gCentroid(clustPoly,byid=TRUE)
   
  # sort by centroid
  centers<-as.data.frame(trueCentroids@coords)
  centers$id<-row.names(centers)
  centers<-centers[
    with(centers, order(y, x)),
  ]
  
  # quick check
  #plot(classMap)
  #text(centers$x,centers$y)
  # save in list
  clustCenter[[i]]<-centers$id
  
    # recode cluster map to geographic centers
    centers$cluster<-seq(1,clusterN, by=1)
    centers$id<-as.numeric(centers$id)
    centers<-centers[,c("id","cluster")]
    classMap <- subs(unC$map, centers)
    # factorize classMap
    classMap<-as.factor(classMap)
    
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

 
# plot classified map
#rasterVis::levelplot(periodClusters)

rasterVis::levelplot(periodClusters, col.regions=darkcols, par.settings=list(panel.background=list(col="white")),
                     margin=FALSE, main="Precip Season Clusters")+
  latticeExtra::layer(sp.polygons(states, fill=NA, col = "grey40"))+
  # layer(sp.polygons(huc4, fill=NA, col = "black"))
  latticeExtra::layer(sp.polygons(SWhucs, fill=NA, col = "black",lwd=0.5))
# lty=2, http://www.sthda.com/english/wiki/line-types-in-r-lty 

test<-do.call(cbind, clustCenter)

 
 
 
 
#####
 
 


 
 



