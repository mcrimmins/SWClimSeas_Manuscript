# spatial clustering on monthly average precip
# 02/04/23 MAC

# more on model based clustering
# https://en.proft.me/2017/02/1/model-based-clustering-r/

library(raster)
library(mclust)

# set rasteroptions
rasterOptions(progress = 'text')

# load data
# watershed boundaries
huc4<-rgdal::readOGR(dsn = "~/RProjects/SOMs/monsoonPrecip/shapes/wbdhu4_a_us_september2019/wbdhu4_a_us_september2019.gdb", layer="WBDHU4")
SWhucs<-subset(huc4, HUC4 %in% c("1504","1506","1502","1302","1408"))
# try other polygons...MLRAs, ecoregions?
states <- getData('GADM', country='United States', level=1)

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
# add in PET from monthlyHargreavesPETraster.R
#  pet<-stack("/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_hargreaves_1895_2022.grd")
  
# monthly averages - prec
monthSum<- stackApply(prec, dates$month, fun = sum)
  moAvgPrec<-monthSum/length(unique(dates$year))
  names(moAvgPrec)<-month.abb
# monthly averages - PET
# monthSum<- stackApply(pet, dates$month, fun = sum)
#   moAvgPET<-monthSum/length(unique(dates$year))
#   names(moAvgPET)<-month.abb

#rm(prec, pet)
      
# check for NAs
table(is.na(moAvgPrec[]))
plot(moAvgPrec, colNA="red")

# sanity check on monthly averages  
#rasterVis::levelplot(moAvgPrec)

# use monthly scaled data to perc of annual total
moScalePrec<-calc(moAvgPrec, fun=function(x){(x/sum(x))*100})
  names(moScalePrec)<-paste0(month.abb,"Prec")
#moScalePET<-calc(moAvgPET, fun=function(x){(x/sum(x))*100})
#  names(moScalePET)<-paste0(month.abb,"PET")  
plot(moScalePrec, colNA="red")

# check for NAs
#table(is.na(moScale[]))
#plot(moScale, colNA="red")

# use mclust https://www.erikkusch.com/courses/bftp-biome-detection/cluster-analysis/
# https://cran.r-project.org/web/packages/mclust/vignettes/mclust.html
# grid to cluster
cGrid<-moScalePrec
# both prec and PET
#cGrid<-stack(moScalePrec,moScalePET)
# format data for mclust
cGrid <- values(cGrid)
rownames(cGrid) <- 1:dim(cGrid)[1] # rownames to index raster cell number

# na omit
cGrid <- na.omit(cGrid) # omit all rows which contain at least one NA record
dim(cGrid) # new dimensions of our matrix

# model testing
dataBIC <- mclustBIC(cGrid, G=2:15) # identify BICs for different models
print(summary(dataBIC)) # show summary of top-ranking models
 plot(dataBIC)
 mod1 <- Mclust(cGrid, x = dataBIC)
 summary(mod1, parameters = TRUE)
 #plot(mod1, what = "classification")
 
#mod1 <- Mclust(cGrid, G=7)
#plot(mod1)
#summary(mod1, parameters = TRUE)

#plot(mod1, what = "classification")

mod <- Mclust(cGrid, # data for the cluster model
              G = 6 # BIC index for model to be built
              #x=dataBIC
)

ModPred <- predict.Mclust(mod, cGrid) # prediction
Pred_ras <- moScalePrec[[1]] # establishing a rediction raster
values(Pred_ras) <- NA # set everything to NA
# set values of prediction raster to corresponding classification according to rowname
values(Pred_ras)[as.numeric(rownames(cGrid))] <- as.vector(ModPred$classification)
plot(Pred_ras)
plot(SWhucs, add=TRUE)

rasterVis::levelplot(Pred_ras,par.settings=list(panel.background=list(col="white")),
                     margin=FALSE, main="Precip Season Clusters")+
  latticeExtra::layer(sp.polygons(SWhucs, fill=NA, col = "grey40"))

# look at cluster values
cMeans<-as.data.frame(t(mod[["parameters"]][["mean"]]))
  cMeans$cluster<-seq(1,nrow(cMeans),1)
  colnames(cMeans)[1:12]<-c(1:12)
moCluster <- tidyr::gather(cMeans, month, precip, 1:12, factor_key=TRUE)
  moCluster$month<-as.numeric(moCluster$month)
library(ggplot2)
ggplot(moCluster, aes(month,precip, color=as.factor(cluster)))+
  geom_line()+
  #geom_bar(stat = "identity")+
  geom_point()
  #coord_polar()

library("PerformanceAnalytics")
my_data <- mod[["parameters"]][["mean"]]
chart.Correlation(my_data, histogram=TRUE, pch=19)


# compare to kmeans
unC <- RStoolbox::unsuperClass(moScalePrec, nSamples = 5000000, nClasses = 5, nStarts = 25, nIter = 1000, norm = FALSE)
plot(unC$map)
plot(SWhucs, add=TRUE)

# cluster diagnostic stats
#library(fpc)
#cs = cluster.stats(dist(cGrid), mod$classification)


library(cluster)
# cluster with clara
cls = clara (cGrid, 5, samples=100000) 
summary(cls)
plot(cls)

# http://www.sthda.com/english/articles/29-cluster-validation-essentials/96-determiningthe-optimal-number-of-clusters-3-must-know-methods/
library(factoextra)
# Elbow method
fviz_nbclust(cGrid, clara, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
# Silhouette method
fviz_nbclust(cGrid, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(cGrid, kmeans, nstart = 10,  method = "gap_stat", nboot = 3)+
  labs(subtitle = "Gap statistic method")

library("NbClust")
nb <- NbClust(cGrid, distance = "euclidean", min.nc = 2,
              max.nc = 6, method = "kmeans")
library("factoextra")
fviz_nbclust(nb)

