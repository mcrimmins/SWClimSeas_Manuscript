# fire stats by precip cluster
# MAC 06/05/23

library(raster)
library(ggplot2)

# load cluster map
classMap<-raster("./data/classMap5.grd")

#####
# load cropped FOD 
load("~/RProjects/BurnPeriodResearch/data/swFOD.RData")
# work with  FOD dataframe only
fc<-fc@data
# remove factors
fc[,] <- lapply(fc, function(x) type.convert(as.character(x), as.is = TRUE))
# adjust dates on fc DF  
fc$DISCOVERY_DATE<-as.Date(fc$DISCOVERY_DATE,"%m/%d/%Y")
fc$CONT_DATE<-as.Date(fc$CONT_DATE,"%m/%d/%Y")
fc$DISCOVERY_MONTH<-as.numeric(format(fc$DISCOVERY_DATE, "%m"))
fc$CONT_MONTH<-as.numeric(format(fc$CONT_DATE, "%m"))
fc$DISCOVERY_YEAR<-as.numeric(format(fc$CONT_DATE, "%Y"))
#####

# extract raster value to fire points
fc$fireCluster<-extract(classMap,fc[,c("LONGITUDE","LATITUDE")])
# 

ggplot(fc, aes(fireCluster,DISCOVERY_MONTH, group=fireCluster))+
  geom_boxplot(varwidth = TRUE)+
  ggtitle("Wildfire Discovery Month by Cluster Region")

ggplot(fc, aes(fireCluster,CONT_MONTH, group=fireCluster))+
  geom_boxplot(varwidth = TRUE)+
  ggtitle("Wildfire Containment Month by Cluster Region")

ggplot(fc, aes(DISCOVERY_YEAR,DISCOVERY_DOY, color=as.factor(fireCluster), group=DISCOVERY_YEAR))+
  geom_boxplot(varwidth = TRUE)+
  ggtitle("Wildfire Discovery Day of Year by Cluster Region")+
  geom_hline(yintercept = 182)+
  geom_hline(yintercept = 91)+
  facet_wrap(.~fireCluster)


ggplot(fc, aes(fireCluster,DISCOVERY_DOY, group=fireCluster))+
  geom_boxplot(varwidth = TRUE)+
  ggtitle("Wildfire Discovery Day of Year by Cluster Region")+
  geom_hline(yintercept = 182)+
  geom_hline(yintercept = 91)


ggplot(fc, aes(fireCluster,CONT_DOY, group=fireCluster))+
  geom_boxplot(varwidth = TRUE)+
  ggtitle("Wildfire Containment Day of Year by Cluster Region")+
  geom_hline(yintercept = 182)+
  geom_hline(yintercept = 91)

#####
# plot large fires on classMap
# code from /SWWildfires/AZNM_wildfires_geospatial.R

library(rasterVis)
library(RColorBrewer)

load("~/RProjects/SWWildfires/data/AZNM_largeWildfires.RData")

firePerims<-list()
for(i in 1:length(fireProgList)){
  firePerims[[i]]<-fireProgList[[i]][[3]]
}

# bind Hermit's peak perims
firePerims[[20]]<-bind(firePerims[[20]],firePerims[[21]])
firePerims<-firePerims[-21]

firePerims<-do.call(bind,firePerims)
firePerims_sp<-firePerims
firePerims_df<-firePerims@data
firePerims_df$id<-seq(1,nrow(firePerims_df),1)

firePerims = broom::tidy(firePerims)
firePerims$id<-as.numeric(firePerims$id)
# join dfs
firePerims = dplyr::left_join(firePerims,firePerims_df,by='id')

# get fire map labels
firePerims_df$BurnBndLat<-as.numeric(as.character(firePerims_df$BurnBndLat))
  firePerims_df$BurnBndLat<-firePerims_df$BurnBndLat+(1e-1 * runif(nrow(firePerims_df)))
firePerims_df$BurnBndLon<-as.numeric(as.character(firePerims_df$BurnBndLon))
fireLabCoord = firePerims_df[c("BurnBndLon", "BurnBndLat")]
coordinates(fireLabCoord) <- ~BurnBndLon+BurnBndLat
firePerims_df$label<-paste0(firePerims_df$Incid_Name,"(",firePerims_df$Ig_Date,")")

# try other polygons...MLRAs, ecoregions?
states <- getData('GADM', country='United States', level=1)

# plot classified map
rasterVis::levelplot(classMap, col.regions=colorRampPalette(brewer.pal(5, "Set3")),
                     par.settings=list(panel.background=list(col="white")),
                     margin=FALSE, main="Precip Season Clusters")+
  latticeExtra::layer(sp.polygons(states, fill=NA, col = "grey40"))+
  latticeExtra::layer(sp.polygons(firePerims_sp, fill=NA, col = "red"))+
  # layer(sp.polygons(huc4, fill=NA, col = "black"))
  #latticeExtra::layer(sp.polygons(SWhucs, fill=NA, col = "black",lwd=0.5))
  latticeExtra::layer(sp.text(coordinates(fireLabCoord), txt = firePerims_df$label, cex=0.7))
