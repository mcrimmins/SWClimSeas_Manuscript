# data processing and figures for SW Climate Seasonality manuscript
# MAC 03/19/24

library(raster)
library(dplyr)
library(rasterVis)
library(viridis)
library(ggplot2)
library(cowplot)
library(sf)

# set rasteroptions
rasterOptions(progress = 'text')

##### load data -----
# try other polygons...MLRAs, ecoregions?
#states <- getData('GADM', country='United States', level=1)
states <- geodata::gadm(country = "USA", level = 1, path = tempdir())
#states <- subset(states, NAME_1 %in% c("Arizona","New Mexico","Texas","Colorado","Utah","Nevada","California"))
states <- states[states$NAME_1 %in% c("Arizona","New Mexico","Texas","Colorado","Utah","Nevada","California"), ]
states <- as(states, "Spatial")
countries<-map_data("world")
countries<-subset(countries, region %in% c("Mexico"))

# dates 1895-2022 PRISM data
dates=seq(as.Date("1895-01-01"), as.Date("2022-12-31"), by="month")
dates<-as.data.frame(dates)
dates$month<-as.numeric(format(dates$dates, "%m"))
dates$year<-as.numeric(format(dates$dates, "%Y"))
length(unique(dates$year))

# update scratch dir with PRISM data 1895-2022 monthly precip/mean temp
# use ~/RProjects/PRISMDownload/monthyDownloadPRISM.R
# process to subset using ~/RProjects/WinterSummerPrecip/processPRISM.R
#prec<-stack("/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_prec_1895_2022.grd")
#prec<-stack("/home/crimmins/RProjects/WinterSummerPrecip/data/SWmonthlyPRISM_prec_1895_2022.grd")
prec<-stack("./data/SWmonthlyPRISM_prec_1895_2022.grd")



# nClimGrid precip - use processnClimGrid.R
#prec<-stack("/scratch/crimmins/climgrid/processed/sw/SWmonthly_nClimGrid_prec_1895_2022.grd")
#####

##### set up data ----
# monthly averages - prec
monthSum<- stackApply(prec, dates$month, fun = sum)
moAvgPrec<-monthSum/length(unique(dates$year))
names(moAvgPrec)<-month.abb

# use monthly scaled data to perc of annual total
moScalePrec<-calc(moAvgPrec, fun=function(x){(x/sum(x))*100})
names(moScalePrec)<-paste0(month.abb,"Prec")
plot(moScalePrec, colNA="red")

# set grid for clustering
clusterGrid<-moScalePrec

# crop to smaller area? AZ/NM 
clusterGrid<-crop(clusterGrid, extent(-114.88,-102.9,31.22,37.08))
#####

# supp material figure on monthly perc of annual total
#cols <- colorRampPalette(RColorBrewer::brewer.pal(9,"YlGn"))
#rasterVis::levelplot(clusterGrid, col.regions=cols,  main="Monthly % of Annual Total Precip")+
#  latticeExtra::layer(sp.polygons(states, fill=NA, col = "grey40"))

##### evaluate cluster N ---- supp material ----
# # # # find optimal cluster number
# clusterN=10
# wss<-c(NA, seq(2,clusterN,1))
# minwss<-c(NA, seq(2,clusterN,1))
# totss<-c(NA, seq(2,clusterN,1))
# btwss<-c(NA, seq(2,clusterN,1))
# for (i in 2:clusterN){
#   set.seed(1234)
#   unC <- unsuperClass(clusterGrid, nSamples = 250000, nClasses = i, nStarts = 25, nIter = 1000)
#   wss[i]<-unC$model$tot.withinss
#   minwss[i]<-min(unC$model$withinss)
#   totss[i]<-unC$model$totss
#   btwss[i]<-unC$model$betweenss
#   print(i)
# }
# # save cluster diag vars
# save(clusterN,wss,minwss,totss,btwss,
#      file="./data/moScale_FindClusterDiagnostics.RData")
# load("./data/moScale_FindClusterDiagnostics.RData")
# 
# plot(2:clusterN, wss[2:clusterN], type="b", xlab="Number of Clusters",
#      ylab="Within groups sum of squares", ylim=c(min(wss[2:clusterN]),max(wss[2:clusterN])))
# plot(3:clusterN, diff(wss[2:clusterN])*-1, type="b", xlab="Number of Clusters",
#      ylab="Diff Within groups sum of squares")#, ylim=c(min(diff(wss[2:clusterN])),max(diff(wss[2:clusterN]))))
# plot(2:clusterN, btwss[2:clusterN], type="b", xlab="Number of Clusters",
#      ylab="Between groups sum of squares",  ylim=c(min(btwss[2:clusterN]),max(btwss[2:clusterN])))
# plot(2:clusterN, minwss[2:clusterN], type="b", xlab="Number of Clusters",
#      ylab="Min groups sum of squares",  ylim=c(min(minwss[2:clusterN]),max(minwss[2:clusterN])))
# plot(3:clusterN, diff(minwss[2:clusterN])*-1, type="b", xlab="Number of Clusters",
#      ylab="Diff Min groups sum of squares")#,ylim=c(min(minwss[2:clusterN]),max(minwss[2:clusterN])))
# plot(2:clusterN, btwss[2:clusterN]/totss[2:clusterN], type="b", xlab="Number of Clusters",
#      ylab="BSS/TSS Ratio")#,  ylim=c(min(totss[2:clusterN]),max(totss[2:clusterN])))
#####



##### develop clustering -----
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

# raster code
# classMap<-(as.factor(unC$map))
# #classMap<-unC$map
# rat <- levels(classMap)[[1]]
# # cluster names
# rat[["cluster"]]<-(c("Colorado Plateau (1)","Great Plains (2)","Lower Gila/Colorado (3)","Upper Gila (4)","Rio Grande (5)"))
# #rat[["cluster"]] <- as.character(seq(1, clusterN, by=1))
# levels(classMap) <- rat 

# terra based
classMap<-as.factor(unC$map)
levels(classMap)
levels(classMap) <- data.frame(id=1:5, category=c("Colorado Plateau (CP)","Great Plains (GP)","Lower Gila/Colorado (LGC)","Upper Gila (UG)","Rio Grande (RG)"))
#####


##### FIGURE 1a make cluster map using sp
# convert raster to df
rast_df <- as.data.frame(classMap, xy = TRUE)
# drop NA values
rast_df<-na.omit(rast_df)
# state polygons
#stateDF <- fortify(states, region = "NAME_1")

# add state labels
labs<-c("NV","CA","TX","MX","AZ","NM")   
lon<-c(-114.573669,-114.769800,-104.712067,-112.5, -110.30515736669224, -107.28654183584308)
lat<-c(36.557163,34.28445,31.683050,31.579434,36.59082111537832,36.59082111537832)
stLabs<-cbind.data.frame(labs,lat,lon)

# import rivers
# library(rnaturalearth)
# rivers <- ne_download(scale = 'large', type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")
# us_boundary <- ne_countries(scale = 10, country = "United States of America", returnclass = "sf")
# us_rivers <- st_intersection(rivers, us_boundary)
# us_rivers_sp <- as(us_rivers, "Spatial")
# us_rivers_sp <-fortify(us_rivers_sp)

# alternate rivers shapefile http://www.cec.org/north-american-environmental-atlas/lakes-and-rivers-2023/
us_rivers<-st_read("./data/shapes/northamerica_rivers_cec_2023.shp")
us_rivers <- st_transform(us_rivers, crs = 4326)
us_rivers_sp<-as(us_rivers, "Spatial")
us_rivers_sp<-subset(us_rivers_sp, Country=="USA")
us_rivers_sp<-subset(us_rivers_sp, NameEn %in% c("Colorado River","Gila River","Rio Grande",
                                                 "Salt River", "Verde River", "Pecos River","Canadian River"))
us_rivers_sp<-fortify(us_rivers_sp)

# ggplot(data = us_rivers_sp, aes(x = long, y = lat, group = group)) +
#   geom_path(color="blue", size=0.2)+
#   coord_fixed()+
#   theme_void()

# ggplot inset data
states <- map_data("state")
countries<-map_data("world")
countries<-subset(countries, region %in% c("Canada","Mexico"))
aznm<-subset(states, region %in% c("arizona","new mexico"))

insetmap<-ggplot() + 
  geom_polygon(data = countries, aes(x = long, y = lat, group = group), fill="grey88", color="grey", size=0.1)  +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill="white", color="grey", size=0.1)  +
  geom_polygon(data = aznm, aes(x = long, y = lat, group = group), fill="grey", color="black", size=0.05)  + # get the state border back on top
  coord_equal( xlim = c(-123, -69), ylim = c(24, 51))+
  theme_bw()+
  theme(panel.background = element_rect(fill = "powderblue"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        line = element_blank(),
        plot.margin=grid::unit(c(0,0,-1,-1), "mm"))
g <- ggplotGrob(insetmap)
#####

p1<-ggplot(data = rast_df) +
  geom_raster(aes(x = x, y = y, fill = category)) +
  #scale_fill_manual(values=c("grey","#7570B3","#B2DF8A","#8DD3C7","#80B1D3"), name="region")+
  scale_fill_manual(values=c('#7fc97f','#beaed4','#fdc086','#dede73','#386cb0'), name="")+
  #scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854'), name="region")+
  #geom_polygon(data=stateDF, aes(x = long, y = lat, group = group),fill=NA, color="grey17", size=0.1)+
  geom_polygon(data=states, aes(x = long, y = lat, group = group),fill=NA, color="grey17", size=0.1)+
  geom_polygon(data = countries, aes(x = long, y = lat, group = group), fill="grey88", color="grey88", size=0.1)+
  geom_text(data = stLabs, aes(x=lon,y=lat,label=labs))+
  geom_path(data = us_rivers_sp, aes(x = long, y = lat, group = group), fill=NA, color="cyan2", size=0.2)+
  #coord_equal()+
  coord_fixed(xlim=c(-114.88,-102.9), ylim=c(31.22,37.08), ratio = 1)+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  xlab("")+
  ylab("")+
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

p1<-p1 + annotation_custom(grob = g, ymin = 31.25, ymax = 32.25, xmin = -114.85, xmax = -112.9)

#save_plot("./figs/Fig1_map.png", p1, base_height = 5, base_aspect_ratio = 1.75, bg = "white")
#####

##### updated with river labels

# Ensure the river data is an sf object
river_labels <- us_rivers %>%
  filter(NameEn %in% c("Colorado River", "Gila River", "Rio Grande",
                       "Salt River", "Verde River", "Pecos River", "Canadian River"))

# Calculate centroids for selected rivers
river_centroids <- st_centroid(river_labels)

# Extract coordinates for labels
river_labels_df <- data.frame(
  x = st_coordinates(river_centroids)[, 1],
  y = st_coordinates(river_centroids)[, 2],
  name = river_labels$NameEn
)

# manually edit the coordinates for the labels

river_labels_df[12,1]<--104
river_labels_df[3,1]<--104

river_labels_df<-river_labels_df[-15,]

# Add river labels to the plot
p1 <- p1 +
  geom_text(data = river_labels_df, aes(x = x, y = y, label = name), 
            size = 3, color = "blue", fontface = "italic", check_overlap = TRUE)
save_plot("./figs/Fig1_map.png", p1, base_height = 5, base_aspect_ratio = 1.75, bg = "white")

#####



##### FIGURE 1b -- plot monthly cluster values
# cluster centers - months
moCluster<-as.data.frame(unC$model$centers)
colnames(moCluster)<-seq(1,12,1)
#moCluster$cluster<-seq(1,nrow(moCluster),1)
moCluster$cluster<-c("Colorado Plateau (CP)","Great Plains (GP)","Lower Gila/Colorado (LGC)","Upper Gila (UG)","Rio Grande (RG)")
moCluster$cluster<-factor(moCluster$cluster, levels=c("Colorado Plateau (CP)","Great Plains (GP)","Lower Gila/Colorado (LGC)","Upper Gila (UG)","Rio Grande (RG)"))
moCluster <- tidyr::gather(moCluster, month, precip, 1:12, factor_key=TRUE)
moCluster$month<-as.numeric(moCluster$month)

# plot monthly data
p2<-ggplot(moCluster, aes(month,precip, color=as.factor(cluster)))+
  geom_line()+
  geom_point(size=2)+
  scale_color_manual(values=c('#7fc97f','#beaed4','#fdc086','#dede73','#386cb0'), name="")+
  #scale_x_continuous(breaks=seq(1,12,1))+
  scale_x_continuous(
    breaks = 1:12, 
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  )+ 
  #ggtitle("K-mean cluster centers - Percent of annual total by month")+
  ylab("% of Annual Total Precipitation")+
  xlab("Month")+
  theme_bw()+
  theme(
    legend.position = "bottom"
  )

# combined figure
#pMap<-plot_grid(p1, p2, ncol=1, labels="AUTO")
save_plot("./figs/Figs2_moPerc.png", p2, base_height = 5, base_aspect_ratio = 1.75, bg = "white")
#####


##### FIGURE 3(?) -- seasonal climate heat maps
# code from percTransitions.R
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(readr)

##### 
# seasonal percentiles --- calc from calcPercentiles.R
seasPrec<-read_csv("data/csv_5cluster/Cluster5_3mo_total_precip_percentile_PRISM_1895-2022.csv")
seasTemp<-read_csv("data/csv_5cluster/Cluster5_3mo_mean_temp_percentile_PRISM_1895-2022.csv")

# rename columns from cluster to 'c'
# c("Colorado Plateau (1)","Great Plains (2)","Lower Gila/Colorado (3)","Upper Gila (4)","Rio Grande (5)")
# c("Colorado Plateau (CP)","Great Plains (GP)","Lower Gila/Colorado (LGC)","Upper Gila (UG)","Rio Grande (RG)")
# colnames(seasPrec)[4:8]<-c("c1","c2","c3","c4","c5")
# colnames(seasTemp)[4:8]<-c("c1","c2","c3","c4","c5")
colnames(seasPrec)[4:8]<-c("CP","GP","LGC","UG","RG")
colnames(seasTemp)[4:8]<-c("CP","GP","LGC","UG","RG")

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

# iconic droughts
drought1 <- data.frame(
  xmin = 1949,
  xmax = 1958,
  ymin = -Inf,  # Use -Inf and Inf to cover the full height of the facet
  ymax = Inf,
  season = c("OND", "JFM", "AMJ", "JAS")  # Facet variable(s)
)

drought2 <- data.frame(
  xmin = 2006,
  xmax = 2014,
  ymin = -Inf,  # Use -Inf and Inf to cover the full height of the facet
  ymax = Inf,
  season = c("OND", "JFM", "AMJ", "JAS")  # Facet variable(s)
)


p1<-ggplot(seasPrecCatsLong, aes(x = year, y = as.factor(cluster), fill = cat)) + # change y to cluster/month
  geom_tile(color = "black") +
  scale_fill_manual(values = c("brown", "white", "green"), name = "", 
                    guide = guide_legend(reverse = TRUE))+
  #geom_text(aes(label = prec), color = "black", size = 4) +
  #facet_wrap(cluster~., nrow=5)+
  facet_grid((seas~.))+ # change to cluster/month
  #coord_fixed()+
  scale_x_continuous(breaks=seq(1900,2020,10),expand = c(0, 0))+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  #ggtitle("Seasonal Total Precip Percentiles Categories")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_text(size = 7))+
  xlab("")+
  #geom_vline(xintercept = c(1949, 1958), linetype = "dashed", color = "black", size=0.6)+ # Add vertical lines
  #geom_vline(xintercept = c(2007, 2014), linetype = "dashed", color = "black", size=0.6)
  geom_rect(data = drought1,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = NA, color="black",size=0.8, inherit.aes = FALSE)+
  geom_rect(data = drought2,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = NA, color="black",size=0.8, inherit.aes = FALSE)


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
  scale_fill_manual(values = c("blue", "white", "red"), name = "", 
                    guide = guide_legend(reverse = TRUE))+
  #geom_text(aes(label = prec), color = "black", size = 4) +
  #facet_wrap(cluster~., nrow=5)+
  facet_grid(seas~.)+
  #coord_fixed()+
  scale_x_continuous(breaks=seq(1900,2020,10),expand = c(0, 0))+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  #ggtitle("Seasonal Mean Temp Percentiles Categories")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_text(size = 7))+
  xlab("")+
  #geom_vline(xintercept = c(1949, 1958), linetype = "dashed", color = "black", size=0.6)+ # Add vertical lines
  #geom_vline(xintercept = c(2007, 2014), linetype = "dashed", color = "black", size=0.6)
  geom_rect(data = drought1,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = NA, color="black",size=0.8, inherit.aes = FALSE)+
  geom_rect(data = drought2,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = NA, color="black",size=0.8, inherit.aes = FALSE)

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
  scale_fill_manual(values = colorsTP[order(colorsTP$code),"rgbComb"], name = "", 
                    guide = guide_legend(reverse = TRUE))+
  #scale_fill_manual(values = c("darkgoldenrod1","lightgoldenrod1","brown"), name="Drought Cat")+
  #geom_text(aes(label = prec), color = "black", size = 4) +
  #facet_wrap(.~cluster, nrow=5)+
  #coord_fixed()+
  scale_x_continuous(breaks=seq(1900,2020,10),expand = c(0, 0))+
  facet_grid(seas.x~.)+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  #ggtitle("Seasonal Climate Percentile Categories")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_text(size = 7))+
  xlab("year")+
  #geom_vline(xintercept = c(1949, 1958), linetype = "solid", color = "grey50", size=0.6)+ # Add vertical lines
  #geom_vline(xintercept = c(2007, 2014), linetype = "dashed", color = "black", size=0.6)
  geom_rect(data = drought1,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = NA, color="black",size=0.8, inherit.aes = FALSE)+
  geom_rect(data = drought2,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = NA, color="black",size=0.8, inherit.aes = FALSE)
  

pMap<-cowplot::plot_grid(p1, p2,p3, labels = "AUTO", align = "v",ncol = 1,
                         label_size = 12,
                         label_x = 0.01, label_y = 0.06,
                         hjust = -0.5, vjust = -0.5)
save_plot("./figs/Fig4_heatMap.png", pMap, base_height = 7, base_aspect_ratio = 1.5, bg = "white")


##### FIGURE X --- seasonal temperature and precipitation anomalies/trends

##### 
# seasonal means/sums
seasPrec<-read_csv("data/csv_5cluster/Cluster5_3mo_total_precip_PRISM_1895-2022.csv")
seasTemp<-read_csv("data/csv_5cluster/Cluster5_3mo_mean_temp_PRISM_1895-2022.csv")

# nClimDiv data
seasPrec<-read_csv("data/csv_5cluster/Cluster5_3mo_total_precip_nClimDiv_1895-2022.csv")
seasTemp<-read_csv("data/csv_5cluster/Cluster5_3mo_mean_temp_nClimDiv_1895-2022.csv")


# rename columns
colnames(seasPrec)[4:8]<-c("CP","GP","LGC","UG","RG")
colnames(seasTemp)[4:8]<-c("CP","GP","LGC","UG","RG")

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
#seasPrecLong<-tidyr::gather(seasPrec, cluster,prec, 4:8)
#seasPrecLong$month<-factor(seasPrecLong$month, levels=c(12,3,6,9))
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

# convert to long for plotting - temp
#seasTempLong<-tidyr::gather(seasTemp, cluster,temp, 4:8)
#seasTempLong$month<-factor(seasTempLong$month, levels=c(12,3,6,9))
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


##### PRECIP SEAS
# mean by group
seasPrecLong<-subset(seasPrecLong, !is.na(prec))
seasPrecLong<-seasPrecLong %>% group_by(cluster, month) %>%
  mutate(meanPrec=mean(prec, na.rm=TRUE))
seasPrecLong$anom<-seasPrecLong$prec-seasPrecLong$meanPrec
seasPrecLong$anomCat<-ifelse(seasPrecLong$anom>0, "wet","dry")
seasPrecLong$seas<-factor(seasPrecLong$seas, levels=(c("OND","JFM","AMJ","JAS")))

# get trends
trend_prec <- seasPrecLong %>%
  group_by(cluster, seas) %>%
  summarize(
    slope = coef(lm(anom ~ year))[2],  # Extract slope
    p_value = summary(lm(anom ~ year))$coefficients[2, 4],  # Extract p-value
    .groups = "drop"
  ) %>%
  mutate(
    significance = ifelse(p_value < 0.05, "*", ""),  # Determine significance
    label = paste0(round(slope, 4), significance, "")  # Create annotation label
  )

# table for paper
table_prec <- trend_prec %>%
  select(cluster, seas, label) %>% # Select relevant columns
  pivot_wider(names_from = seas, values_from = label)

p1<-ggplot(seasPrecLong)+
  geom_bar(aes(year,anom, fill=anomCat),stat="identity",position="identity")+
  scale_fill_manual(values=c("brown","forestgreen"), guide="none")+
  geom_smooth(aes(year,anom),method=lm, se=F)+
  #scale_x_continuous(breaks=seq(1900,2020,10))+
  facet_grid(cluster~seas)+
  #ggtitle("Seasonal Precipitation Anomaly by Cluster")+
  ylab("Anomaly (mm)")+
  theme_bw()
  # geom_text(
  #   data = trend_prec,
  #   aes(x = 1960, y = -90, label = label),  # Adjust x and y for placement
  #   inherit.aes = FALSE, color = "black", size = 3
  # )
#save_plot("./figs/Fig10a_precipTrends.png", p1, base_height = 7, base_aspect_ratio = 1.2, bg = "white")

##### TEMPS SEAS
# mean by group
seasTempLong<-subset(seasTempLong, !is.na(temp))
seasTempLong<-seasTempLong %>% group_by(cluster,month) %>%
  mutate(meanTemp=mean(temp, na.rm=TRUE))
seasTempLong$anom<-seasTempLong$temp-seasTempLong$meanTemp
seasTempLong$anomCat<-ifelse(seasTempLong$anom>0, "warm","cool")
seasTempLong$seas<-factor(seasTempLong$seas, levels=(c("OND","JFM","AMJ","JAS")))

# get trends
trend_temp <- seasTempLong %>%
  group_by(cluster, seas) %>%
  summarize(
    slope = coef(lm(anom ~ year))[2],  # Extract slope
    p_value = summary(lm(anom ~ year))$coefficients[2, 4],  # Extract p-value
    .groups = "drop"
  ) %>%
  mutate(
    significance = ifelse(p_value < 0.05, "*", ""),  # Determine significance
    label = paste0(round(slope, 4), significance, "")  # Create annotation label
  )

# table for paper
table_temp <- trend_temp %>%
  select(cluster, seas, label) %>% # Select relevant columns
  pivot_wider(names_from = seas, values_from = label)

p2<-ggplot(seasTempLong)+
  geom_bar(aes(year,anom, fill=anomCat),stat="identity",position="identity")+
  scale_fill_manual(values=c("blue","red"), guide="none")+
  geom_smooth(aes(year,anom),method=lm, se=F)+
  #scale_x_continuous(breaks=seq(1900,2020,10))+
  facet_grid(cluster~seas)+
  #ggtitle("Seasonal Temperature Anomaly by Cluster")+
  ylab("Anomaly (C)")+
  theme_bw()
  # geom_text(
  #   data = trend_temp,
  #   aes(x = 1960, y = -2.75, label = label),  # Adjust x and y for placement
  #   inherit.aes = FALSE, color = "black", size = 3
  # )
#save_plot("./figs/Fig10b_tempTrends.png", p2, base_height = 7, base_aspect_ratio = 1.2, bg = "white")
p3<-cowplot::plot_grid(p1, p2, labels = "AUTO", align = "v",ncol = 1)
save_plot("./figs/Fig10_TempPrecipTrends_nClimDiv.png", p3, base_height = 11, base_aspect_ratio = 0.8, bg = "white")

##### combine into one figure, fix cluster and season names, remove legend

# write tables to excel file
library(openxlsx)
wb <- createWorkbook()

# Add multiple sheets (tabs) with different tables
addWorksheet(wb, "TempTrends")
writeData(wb, "TempTrends", table_temp)  # Replace `table1` with your actual table

addWorksheet(wb, "PrecTrends")
writeData(wb, "PrecTrends", table_prec)  # Replace `table2` with your actual table

# Save the workbook to a file
saveWorkbook(wb, file = "./figs/Trends_nClimDiv.xlsx", overwrite = TRUE)

#####



##### supp figures
library(ggplot2)

load("./data/catCounts.RData") ##### from percTransitions.R, line 366
# only warm 
p1<-ggplot(subset(catCounts, code %in% c("warm-wet","warm-dry","warm-avg")), aes(x=period,y=(count/(16*4*5)*100),fill=code))+
  geom_bar(stat="identity")+
  #geom_bar(position="fill")+
  scale_fill_manual(values = colorsTP[order(colorsTP$code),"rgbComb"][7:9], name = "", 
                    guide = guide_legend(reverse = TRUE))+
  ylab("Frequency of Occurrence (%)")+xlab("Period")+
theme_bw()

save_plot("./figs/SuppF1_warmSeasons.png", p1, base_height = 5, base_aspect_ratio = 1.5, bg = "white")

# write out seasPrec and seasTemp for data repository
write.csv(seasPrec, "./figs/AZ_NM_Cluster5_3mo_total_precip_PRISM_1895-2022.csv", row.names = FALSE)
write.csv(seasTemp, "./figs/AZ_NM_Cluster5_3mo_mean_temp_PRISM_1895-2022.csv", row.names = FALSE)

