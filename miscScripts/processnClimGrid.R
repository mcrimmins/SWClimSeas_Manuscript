# process nClimGrid data
# MAC 08/30/21
# update netcdf using downloadnClimGrid_netcdf.R

library(raster)

# set rasteroptions
rasterOptions(progress = 'text')

# load data into stack
prec <-stack("/scratch/crimmins/climgrid/netcdf/nclimgrid_prcp.nc")

# western US only
e <- extent(-115.5, -102, 30.5, 39)
prec <- crop(prec, e)	

# find last complete year
dates<-as.data.frame(names(prec))

dates<-seq(as.Date(strsplit(names(prec[[1]]), "X")[[1]][2],"%Y.%m.%d"),
           as.Date(strsplit(names(prec[[nlayers(prec)]]), "X")[[1]][2],"%Y.%m.%d"), by="month")

# subset to complete years
prec<-subset(prec, which(dates<="2022-12-01"))

writeRaster(prec,filename="/scratch/crimmins/climgrid/processed/sw/SWmonthly_nClimGrid_prec_1895_2022.grd", overwrite=TRUE )

