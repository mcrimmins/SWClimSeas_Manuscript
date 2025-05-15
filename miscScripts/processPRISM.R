# process monthly PRISM data for SW winter-summer precip analysis
# adapted from /LivenehDrought/processMonthlyPRISM.R
# MAC 08/23/21

library(raster)
library(prism)

# set rasteroptions
rasterOptions(progress = 'text')

# dates 1895-2017 PRISM data
dates=seq(as.Date("1895-01-01"), as.Date("2023-12-31"), by="month")

#####
# precip
options(prism.path = "/scratch/crimmins/PRISM/monthly/precip") 
  files<-as.data.frame(prism_archive_ls())
  fileOrder<-tidyr::separate(data = files, col = `prism_archive_ls()`, into = c(NA,NA,"version",NA, "date",NA), sep = "_")
  fileOrder$date<-as.Date(paste0(fileOrder$date,"01"), "%Y%m%d")
  dateOrder<-as.data.frame(sort.int(fileOrder$date, index.return=TRUE))
  files<-as.data.frame(prism_archive_ls())
    colnames(files)<-"name"
    #files$fullName<-paste0("/scratch/crimmins/PRISM/monthly/precip",files$name)
    files<-files$name[dateOrder$ix]
  
prec <- pd_stack(files)
# western US only
e <- extent(-115.5, -102, 30.5, 39)
# SW plus (-116,-102,30.5,40)
prec <- crop(prec, e)	
#names(prec)<-dates

writeRaster(prec,filename="/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_prec_1895_2023.grd", overwrite=TRUE )
#writeRaster(prec,filename="/scratch/crimmins/PRISM/monthly/processed/monthlyPRISM_prec_1895_2017.grd", overwrite=TRUE )
#####

#####
# tmean
options(prism.path = "/scratch/crimmins/PRISM/monthly/tmean")
  files<-as.data.frame(prism_archive_ls())
  fileOrder<-tidyr::separate(data = files, col = `prism_archive_ls()`, into = c(NA,NA,"version",NA, "date",NA), sep = "_")
  fileOrder$date<-as.Date(paste0(fileOrder$date,"01"), "%Y%m%d")
  dateOrder<-as.data.frame(sort.int(fileOrder$date, index.return=TRUE))
  files<-as.data.frame(prism_archive_ls())
    colnames(files)<-"name"
    #files$fullName<-paste0("/scratch/crimmins/PRISM/monthly/precip",files$name)
    files<-files$name[dateOrder$ix]

  tmean <- pd_stack(files)
  # western US only
  e <- extent(-115.5, -102, 30.5, 39)
  tmean <- crop(tmean, e)	
  #names(tmean)<-dates
  
  writeRaster(tmean,filename="/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_tmean_1895_2023.grd", overwrite=TRUE )
#writeRaster(tmean,filename="/scratch/crimmins/PRISM/monthly/processed/monthlyPRISM_tmean_1895_2017.grd", overwrite=TRUE )

#####
# tmin
options(prism.path = "/scratch/crimmins/PRISM/monthly/tmin") 
  files<-as.data.frame(prism_archive_ls())
  fileOrder<-tidyr::separate(data = files, col = `prism_archive_ls()`, into = c(NA,NA,"version",NA, "date",NA), sep = "_")
  fileOrder$date<-as.Date(paste0(fileOrder$date,"01"), "%Y%m%d")
  dateOrder<-as.data.frame(sort.int(fileOrder$date, index.return=TRUE))
  files<-as.data.frame(prism_archive_ls())
    colnames(files)<-"name"
    #files$fullName<-paste0("/scratch/crimmins/PRISM/monthly/precip",files$name)
    files<-files$name[dateOrder$ix]
  
  tmin <- pd_stack(files)
  # western US only
  e <- extent(-115.5, -102, 30.5, 39)
  tmin <- crop(tmin, e)	
  #names(tmin)<-dates
  
  writeRaster(tmin,filename="/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_tmin_1895_2023.grd", overwrite=TRUE )
#####

#####  
# tmax
options(prism.path = "/scratch/crimmins/PRISM/monthly/tmax") 
  files<-as.data.frame(prism_archive_ls())
  fileOrder<-tidyr::separate(data = files, col = `prism_archive_ls()`, into = c(NA,NA,"version",NA, "date",NA), sep = "_")
  fileOrder$date<-as.Date(paste0(fileOrder$date,"01"), "%Y%m%d")
  dateOrder<-as.data.frame(sort.int(fileOrder$date, index.return=TRUE))
  files<-as.data.frame(prism_archive_ls())
    colnames(files)<-"name"
    #files$fullName<-paste0("/scratch/crimmins/PRISM/monthly/precip",files$name)
    files<-files$name[dateOrder$ix]
  
  tmax <- pd_stack(files)
  # western US only
  e <- extent(-115.5, -102, 30.5, 39)
  tmax <- crop(tmax, e)	
  #names(tmax)<-dates
  
  writeRaster(tmax,filename="/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_tmax_1895_2023.grd", overwrite=TRUE )
#####

#####  
# vpdmax
options(prism.path = "/scratch/crimmins/PRISM/monthly/vpdmax") 
  files<-as.data.frame(prism_archive_ls())
  fileOrder<-tidyr::separate(data = files, col = `prism_archive_ls()`, into = c(NA,NA,"version",NA, "date",NA), sep = "_")
  fileOrder$date<-as.Date(paste0(fileOrder$date,"01"), "%Y%m%d")
  dateOrder<-as.data.frame(sort.int(fileOrder$date, index.return=TRUE))
  # check if date and file lengths are the same, otherwise delete extra files
  if(nrow(dateOrder)!=nrow(fileOrder)){
      files<-subset(files,!is.na(fileOrder$date))
      fileOrder<-subset(fileOrder,!is.na(fileOrder$date))
  }else{}
  dateOrder<-as.data.frame(sort.int(fileOrder$date, index.return=TRUE))
  
  #files<-as.data.frame(prism_archive_ls())
    colnames(files)<-"name"
    #files$fullName<-paste0("/scratch/crimmins/PRISM/monthly/precip",files$name)
    files<-files$name[dateOrder$ix]

vpdmax <- pd_stack(files)
# western US only
e <- extent(-115.5, -102, 30.5, 39)
vpdmax <- crop(vpdmax, e)	
#names(tmax)<-dates

writeRaster(vpdmax,filename="/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_vpdmax_1895_2023.grd", overwrite=TRUE )
#####  
  
#####  
# tdmean
options(prism.path = "/scratch/crimmins/PRISM/monthly/tdmean") 
files<-as.data.frame(prism_archive_ls())
fileOrder<-tidyr::separate(data = files, col = `prism_archive_ls()`, into = c(NA,NA,"version",NA, "date",NA), sep = "_")
fileOrder$date<-as.Date(paste0(fileOrder$date,"01"), "%Y%m%d")
dateOrder<-as.data.frame(sort.int(fileOrder$date, index.return=TRUE))
# check if date and file lengths are the same, otherwise delete extra files
if(nrow(dateOrder)!=nrow(fileOrder)){
  files<-subset(files,!is.na(fileOrder$date))
  fileOrder<-subset(fileOrder,!is.na(fileOrder$date))
}else{}
dateOrder<-as.data.frame(sort.int(fileOrder$date, index.return=TRUE))

#files<-as.data.frame(prism_archive_ls())
colnames(files)<-"name"
#files$fullName<-paste0("/scratch/crimmins/PRISM/monthly/precip",files$name)
files<-files$name[dateOrder$ix]

tdmean <- pd_stack(files)
# western US only
e <- extent(-115.5, -102, 30.5, 39)
tdmean <- crop(tdmean, e)	
#names(tmax)<-dates

writeRaster(tdmean,filename="/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_tdmean_1895_2023.grd", overwrite=TRUE )
#####    


# SW domain
# -115.576172,30.543339,-101.997070,38.582526

