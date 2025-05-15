# create monthly Hargreaves based water balance
# Applying SPEI::Hargreaves to raster grid using overlay creates problem with monthly cycling through each grid
# This code creates driving grids without needing specific time series cycles to be respected
# code taken from SPEI::Hargreaves
# MAC 10/1/18
# updated 2/3/23

library(raster)
# set rasteroptions
rasterOptions(progress = 'text')

# load grids Livneh or PRISM
tmin<-stack("/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_tmin_1895_2022.grd")
tmax<-stack("/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_tmax_1895_2022.grd")

# get tDiff and tAvg
tDiff<-tmax-tmin
tAvg<-(tmax+tmin)/2

# get latitudes
lat<-init(tmin, 'y')
latStack <- stack(replicate(12, lat))

# create month grid of values
month <- latStack
for(i in 1:12){
  month[[i]]<-setValues(latStack[[i]],i)
}

# days in month stack
mlen <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 
          31)
monthDays <- latStack
for(i in 1:12){
  monthDays[[i]]<-setValues(latStack[[i]],mlen[i])
}

# monthly radiation calc based on month and latitude
monthRadcalc<-function(c,lat){
  J <- as.integer(30.5 * c - 14.6)
  delta <- 0.409 * sin(0.0172 * J - 1.39)
  dr <- 1 + 0.033 * cos(0.0172 * J)
  latr <- lat/57.2957795
  sset <- -tan(latr) * tan(delta)
  omegas <- sset * 0
  omegas[sset >= {
    -1
  } & sset <= 1] <- acos(sset[sset >= {
    -1
  } & sset <= 1])
  omegas[sset < {
    -1
  }] <- max(omegas)
  Ra <- 37.6 * dr * (omegas * sin(latr) * sin(delta) + 
                       cos(latr) * cos(delta) * sin(omegas))
  Ra <- ifelse(Ra < 0, 0, Ra)
}
# get grid stack of month average radiation
monthRad <- raster::overlay(month,latStack,fun = monthRadcalc)

# create full time series stack
years=100 # 1915-2015
allRad<- stack(replicate(101, monthRad))

# daily ETO calc - average daily ET0 by month
ET0 <- function(Ra, tavg, tdiff) {
  0.0023 * 0.408 * Ra * (tavg + 17.8) * tdiff^0.5
}
harg <- raster::overlay(allRad, tAvg, tDiff, fun = ET0)
# get total monthly ET0 in mm
harg<-harg*stack(replicate(101, monthDays))

# write PET to grid
writeRaster(harg,filename="/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_hargreaves_1895_2022.grd", overwrite=TRUE )


# check monthly climo
dates=seq(as.Date("1895-01-01"), as.Date("2022-12-31"), by="month")
dates<-as.data.frame(dates)
dates$month<-as.numeric(format(dates$dates, "%m"))
dates$year<-as.numeric(format(dates$dates, "%Y"))

monthSum<- stackApply(harg, dates$month, fun = sum)
moAvgPrec<-monthSum/length(unique(dates$year))

annSumm<-calc(moAvgPrec, sum)

# Hamon PET?
# https://github.com/ua-snap/potential-evapotranspiration/blob/master/PETFunctions/calcPEThamon.R

