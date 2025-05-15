# calculate SPI and SPEI from monthly PRISM time series
# MAC 06/08/23

library(readr)
library(SPEI)

precip <- read_csv("data/csv_5cluster/Cluster5_monthly_total_precip_PRISM_1895-2022.csv",
                  col_types = cols(dates = col_date(format = "%Y-%m-%d")))

pet <- read_csv("data/csv_5cluster/Cluster5_monthly_mean_PET_PRISM_1895-2022.csv", 
                col_types = cols(dates = col_date(format = "%Y-%m-%d")))

# number of clusters
# n=5

# calculate SPI values
spiTS3<-spi(precip[,4:8],3, na.rm = TRUE)$fitted
spiTS3<-cbind.data.frame(precip[,1:3], spiTS3)

spiTS6<-spi(precip[,4:8],6, na.rm = TRUE)$fitted
spiTS6<-cbind.data.frame(precip[,1:3], spiTS6)

# calculate
wtrBal<-precip[,4:8]-pet[,4:8]
speiTS3<-spei(wtrBal,3, na.rm=TRUE)$fitted
speiTS3<-cbind.data.frame(pet[,1:3], speiTS3)

speiTS6<-spei(wtrBal,6, na.rm=TRUE)$fitted
speiTS6<-cbind.data.frame(pet[,1:3], speiTS6)

# write out csv
write.csv(spiTS3,file='./data/csv_5cluster/Cluster5_3mo_SPI_PRISM_1895-2022.csv', row.names=FALSE)
write.csv(spiTS6,file='./data/csv_5cluster/Cluster5_6mo_SPI_PRISM_1895-2022.csv', row.names=FALSE)

write.csv(speiTS3,file='./data/csv_5cluster/Cluster5_3mo_SPEI_PRISM_1895-2022.csv', row.names=FALSE)
write.csv(speiTS6,file='./data/csv_5cluster/Cluster5_6mo_SPEI_PRISM_1895-2022.csv', row.names=FALSE)

