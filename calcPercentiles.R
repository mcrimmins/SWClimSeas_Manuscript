# calculate seasonal percentile rankings for climate variables
# MAC 08/11/23
# generate csv files with extractClusterTS.R

library(readr)
library(dplyr)
library(tidyr)


##### precipitation percentiles ----
# read in data file
prec<-read_csv("data/csv_5cluster/Cluster5_monthly_total_precip_nClimDiv_1895-2022.csv")
# apply rolling sum to seas length
precPerc<-cbind.data.frame(prec[,c(1:3)],zoo::rollapply(prec[,4:8], FUN = sum, width = 3,
                                                        fill=NA,align="right", by.column = TRUE))

# save total seas precip
write.csv(precPerc,file='./data/csv_5cluster/Cluster5_3mo_total_precip_nClimDiv_1895-2022.csv', row.names=FALSE)

# wide to long
precPerc <- gather(precPerc, cluster, prec, cluster1:cluster5, factor_key=TRUE)
precPerc<-subset(precPerc, !is.na(prec))
# calc percent rank to seas and cluster
precPerc<-precPerc%>%group_by(month,cluster)%>%mutate(percRank=rank(prec)/(length(prec)+1))
precPerc<-precPerc[,-5]
# long back to wide
precPerc <- spread(precPerc, cluster, percRank, drop=TRUE)

# write out csv
write.csv(precPerc,file='./data/csv_5cluster/Cluster5_12mo_total_precip_percentile_PRISM_1895-2022.csv', row.names=FALSE)
#####

##### temp percentiles ----
# read in data file
temp<-read_csv("data/csv_5cluster/Cluster5_monthly_mean_temp_nClimDiv_1895-2022.csv")
# apply rolling sum to seas length
tempPerc<-cbind.data.frame(temp[,c(1:3)],zoo::rollapply(temp[,4:8], FUN = mean, width = 3,
                                                        fill=NA,align="right", by.column = TRUE))

# write out seas means
write.csv(tempPerc,file='./data/csv_5cluster/Cluster5_3mo_mean_temp_nClimDiv_1895-2022.csv', row.names=FALSE)
#####

# wide to long
tempPerc <- gather(tempPerc, cluster, temp, cluster1:cluster5, factor_key=TRUE)
tempPerc<-subset(tempPerc, !is.na(temp))
# calc percent rank to seas and cluster
tempPerc<-tempPerc%>%group_by(month,cluster)%>%mutate(tempRank=rank(temp)/(length(temp)+1))
tempPerc<-tempPerc[,-5]
# long back to wide
tempPerc <- spread(tempPerc, cluster, tempRank, drop=TRUE)

# write out csv
write.csv(tempPerc,file='./data/csv_5cluster/Cluster5_12mo_mean_temp_percentile_PRISM_1895-2022.csv', row.names=FALSE)
#####


