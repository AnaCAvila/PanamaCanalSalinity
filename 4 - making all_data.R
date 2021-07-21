#predictions for stations around the lake
library(relaimpo)



lake_Shallow <- Shallow[Shallow$site %in% (lake), ]

df <- data.frame(site = lake_Shallow$site, date = lake_Shallow$date, data = lake_Shallow$data)
df$date <- as.Date(df$date)
df$date <- as.yearmon(df$date)


#adding distances
df$Gatun <- lake_distances$Gatun[match(df$site, lake_distances$site)]
df$Caño_Quebrado <- lake_distances$Caño_Quebrado[match(df$site, lake_distances$site)]
df$Trinidad <- lake_distances$Trinidad[match(df$site, lake_distances$site)]
df$Ciri_Grande <- lake_distances$Ciri_Grande[match(df$site, lake_distances$site)]
df$min_dist <- lake_distances$min_dist[match(df$site, lake_distances$site)]

##################### repeating monthly average of flow to compensate for not enough data
Gatun_mon_mean <- tapply(freshwater_monthly$Gatun, format(freshwater_monthly$date, "%m"), mean)
Ciri_Grande_mon_mean <- tapply(freshwater_monthly$Ciri_Grande, format(freshwater_monthly$date, "%m"), mean)
Trinidad_mon_mean <- tapply(freshwater_monthly$Trinidad, format(freshwater_monthly$date, "%m"), mean)
Caño_Quebrado_mon_mean <- tapply(freshwater_monthly$Cano_Quebrado, format(freshwater_monthly$date, "%m"), mean)

Gatun_mon_mean <- data.frame(mean = Gatun_mon_mean, mon = c(1:12))
Ciri_Grande_mon_mean <- data.frame(mean = Ciri_Grande_mon_mean, mon = c(1:12))
Trinidad_mon_mean <- data.frame(mean = Trinidad_mon_mean, mon = c(1:12))
Caño_Quebrado_mon_mean <- data.frame(mean = Caño_Quebrado_mon_mean, mon = c(1:12))


df$Ciri_Grande_flow <- Ciri_Grande_mon_mean$mean[match(as.numeric(format(df$date, "%m")), as.numeric(Ciri_Grande_mon_mean$mon))]
df$Trinidad_flow <- Trinidad_mon_mean$mean[match(as.numeric(format(df$date, "%m")), as.numeric(Trinidad_mon_mean$mon))]
df$Cano_Quebrado_flow <- Caño_Quebrado_mon_mean$mean[match(as.numeric(format(df$date, "%m")), as.numeric(Caño_Quebrado_mon_mean$mon))]
df$Gatun_flow <- Gatun_mon_mean$mean[match(as.numeric(format(df$date, "%m")), as.numeric(Gatun_mon_mean$mon))]


############### adding flow and traffic data
df$n <- traffic$n[match(df$date, traffic$date)]
df$neo_n <- traffic$neo_n[match(df$date, traffic$date)]
df$Gatun_flow_2 <- freshwater_monthly$Gatun[match(df$date, freshwater_monthly$date)]
df$Cano_Quebrado_flow_2 <- freshwater_monthly$Cano_Quebrado[match(df$date, freshwater_monthly$date)]
df$Trinidad_flow_2 <- freshwater_monthly$Trinidad[match(df$date, freshwater_monthly$date)]
df$Ciri_Grande_flow_2 <- freshwater_monthly$Ciri_Grande[match(df$date, freshwater_monthly$date)]


############## column that has data values when available and averaged data when NA

df$Gatun_flow_3 <- ifelse(is.na(df$Gatun_flow_2), df$Gatun_flow, df$Gatun_flow_2)
df$Cano_Quebrado_flow_3 <- ifelse(is.na(df$Cano_Quebrado_flow_2), df$Cano_Quebrado_flow, df$Cano_Quebrado_flow_2)
df$Trinidad_flow_3 <- ifelse(is.na(df$Trinidad_flow_2), df$Trinidad_flow, df$Trinidad_flow_2)
df$Ciri_Grande_flow_3 <- ifelse(is.na(df$Ciri_Grande_flow_2), df$Ciri_Grande_flow, df$Ciri_Grande_flow_2)



df <- subset(df, df$data != 15)
df <- subset(df, df$data != 4.11)





names(df)[19] <- 'Gatun_flow'
names(df)[20] <- 'Cano_Quebrado_flow'
names(df)[21]<- 'Trinidad_flow'
names(df)[22] <- 'Ciri_Grande_flow'


names(df)[4] <- 'Gatun_dist'
names(df)[5] <- 'Cano_Quebrado_dist'
names(df)[6] <- 'Trinidad_dist'
names(df)[7] <- 'Ciri_Grande_dist'



df$Gatun_flow_dist <- df$Gatun_flow/df$Gatun_dist
df$Cano_Quebrado_flow_dist <- df$Cano_Quebrado_flow/df$Cano_Quebrado_dist
df$Trinidad_flow_dist <- df$Trinidad_flow/df$Trinidad_dist
df$Ciri_Grande_flow_dist <- df$Ciri_Grande_flow/df$Ciri_Grande_dist
df$min_dist_traffic <- df$n/df$min_dist
df$min_dist_neo_traffic <-df$neo_n/df$min_dist


#################################################################################
# NOTE: When writing the code, I wrote a section to intall 0 in the neopanamax column for all dates before the lock opening, and NA for the dates after the
# opening of the lock for which there is no data. However, I have completely lost this section of code, which probably explains this
# piece of the code having a lower R squared of 59.74%. The complete dataset can be found in the all_data.csv file. Lesson learned to backup my code more often
# and not get lost on a thousand different files.
#################################################################################










