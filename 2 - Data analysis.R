
library(tidyverse)
library(lubridate)
library(zoo)
library('NLP')


#---------------
#Distances dataframe

Atlantic <- c(619605.94, 1024183.77)

Pacific <- c(656962.15, 990814.93)

Ciri_Grande <- c(605212.38, 996625.48)

Trinidad <- c(614365.08, 997086.71)

Cano_Quebrado <- c(628658.16, 999555.20)

Gatun <- c(633184.71, 1023553.01)

distances <- data.frame(stations$site)
distances['Gatun'] <- ((stations$UTM.X-Gatun[1])^2+(stations$UTM.Y-Gatun[2])^2)^(1/2)
distances['Caño_Quebrado'] <- ((stations$UTM.X-Cano_Quebrado[1])^2+(stations$UTM.Y-Cano_Quebrado[2])^2)^(1/2)
distances['Trinidad'] <- ((stations$UTM.X-Trinidad[1])^2+(stations$UTM.Y-Trinidad[2])^2)^(1/2)
distances['Ciri_Grande'] <- ((stations$UTM.X-Ciri_Grande[1])^2+(stations$UTM.Y-Ciri_Grande[2])^2)^(1/2)
distances['Atlantic'] <- ((stations$UTM.X-Atlantic[1])^2+(stations$UTM.Y-Atlantic[2])^2)^(1/2)
distances['Pacific'] <- ((stations$UTM.X-Pacific[1])^2+(stations$UTM.Y-Pacific[2])^2)^(1/2)

names(distances)[1] <- "site" 
distances <- distances[!is.na(distances$Gatun), ]

save(distances,file="distances.csv")




################################################################
##### Average salinity levels before and after neopanamax #####
################################################################


salinity_pre_neo <- subset(salinity, salinity$date < as.Date("2016-06-01") )

averages <- data.frame(unique(salinity_pre_neo$site))
names(averages)[1] <- "site"

count <- 1
for (station in unique(salinity_pre_neo$site)){
  station <- String(station)
  set <- subset(salinity_pre_neo, site==station)
  averages$average[count] <- mean(set$data)
  count <- count + 1
}

#averages <- averages[order(-averages$average),]
mean(averages$average)

####################################

salinity_post_neo <- subset(salinity, salinity$date > as.Date("2016-06-01") )

post_neo_averages <- data.frame(unique(salinity_post_neo$site))
names(post_neo_averages)[1] <- "site"

count <- 1
for (station in unique(salinity_post_neo$site)){
  station <- String(station)
  set <- subset(salinity_post_neo, site==station)
  post_neo_averages$average[count] <- mean(set$data)
  count <- count + 1
}

mean(post_neo_averages$average)


########################### show pre neo averages of stations that have pre and post neo data

salinity_pre_neo_same_stations <- data.frame(unique(salinity_post_neo$site))

count <- 1
for (station in unique(salinity_post_neo$site)){
  station <- String(station)
  set <- subset(salinity_pre_neo, site==station)
  salinity_pre_neo_same_stations$average[count] <- mean(set$data)
  count <- count + 1
}
salinity_pre_neo_same_stations

salinity_pre_neo_same_stations <- salinity_pre_neo_same_stations[!is.na(salinity_pre_neo_same_stations$average), ]

mean(salinity_pre_neo_same_stations$average)


######################################## select stations that have distance data

stations_with_distance <- data.frame(unique(distances$site))

count <- 1
for (station in unique(distances$site)){
  station <- String(station)
  set <- subset(salinity_pre_neo, site==station)
  set2 <- subset(salinity_post_neo, site==station)
  stations_with_distance$average[count] <- mean(set$data)
  stations_with_distance$post_neo_average[count] <- mean(set2$data)
  count <- count + 1
}

names(stations_with_distance)[1] <- "site"


distances <- cbind(distances, stations_with_distance$post_neo_average)
distances <- cbind(distances, stations_with_distance$average)
names(distances)[8] <- "post_neo_average"
names(distances)[9] <- "average"



lake_distances <- distances[distances$site %in% (lake), ]
lake_distances$min_dist <- apply(lake_distances, 1, function(x) min(x[6],x[7]))

lake_distances$min_dist <- as.numeric(lake_distances$min_dist)

#lake_distances$average shows the average salinity level per station BEFORE neopanamax
#lake_distances$post_neo_average shows the average salinity level per station AFTER neopanamax


plot(lake_distances$min_dist, lake_distances$average, col='lightblue', main="Pre-neopanamax",
     xlab="min distance", ylab="salinity", pch=19)

text(lake_distances$min_dist, lake_distances$average, labels=lake_distances$site, cex=0.9, font=2)

plot(lake_distances$min_dist, lake_distances$post_neo_average, col='lightblue', main="Post-NeoPanamax",
     xlab="min distance", ylab="salinity", pch=19)

text(lake_distances$min_dist, lake_distances$post_neo_average, labels=lake_distances$site, cex=0.9, font=2)

################################################################# SAME FOR ALAJUELA

Alajuela_distances <- distances[distances$site %in% (Alajuela), ]
Alajuela_distances$min_dist <- apply(Alajuela_distances, 1, function(x) min(x[6],x[7]))

plot(Alajuela_distances$min_dist, Alajuela_distances$average, col='lightblue', main="Pre-neopanamax ALA",
     xlab="min distance", ylab="salinity", pch=19)

text(Alajuela_distances$min_dist, Alajuela_distances$average, labels=Alajuela_distances$site, cex=0.9, font=2)

plot(Alajuela_distances$min_dist, Alajuela_distances$post_neo_average, col='lightblue', main="Post-NeoPanamax ALA",
     xlab="min distance", ylab="salinity", pch=19)

text(Alajuela_distances$min_dist, Alajuela_distances$post_neo_average, labels=Alajuela_distances$site, cex=0.9, font=2)

################################################################# SAME FOR RIVERS

Rivers_distances <- distances[distances$site %in% (Rivers), ]
Rivers_distances$min_dist <- apply(Rivers_distances, 1, function(x) min(x[6],x[7]))
names(Rivers_distances)[names(Rivers_distances) == "stations_with_distance$post_neo_average"] <- "post_neo_average"

plot(Rivers_distances$min_dist, Rivers_distances$average, col='lightblue', main="Pre-neopanamax Rivers",
     xlab="min distance", ylab="salinity", pch=19)

text(Rivers_distances$min_dist, Rivers_distances$average, labels=Rivers_distances$site, cex=0.9, font=2)

plot(Rivers_distances$min_dist, Rivers_distances$post_neo_average, col='lightblue', main="Post-NeoPanamax Rivers",
     xlab="min distance", ylab="salinity", pch=19)

text(Rivers_distances$min_dist, Rivers_distances$post_neo_average, labels=Rivers_distances$site, cex=0.9, font=2)



###############################################################

#plot trade volume over time

monthly_traffic_2018 <- read.csv("monthly_traffic_2018.csv")
monthly_traffic <- readRDS("monthly_traffic.rds")


monthly_traffic$neo_n <- 0
monthly_traffic_2018$n <- as.numeric(gsub(",","",monthly_traffic_2018$n))
monthly_traffic_2018 <- data.frame(monthly_traffic_2018[1:3], n.total = NA, n.frac = NA ,monthly_traffic_2018[4])

monthly_traffic_2018$n.total[1:12] <- sum(monthly_traffic_2018$n[1:12])

monthly_traffic_2018$n.total[13:24] <- sum(monthly_traffic_2018$n[13:24])

monthly_traffic_2018$n.total[25:36] <- sum(monthly_traffic_2018$n[25:36])

#assuming it's 15% for every month
for (i in 1:12){
  monthly_traffic_2018$neo_n[i] <- monthly_traffic_2018$n[i]*.2
}

for (i in 1:nrow(monthly_traffic_2018)){
  monthly_traffic_2018$n.frac[i] <- monthly_traffic_2018$n[i]/monthly_traffic_2018$n.total[i]
}

traffic <- rbind(monthly_traffic, monthly_traffic_2018)

traffic$neo.frac <- traffic$neo_n/traffic$n

plot(traffic$n)



traffic$date <- as.yearmon(paste(traffic$yr, traffic$mnth), "%Y %m")



##################################
# TRAFFIC
#################################


plot(df$date, df$n,  xaxt='n', ann=FALSE, type='l')
axis(1, at=seq(1, 144, length.out = 15), labels=c(2006:2020))

plot(df$date, df$n, cex = 0.5, xlim = c(as.Date('2006-01-22'), as.Date('2019-12-26')),
     ylim = c(550, 1250), main = "Total traffic",pch=16, xlab="Date", ylab="# of ships")
#axis(1, at=seq(1, 161, length.out = 14), labels=c(2006:2019))
lines(df$date[order(df$date)], df$n[order(df$date)], ylim = c(550, 1250), xlim = c(as.Date('2003-01-22'), as.Date('2019-12-26')))

plot(df$date[order(df$date)], df$neo_n[order(df$date)], cex = 0.5, main = "Neopanamax traffic",pch=16, xlab="Date", ylab="# of ships", ylim = c(100, 400), xlim = c(as.Date('2017-10-22'), as.Date('2019-12-26')))
lines(df$date[order(df$date)], df$neo_n[order(df$date)], ylim = c(100, 400), xlim = c(as.Date('2017-10-22'), as.Date('2019-12-26')))



#traffic$n does not include small commercial traffic


##################################################################
##### DELAY
#################################################################

#the relationship between trade and salinity changed for most stations.
#first regression for the lake: check out the delay.
#how long after the opening did each station spike?

lake_salinity_post_neo <- subset(lake_salinity, lake_salinity$date > as.Date("2016-06-26") )
lake_salinity_post_neo$delta <- ave(lake_salinity_post_neo$data, lake_salinity_post_neo$site, FUN=function(x) c(0, diff(x)))

delay <- data.frame(unique(lake_salinity_post_neo$site))
names(delay)[1] <- "site"

count <- 1
for (station in unique(lake_salinity_post_neo$site)){
  station <- String(station)
  lake_post_neo_station <- subset(lake_salinity_post_neo, lake_salinity_post_neo$site == station)
  dts <- as.POSIXct(lake_post_neo_station$date[which(lake_post_neo_station$delta > 0)[1]],format="%Y%j")
  dts <- format(dts,format="%Y-%m-%d")
  delay$first[count] <- dts
  count <- count + 1
}

delay$first <- as.Date(delay$first, format="%Y-%m-%d")
delay$delay <- delay$first-as.Date("2016-06-26")
delay <- delay[!is.na(delay$delay), ]

delay <- delay[order(delay$site),]

plot((lake_distances[lake_distances$site %in% delay$site,])$min_dist, delay$delay)
text((lake_distances[lake_distances$site %in% delay$site,])$min_dist, delay$delay, labels=delay$site, cex=0.9, font=2)


#delay information is useful when indexing; basically, making the 

write.csv(delay,"delay.csv", row.names = FALSE)

reg <- lm(delay$delay ~ delay$dist)
abline(reg, col = 'red')
summary(reg)



###########################################

freshwater <- readRDS('freshwater.RDS')

for (unique in unique(freshwater$station)){
  unique <- String(unique)
  set <- subset(freshwater, freshwater$station == unique)
  plot(set$q, ylab = 'flux', main = unique)
}


##################################
# El Niño
#################################

nino <- read.csv("nino.csv")

nino <- subset(nino, nino$YR >= 2003)

plot(nino$ANOM,  xaxt='n', ann=FALSE, type='l')
axis(1, at=seq(1, 216, length.out = 19), labels=c(2003:2021))
abline(h = 1, col='red')
abline(h = -1, col='red')
abline(v = 161, col='blue')

title(main="El niño",
      xlab="Year", ylab="Anomaly")
