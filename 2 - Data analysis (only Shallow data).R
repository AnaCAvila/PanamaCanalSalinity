
################################################################
##### Average Shallow levels before and after neopanamax #####
################################################################
library('NLP')


Shallow_pre_neo <- subset(Shallow, Shallow$date < as.Date("2016-06-01") )

averages <- data.frame(unique(Shallow_pre_neo$site))
names(averages)[1] <- "site"

count <- 1
for (station in unique(Shallow_pre_neo$site)){
  station <- String(station)
  set <- subset(Shallow_pre_neo, site==station)
  averages$average[count] <- mean(set$data)
  count <- count + 1
}

#averages <- averages[order(-averages$average),]
mean(averages$average)

####################################

Shallow_post_neo <- subset(Shallow, Shallow$date > as.Date("2016-06-01") )

post_neo_averages <- data.frame(unique(Shallow_post_neo$site))
names(post_neo_averages)[1] <- "site"

count <- 1
for (station in unique(Shallow_post_neo$site)){
  station <- String(station)
  set <- subset(Shallow_post_neo, site==station)
  post_neo_averages$average[count] <- mean(set$data)
  count <- count + 1
}

mean(post_neo_averages$average)


########################### show pre neo averages of stations that have pre and post neo data

Shallow_pre_neo_same_stations <- data.frame(unique(Shallow_post_neo$site))

count <- 1
for (station in unique(Shallow_post_neo$site)){
  station <- String(station)
  set <- subset(Shallow_pre_neo, site==station)
  Shallow_pre_neo_same_stations$average[count] <- mean(set$data)
  count <- count + 1
}
Shallow_pre_neo_same_stations

Shallow_pre_neo_same_stations <- Shallow_pre_neo_same_stations[!is.na(Shallow_pre_neo_same_stations$average), ]

mean(Shallow_pre_neo_same_stations$average)


######################################## select stations that have distance data

stations_with_distance <- data.frame(unique(distances$site))

count <- 1
for (station in unique(distances$site)){
  station <- String(station)
  set <- subset(Shallow_pre_neo, site==station)
  set2 <- subset(Shallow_post_neo, site==station)
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

plot(lake_distances$min_dist, lake_distances$average, col='lightblue', main="Pre-neopanamax",
     xlab="min distance", ylab="Shallow", pch=19)

text(lake_distances$min_dist, lake_distances$average, labels=lake_distances$site, cex=0.9, font=2)

plot(lake_distances$min_dist, lake_distances$post_neo_average, col='lightblue', main="Post-NeoPanamax",
     xlab="min distance", ylab="Shallow", pch=19)

text(lake_distances$min_dist, lake_distances$post_neo_average, labels=lake_distances$site, cex=0.9, font=2)

################################################################# SAME FOR ALAJUELA

Alajuela_distances <- distances[distances$site %in% (Alajuela), ]
Alajuela_distances$min_dist <- apply(Alajuela_distances, 1, function(x) min(x[6],x[7]))

plot(Alajuela_distances$min_dist, Alajuela_distances$average, col='lightblue', main="Pre-neopanamax ALA",
     xlab="min distance", ylab="Shallow", pch=19)

text(Alajuela_distances$min_dist, Alajuela_distances$average, labels=Alajuela_distances$site, cex=0.9, font=2)

plot(Alajuela_distances$min_dist, Alajuela_distances$post_neo_average, col='lightblue', main="Post-NeoPanamax ALA",
     xlab="min distance", ylab="Shallow", pch=19)

text(Alajuela_distances$min_dist, Alajuela_distances$post_neo_average, labels=Alajuela_distances$site, cex=0.9, font=2)

################################################################# SAME FOR RIVERS

Rivers_distances <- distances[distances$site %in% (Rivers), ]
Rivers_distances$min_dist <- apply(Rivers_distances, 1, function(x) min(x[6],x[7]))
names(Rivers_distances)[names(Rivers_distances) == "stations_with_distance$post_neo_average"] <- "post_neo_average"

plot(Rivers_distances$min_dist, Rivers_distances$average, col='lightblue', main="Pre-neopanamax Rivers",
     xlab="min distance", ylab="Shallow", pch=19)

text(Rivers_distances$min_dist, Rivers_distances$average, labels=Rivers_distances$site, cex=0.9, font=2)

plot(Rivers_distances$min_dist, Rivers_distances$post_neo_average, col='lightblue', main="Post-NeoPanamax Rivers",
     xlab="min distance", ylab="Shallow", pch=19)

text(Rivers_distances$min_dist, Rivers_distances$post_neo_average, labels=Rivers_distances$site, cex=0.9, font=2)


##################################################################
##### DELAY
#################################################################

lake_Shallow <- Shallow[Shallow$site %in% (lake), ]


lake_Shallow_post_neo <- subset(lake_Shallow, lake_Shallow$date > as.Date("2016-06-26") )
lake_Shallow_post_neo$delta <- ave(lake_Shallow_post_neo$data, lake_Shallow_post_neo$site, FUN=function(x) c(0, diff(x)))

delay <- data.frame(unique(lake_Shallow_post_neo$site))
names(delay)[1] <- "site"

count <- 1
for (station in unique(lake_Shallow_post_neo$site)){
  station <- String(station)
  lake_post_neo_station <- subset(lake_Shallow_post_neo, lake_Shallow_post_neo$site == station)
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








