#Predicting Salinity Diffusion Patterns in the Panama Canal
#Ana Avila, 2021 (ana.avila@mail.mcgill.ca)

setwd("/Users/anavitorino/Desktop/Salinity")


#----------------
#  Loading libraries
#----------------

library('NLP')
library('ggplot2')
library('tidyverse')
library(lubridate)


#----------------
#  Importing Dataset
#----------------

salinity <- read.csv('ACP_salinity.csv')
stations <- read.csv('stations.csv')
salinity$date <- as.Date(salinity$date, "%d/%m/%Y")
salinity <- salinity[salinity$data >= 0, ]
salinity <- salinity[-c(5)]
names(stations)[3] <- "site"

salinity <- salinity[!is.na(salinity$date),]


for(count in 1:nrow(salinity)){
  if(format(as.Date(salinity$date[count]), format="%Y") == 7018){
    year(salinity$date[count]) <- 2018
  }
}

for(count in 1:nrow(salinity)){
  if(format(as.Date(salinity$date[count]), format="%Y") == 7009){
    year(salinity$date[count]) <- 2009
  }
}

for(count in 1:nrow(salinity)){
  if(format(as.Date(salinity$date[count]), format="%Y") == 7008){
    year(salinity$date[count]) <- 2008
  }
}

for(count in 1:nrow(salinity)){
  if(format(as.Date(salinity$date[count]), format="%Y") %in% c(3009,2099) ){
    year(salinity$date[count]) <- 2009
  }
}


salinity_site <- unique(salinity$site)
stations_site <- unique(stations$site)

#Stations missing position data:
missing_position <- salinity_site[!(salinity_site %in% stations_site)]

#Stations with salinity and position data:
with_position <- salinity_site[(salinity_site %in% stations_site)]


x_coord <- stations$UTM.X
y_coord <- stations$UTM.Y

#Changing index of station sites that have been mistyped:
salinity$site[salinity$site == "RC0"] <- "RCO"
salinity$site[salinity$site == "DC!"] <- "DC1"
salinity$site[salinity$site == "B0P"] <- "BOP"


#NOTE 1: The only station with salinity data, but not present in "stations' csv file is GUA (Isla Guacha). The only information it has is 0.00-0.05 salinity for 9 months in 2010. I could add that information, but I am not sure if the station was not included in the file for some valid reason out of my knowledge, so I made no changes to the original file.

#NOTE 2: BDI, CNA, LMA dont connect to the canal
  #HJ3 = HU3
  
  

lake <- list('TMH', 'MLR', 'TAS', 'BAT', 'ESC', 'RAI', 'ARN', 'TAC', 'HUM', 'BCI', 'DC1', 'TMR', 'M12', 'RAP', 'M5', 'M12', 'M2', 'RCO', 'LAT', 'TAR', 'TME')

lake_salinity <- salinity[salinity$site %in% (lake), ]
lake_stations <- stations[stations$site %in% (lake), ]

Alajuela <- list('TM4', 'TM3', 'TM2', 'TM1', 'CH9', 'TAG', 'PNP', 'ERP', 'BOP', 'DCH')

Alajuela_salinity <- salinity[salinity$site %in% (Alajuela), ]
Alajuela_stations <- stations[stations$site %in% (Alajuela), ]


Rivers <- list('CAN', 'CHR', 'RCQ', 'QAL', 'QIG', 'QLG', 'CAQ', 'CQA', 'RCN', 'BR1', 'CNT', 'PEL', 'CDL', 'CHI', 'IGU')

Rivers_salinity <- salinity[salinity$site %in% (Rivers), ]
Rivers_stations <- stations[stations$site %in% (Rivers), ]





#---------------

Shallow <- subset(salinity, depth=='Shallow')
Deep <- subset(salinity, depth=='Deep')
Depth <- subset(salinity, depth=='Depth')
na <- subset(salinity, depth=='n/a')


na_stations <- unique(na$site)
Depth_stations <- unique(Depth$site)

na_with_position <- c()

for (station in with_position){
  new <- subset(na, site == station)
  na_with_position <- rbind(na_with_position, new)
}


Deep$delta <- ave(Deep$data, Deep$site, FUN=function(x) c(0, diff(x)))

Shallow$delta <- ave(Shallow$data, Shallow$site, FUN=function(x) c(0, diff(x)))

na_with_position$delta <- ave(na_with_position$data, na_with_position$site, FUN=function(x) c(0, diff(x)))


save(Shallow,file="Shallow.csv")
save(Deep,file="Deep.csv")
save(na_with_position,file="NA.csv")




#Some stations had NA dates. removed from the dataframe.



#----------------
#  Plotting
#----------------

#Time series of salinity measured in Deep category
plot_list_Deep <- list()
count = 1
for (station in unique(Deep$site)) {
  station <- String(station)
  set <- subset(Deep, site==station)
  set$date <- as.Date(set$date, "%d/%m/%Y")
  p <- ggplot(set, aes(date, data)) + geom_line() + scale_x_date(date_breaks="1 year", date_labels="%Y")+
    labs(x="Date of measurement", y = "Salinity") + ggtitle(station)
  plot_list_Deep[[count]] = p
  count = count + 1
}

pdf("All_Stations_Deep.pdf")
plot_list_Deep
dev.off()



#map of Shallow salinity
plot_list_Shallow = list()
count = 1
for (station in unique(Shallow$site)) {
  station <- String(station)
  print(station)
  set <- subset(Shallow, site==station)
  set$date <- as.Date(set$date, "%d/%m/%Y")
  print(class(set$date))
  p <- ggplot(set, aes(date, data)) + geom_line() + scale_x_date(date_breaks="1 year", date_labels="%Y")+labs(x="Date of measurement", y = "Salinity") + ggtitle(station)
  plot_list_Shallow[[count]] = p
  count = count + 1
}

pdf("All_Stations_Shallow.pdf")
plot_list_Shallow
dev.off()


#map of all stations na
plot_list_na = list()
count = 1
for (station in unique(na_with_position$site)) {
  station <- String(station)
  print(station)
  set <- subset(na, site==station)
  set$date <- as.Date(set$date, "%d/%m/%Y")
  print((set$date))
  if (!is.na(set$date)){
    p <- ggplot(set, aes(date, data)) + geom_line() + scale_x_date(date_breaks="1 year", date_labels="%Y")+labs(x="Date of measurement", y = "Salinity") + ggtitle(station)
    plot_list_na[[count]] = p
    count = count + 1
  }
}

pdf("All_Stations_na.pdf")
plot_list_na
dev.off()



#finding max values

for (station in unique(Shallow$site)) {
  station <- String(station)
  print(station)
  set <- subset(Shallow, site==station)
  print(max(set$data))
}

for (station in unique(Deep$site)) {
  station <- String(station)
  print(station)
  set <- subset(Deep, site==station)
  set <- set[order(set$site),]
  print(max(set$data))
}


for (station in unique(na_with_position$site)) {
  station <- String(station)
  print(station)
  set <- subset(na_with_position, site==station)
  set <- set[order(set$site),]
  print(max(set$data))
}


#-----------------------  getting the instant change in salinity with relation to previous month


################## DEEP



#delta salt dataframe - deep
time_seq_deep <- seq(as.Date("01/01/2003",format = "%d/%m/%Y"),length.out = 204,by = "month")
time_seq_deep <- strftime(time_seq_deep, "%m/%Y")

delta_salt_deep <- data.frame(unique(Deep$site))

for (unique in time_seq_deep){
  delta_salt_deep[toString(unique)] <- NA
}




plot_list_deep_delta = list()
count = 1
for (station in unique(Deep$site)) {
  station <- String(station)
  print(station)
  set <- subset(Deep, site==station)
  set$date <- as.Date(set$date, "%d/%m/%Y")
  if (!is.na(set$date)){
    print(set$delta)
    p <- ggplot(set, aes(date, delta)) + scale_x_date(date_breaks="1 year", date_labels="%Y") + geom_line() + labs(x="Date of measurement", y = "Salinity") + ggtitle(station)
    plot_list_deep_delta[[count]] = p
  }
  count = count + 1
}

pdf("deep_delta.pdf")
plot_list_deep_delta
dev.off()



################## SHALLOW


plot_list_Shallow_delta = list()
count = 1
for (station in unique(Shallow$site)) {
  station <- String(station)
  print(station)
  set <- subset(Shallow, site==station)
  set$date <- as.Date(set$date, "%d/%m/%Y")
  if (!is.na(set$date)){
    print(set$delta)
    p <- ggplot(set, aes(date, delta)) + scale_x_date(date_breaks="1 year", date_labels="%Y") + geom_line() + labs(x="Date of measurement", y = "Salinity") + ggtitle(station)
    plot_list_Shallow_delta[[count]] = p
  }
  count = count + 1
}

pdf("shallow_delta.pdf")
plot_list_Shallow_delta
dev.off()


################## NA


plot_list_na_with_position_delta = list()
count = 1
for (station in unique(na_with_position$site)) {
  station <- String(station)
  print(station)
  set <- subset(na_with_position, site==station)
  set$date <- as.Date(set$date, "%d/%m/%Y")
  if (!is.na(set$date)){
    print(set$delta)
    p <- ggplot(set, aes(date, delta)) + scale_x_date(date_breaks="1 year", date_labels="%Y") + geom_line() + labs(x="Date of measurement", y = "Salinity") + ggtitle(station)
    plot_list_na_with_position_delta[[count]] = p
  }
  count = count + 1
}

pdf("na_delta.pdf")
plot_list_na_with_position_delta
dev.off()


