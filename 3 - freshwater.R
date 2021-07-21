
################################# Assigning proper names to freshwater stations

freshwater <- readRDS('freshwater.RDS')


for (value in 1:nrow(freshwater)){
  if (freshwater$station[value] == 21){
    freshwater$station[value] <- "Ciri_Grande"
  }
  if (freshwater$station[value] == 48){
    freshwater$station[value] <- "Trinidad"
  }
  if (freshwater$station[value] == 52){
    freshwater$station[value] <- "Gatun"
  }
  if (freshwater$station[value] == 59){
    freshwater$station[value] <- "Caño_Quebrado"
  }
}




Gatun <- subset(freshwater, freshwater$station == "Gatun")
Ciri_Grande <- subset(freshwater, freshwater$station == "Ciri_Grande")
Trinidad <- subset(freshwater, freshwater$station == "Trinidad")
Caño_Quebrado <- subset(freshwater, freshwater$station == "Caño_Quebrado")

plot(Gatun$q, main = "Gatun")
plot(Ciri_Grande$q, main = "Ciri_Grande")
plot(Trinidad$q, main = "Trinidad")
plot(Caño_Quebrado$q, main = "Caño_Quebrado")


Gatun$date <- as.yearmon(with(Gatun, paste(year, mon, sep="-")), "%Y-%m")
Ciri_Grande$date <- as.yearmon(with(Ciri_Grande, paste(year, mon, sep="-")), "%Y-%m")
Trinidad$date <- as.yearmon(with(Trinidad, paste(year, mon, sep="-")), "%Y-%m")
Caño_Quebrado$date <- as.yearmon(with(Caño_Quebrado, paste(year, mon, sep="-")), "%Y-%m")


freshwater_monthly <- aggregate(Gatun[, 5], list(Gatun$date), mean)
freshwater_monthly$Ciri_Grande <- aggregate(Ciri_Grande[, 5], list(Ciri_Grande$date), mean)[,2]
freshwater_monthly$Trinidad <- aggregate(Trinidad[, 5], list(Trinidad$date), mean)[,2]
freshwater_monthly$Cano_Quebrado <- aggregate(Caño_Quebrado[, 5], list(Caño_Quebrado$date), mean)[,2]
names(freshwater_monthly)[1] <- "date" 
names(freshwater_monthly)[2] <- "Gatun" 
